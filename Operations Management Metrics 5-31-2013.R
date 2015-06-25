#######################################
### RTY and Moving Range Limits     ###
#######################################
###################################################################
### Author: Matt Beckman                                        ###
### Date Created: 06/25/2012                                    ###
### Last Modified: 05/31/2013                                   ###
### Reason:                                                     ###
###   (07/03/2012) Automatically update phase I data set when   ###
###   a new week of data is available.                          ###
###                                                             ###
###   (06/29/2012) Include rolling weekly phase I with rolling  ###
###   7-day average in phase II.                                ###
###                                                             ###
###   (08/07/2012) Added indicator for last 7 days.             ###
###                                                             ###
###   (08/08/2012) Added MR chart limits                        ###
###   (08/29/2012) 1- Accommodates new date input column        ###
###   2- 0% RTY always flagged as OOC                           ###
###   3- MR OOC suppressed if coincides with increase in RTY    ###
###   4- Added Median Value to Step Yield Table                 ###
###                                                             ###
###   (05/31/2013) Added Median yield to summary table          ###
###################################################################

ptm <- proc.time() 


####################################
### Load and Prepare Data Tables ###
####################################

## load data table and initialize variables ##

setwd("h:/_Project and gCAPA Support/2012/Tiered Management Metrics (Brian Doe)")

# [1] "FACILITY"            "Grouping"            "LOTID"               "STEP_NAME"           "MFGDATE"             "GOOD"               
# [7] "SCRAP"               "REWORK"              "PRB"                 "FIRST_FISCAL_DAY_WK" "Attempts"            "AttemptsValue"  
raw.dat <- read.csv("DailyFPY v6.csv", header = TRUE) # Pump tube data
# raw.dat <- read.csv("FRY Test Data.csv", header = TRUE) # larger test data (~11MB)


# store month and year of today's date #
raw.dat$MFGWEEK <- as.POSIXct(format(strptime(raw.dat$ReportingWeek_S, format = "%m/%d/%Y")))
today <- max(raw.dat$MFGWEEK) # identify rolling 7 day period

dat7day <- subset(x = raw.dat, subset = (raw.dat$MFGWEEK == today))

# exclude the rolling 7 day data from the weekly data 
datweekly <- subset(x = raw.dat, subset = (as.Date(raw.dat$MFGWEEK) < as.Date(today) - 14))

# set MFGWEEK to yesterday's date in dat7day -- for charting purposes
# dat7day$MFGWEEK <- today-1

# merge weekly data and 7 day rolling data into raw.dat2
raw.dat2 <- rbind(datweekly, dat7day)


## For each Week and Step, calculate sum(good) and Sum(attempts)
GOOD <- aggregate(raw.dat2$GOOD, by = list(raw.dat2$FACILITY, raw.dat2$Grouping, raw.dat2$STEP_NAME, raw.dat2$MFGWEEK), FUN = sum)
ATTEMPTS <- aggregate(raw.dat2$Attempts, by = list(raw.dat2$FACILITY, raw.dat2$Grouping, raw.dat2$STEP_NAME, raw.dat2$MFGWEEK), FUN = sum)
VALUE <- aggregate(raw.dat2$AttemptsValue, by = list(raw.dat2$FACILITY, raw.dat2$Grouping, raw.dat2$STEP_NAME, raw.dat2$MFGWEEK), FUN = sum)


## merge aggregated data; rename columns ##
temp <- merge(x = GOOD, y = ATTEMPTS, by = c("Group.1", "Group.2", "Group.3", "Group.4"))
Yield <- merge(x = temp, y = VALUE, by = c("Group.1", "Group.2", "Group.3", "Group.4"))
colnames(Yield) = c("FACILITY", "GROUPING", "STEP.NAME", "MFGWEEK", "GOOD", "ATTEMPTS", "VALUE") 

## Prepare numeric Date column for sorting purposes ##
# Yield$MFGWEEK <- as.POSIXct(format(strptime(Yield$MFGWEEK, format = "%m/%d/%Y")))

## Sort data frame by step name and mfg date ##
Yield <- Yield[order(Yield$FACILITY, Yield$GROUPING, Yield$MFGWEEK), ]


####################
### Calculations ###
####################

## Calculate First Pass Yield ##
Yield$FPY <- Yield$GOOD / Yield$ATTEMPTS

## RTY, Attempts, Good per week
RTY.group <- aggregate(Yield$FPY, by = list(Yield$FACILITY, Yield$GROUPING, Yield$MFGWEEK), FUN = prod)
colnames(RTY.group) = c("FACILITY", "GROUPING", "MFGWEEK", "RTY")
ATTEMPTS.group <- aggregate(Yield$ATTEMPTS, by = list(Yield$FACILITY, Yield$GROUPING, Yield$MFGWEEK), FUN = median)
colnames(ATTEMPTS.group) = c("FACILITY", "GROUPING", "MFGWEEK", "Med.ATTEMPTS")
VALUE.group <- aggregate(Yield$VALUE, by = list(Yield$FACILITY, Yield$GROUPING, Yield$MFGWEEK), FUN = median)
colnames(VALUE.group) = c("FACILITY", "GROUPING", "MFGWEEK", "Med.VALUE")

temp2 <- merge(x = RTY.group, y = ATTEMPTS.group, by = c("FACILITY", "GROUPING", "MFGWEEK"))
Yield.group <- merge(x = temp2, y = VALUE.group, by = c("FACILITY", "GROUPING", "MFGWEEK"))

temp3 <- merge(x = Yield, y = ATTEMPTS.group, by = c("FACILITY", "GROUPING", "MFGWEEK"))   
Yield.step <- merge(x = temp3, y = VALUE.group, by = c("FACILITY", "GROUPING", "MFGWEEK"))  


## Calculate column for moving ranges ##
M <- length(Yield.group[,1])

Yield.group$RTY.iter <- c(NA, Yield.group$RTY[1:(M-1)])
Yield.group$MR <- abs(Yield.group$RTY - Yield.group$RTY.iter)


## Remove first MR value for each step ##
Yield.group$MR[!duplicated(Yield.group[ , c("FACILITY", "GROUPING")])] <- NA

## for each group, calculate the RMS error, RTY mean, RTY AMR, UCL, & LCL ##

# overall mean, median, sd, & AMR
group.mean <- aggregate(Yield.group$RTY, by = list(Yield.group$FACILITY, Yield.group$GROUPING), FUN = mean, na.rm = TRUE)
group.median <- aggregate(Yield.group$RTY, by = list(Yield.group$FACILITY, Yield.group$GROUPING), FUN = median, na.rm = TRUE)
group.temp <- merge(x = group.mean, y = group.median, by = c("Group.1", "Group.2"))

group.sd <- aggregate(Yield.group$RTY, by = list(Yield.group$FACILITY, Yield.group$GROUPING), FUN = sd, na.rm = TRUE)
group.temp2 <- merge(x = group.temp, y = group.sd, by = c("Group.1", "Group.2"))

group.amr <- aggregate(Yield.group$MR, by = list(Yield.group$FACILITY, Yield.group$GROUPING), FUN = mean, na.rm = TRUE)
group.summary <- merge(x = group.temp2, y = group.amr, by = c("Group.1", "Group.2"))
colnames(group.summary) = c("FACILITY", "GROUPING", "RTY.MEAN", "RTY.MEDIAN", "RTY.SD", "RTY.AMR")

# Stability Index (Cpk / Ppk; overall_sd / within_sd)
group.summary$Stability <- group.summary$RTY.SD / (group.summary$RTY.AMR / 1.128)


## Calculate control limits based on specified Phase I period ##

# Phase I mean, sd, & amr do not appear in final output table
# they impact control limit calculations ONLY.


phase1.b <- "2012-02-25"  # temporary input
phase1.e <- "2012-07-02"  # temporary input
# phase1.b <- NULL  # temporary input
# phase1.e <- NULL  # temporary input
# phase1.beg <- as.POSIXct(phase1.b)
# phase1.end <- as.POSIXct(phase1.e)


# end of phase 1; used for control limit calculations
if(is.null(phase1.e)) {
  phase1.end <- as.POSIXct(today-30)
  } else {
    phase1.end <- as.POSIXct(phase1.e)
    }

# beginning of phase 1; used for control limit calculations
if(is.null(phase1.b)) {
  phase1.beg <- as.POSIXct(as.Date(phase1.end)-183)
  } else {
    phase1.beg <- as.POSIXct(phase1.b)
    }


phase1.dat <- subset(Yield.group, subset = (phase1.beg <= Yield.group$MFGWEEK & Yield.group$MFGWEEK <= phase1.end))

group.mean.CL <- aggregate(phase1.dat$RTY, by = list(phase1.dat$FACILITY, phase1.dat$GROUPING), FUN = mean, na.rm = TRUE)
group.amr.CL <- aggregate(phase1.dat$MR, by = list(phase1.dat$FACILITY, phase1.dat$GROUPING), FUN = mean, na.rm = TRUE)

group.summary.CL <- merge(x = group.mean.CL, y = group.amr.CL, by = c("Group.1", "Group.2"))
colnames(group.summary.CL) = c("FACILITY", "GROUPING", "RTY.PLOT.MEAN", "RTY.PLOT.AMR")

# RTY UCL
group.summary.CL$RTY.UCL <- group.summary.CL$RTY.PLOT.MEAN + 3*(group.summary.CL$RTY.PLOT.AMR / 1.128) 
group.summary.CL$RTY.UCL[group.summary.CL$RTY.UCL > 1] <- 1

# RTY LCL
group.summary.CL$RTY.LCL <- group.summary.CL$RTY.PLOT.MEAN - 3*(group.summary.CL$RTY.PLOT.AMR / 1.128)
group.summary.CL$RTY.LCL[group.summary.CL$RTY.LCL < 0] <- 0


# MR UCL
group.summary.CL$MR.UCL <- group.summary.CL$RTY.PLOT.AMR + 3 * 0.8525 * (group.summary.CL$RTY.PLOT.AMR / 1.128) # 0.8525 = d3; 1.128 = d2; Wheeler & Chambers (1992)
group.summary.CL$MR.UCL[group.summary.CL$MR.UCL > 1] <- 1

# MR LCL
group.summary.CL$MR.LCL <- group.summary.CL$RTY.PLOT.AMR - 3 * 0.8525 * (group.summary.CL$RTY.PLOT.AMR / 1.128) # 0.8525 = d3; 1.128 = d2; Wheeler & Chambers (1992)
group.summary.CL$MR.LCL[group.summary.CL$MR.LCL < 0] <- 0


## Apply group calculations to main data table ##
Yield.group <- merge(Yield.group, group.summary.CL, by = c("FACILITY", "GROUPING"), all.x = TRUE)


## Indicator for Detection of Out of Control (OOC) Data ##
M <- length(Yield.group[,1])
Yield.group$RTY.OOC <- rep(FALSE, M)

Yield.group$RTY.OOC[Yield.group$RTY > Yield.group$RTY.UCL] <- TRUE
Yield.group$RTY.OOC[Yield.group$RTY < Yield.group$RTY.LCL] <- TRUE
Yield.group$RTY.OOC[Yield.group$RTY == 0] <- TRUE # flags RTY = 0 even if LCL = 0  


Yield.group$MR.OOC <- rep(FALSE, M)

Yield.group$MR.OOC[Yield.group$MR > Yield.group$MR.UCL] <- TRUE
Yield.group$MR.OOC[Yield.group$MR < Yield.group$MR.LCL] <- TRUE

## Suppress MR signals when RTY increases
Yield.group$MR.OOC[Yield.group$RTY > Yield.group$RTY.iter] <- FALSE

# Remove FPY.iter variable
Yield.group$RTY.iter <- NULL

## Indicator for baseline time period
Yield.group$Baseline <- rep("Excluded", M)
Yield.group$Baseline[phase1.beg <= Yield.group$MFGWEEK & Yield.group$MFGWEEK <= phase1.end] <- "Included"


## Indicator for Last 7 Days (WORKING IN S+; NOT WORKING IN R)
Yield.group$Last7Days[Yield.group$MFGWEEK != (today)] <- FALSE
Yield.group$Last7Days[Yield.group$MFGWEEK == (today)] <- TRUE

proc.time() - ptm


## Resulting Tables ##

head(Yield.step) 
# head(step.summary)
head(Yield.group)
head(group.summary)


################
### Plotting ###
################

# foo <- subset(x = Yield.group, subset = (Yield.group$FACILITY == "FWSLK" & Yield.group$GROUPING == "PumpTube")) 
# foo$MFGWEEK <- as.Date(foo$MFGWEEK)
# 
# # set up the plot 
# # pdf("temp3.pdf")
# par(mai = c(1.6, 1.2, 1.2, 0.5))
# plot(range(foo$MFGWEEK), range(c(-0.02, 1.02)), xlab = " ",
#      ylab="RTY", type = "n", axes = FALSE, frame.plot = TRUE)
# axis(side = 2, at = seq(0, 1.0, by = 0.1), labels = seq(0, 1.0, by = 0.1), las = 2)
# axis(side = 2, at = seq(0, 1.0, by = 0.05), labels = FALSE)
# axis(1, at = foo$MFGWEEK, labels = foo$MFGWEEK, las = 2)
# lines(foo$MFGWEEK, foo$RTY) 
# lines(foo$MFGWEEK, foo$RTY.UCL, col = "red") 
# lines(foo$MFGWEEK, foo$RTY.LCL, col = "red") 
# lines(foo$MFGWEEK, foo$RTY.PLOT.MEAN, col = "green") 
# points(foo$MFGWEEK, foo$RTY, pch = 20)
# points(foo$MFGWEEK[foo$RTY.OOC == TRUE], foo$RTY[foo$RTY.OOC == TRUE], col = "red", pch = 15)
# title("Rolled Throughput Yield (RTY)")
# # dev.off()
# 
# # pdf("temp4.pdf")
# par(mai = c(1.6, 1.2, 1.2, 0.5))
# plot(range(foo$MFGWEEK), range(c(-0.02, 1.02)), xlab = " ",
#      ylab="RTY", type = "n", axes = FALSE, frame.plot = TRUE)
# axis(side = 2, at = seq(0, 1.0, by = 0.1), labels = seq(0, 1.0, by = 0.1), las = 2)
# axis(side = 2, at = seq(0, 1.0, by = 0.05), labels = FALSE)
# axis(1, at = foo$MFGWEEK, labels = foo$MFGWEEK, las = 2)
# lines(foo$MFGWEEK, foo$MR) 
# lines(foo$MFGWEEK, foo$MR.UCL, col = "red") 
# lines(foo$MFGWEEK, foo$MR.LCL, col = "red") 
# points(foo$MFGWEEK, foo$MR, pch = 20)
# points(foo$MFGWEEK[foo$MR.OOC == TRUE], foo$MR[foo$MR.OOC == TRUE], col = "red", pch = 15)
# title("Rolled Throughput Yield (RTY) Moving Range")
# # dev.off()

# write.csv(Yield.group, "verification.csv") # write file for verification against MTB or JMP

# rm(list = ls())  # remove all variables