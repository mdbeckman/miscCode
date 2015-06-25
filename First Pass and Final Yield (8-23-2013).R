# For each Day and Step, calculate sum(good)/Sum(attempts) = FPY
# Then MovingRange, Avg moving range for STEP_NAME and control limit for step.
# 
# Output table Day/Step/FPY/MR/AvgMR/CL

#######################################
### FPY Statistical Process Control ###
#######################################
#####################################################
### Author: Matt Beckman                          ###
### Date Created: 29-FEB-2012                     ###
### Last Modified: 09-SEP-2013                    ###
### Reason:                                       ###
### 15-JAN-2013: script modified to resolve divide###
###  by zero in FPY calc when there have been     ###
###	no attempts for a step on a given date	      ###
### 24-JAN-2013: script modified to augment step  ###
###	summary table to include sd & stability       ###
### 12-APR-2013: script modified to resolve divide###
###   by zero in stability calc when sd is zero   ###
### 23-AUG-2013: script modified to remove one    ###
###   iteration of OOC data from control limit    ###
###   calculations.                               ###
###                                               ###
#####################################################



####################################
### Load and Prepare Data Tables ###
####################################

## load data table for First Pass and Final Yield ##
getwd()
setwd("H:/_Project and gCAPA Support/2012/First Pass Yield (Doe)")
raw.dat <- read.csv("FPY Live Raw Big.csv", header = TRUE)
# raw.dat <- read.csv("FPY Live Raw Bigger.csv", header = TRUE)


ptm <- proc.time()

## For each Day and Step, calculate sum(good) and Sum(attempts)
GOOD.sum <- aggregate(raw.dat$GOOD, by = list(raw.dat$MODEL, raw.dat$STEP_NAME, raw.dat$S_MFGDATE), FUN = sum, na.rm = TRUE)
ATTEMPTS.sum <- aggregate(raw.dat$ATTEMPTS, by = list(raw.dat$MODEL, raw.dat$STEP_NAME, raw.dat$S_MFGDATE), FUN = sum, na.rm = TRUE)

## merge aggregated data; rename columns ##
Yield.dat <- merge(x = GOOD.sum, y = ATTEMPTS.sum, by = c("Group.1", "Group.2", "Group.3"))
colnames(Yield.dat) = c("PRODUCT.GROUP", "STEP.NAME", "MFGDATE", "GOOD", "ATTEMPTS")

## Prepare numeric Date column for sorting purposes ##
# Yield.dat$MFGDATE <- as.POSIXct(format(strptime(Yield.dat$MFGDATE, format = "%m/%d/%Y")))

## Sort data frame by step name and mfg date ##
Yield.dat <- Yield.dat[order(Yield.dat$PRODUCT.GROUP, Yield.dat$STEP.NAME, Yield.dat$MFGDATE),]


####################
### Calculations ###
####################

## Calculate First Pass Yield ##
Yield.dat$FPY <- Yield.dat$GOOD / Yield.dat$ATTEMPTS
Yield.dat <- Yield.dat[!is.nan(Yield.dat$FPY), ] # remove NaN rows resulting from zero attempts 

## Calculate column for moving ranges ##

N <- length(Yield.dat[,1])

Yield.dat$FPY.iter <- c(NA, Yield.dat$FPY[1:(N-1)])
Yield.dat$MR <- abs(Yield.dat$FPY - Yield.dat$FPY.iter)

Yield.dat <- Yield.dat[,-which(colnames(Yield.dat) == "FPY.iter")]

## Remove first MR value for each step ##
Yield.dat$MR[!duplicated(Yield.dat$STEP.NAME) | !duplicated(Yield.dat$PRODUCT.GROUP)] <- NA



## for each Step, calculate the FRY mean, FRY AMR, UCL, & LCL ##

# mean & AMR
step.mean <- aggregate(Yield.dat$FPY, by = list(Yield.dat$PRODUCT.GROUP, Yield.dat$STEP.NAME), FUN = mean, na.rm = TRUE)
step.sd <- aggregate(Yield.dat$FPY, by = list(Yield.dat$PRODUCT.GROUP, Yield.dat$STEP.NAME), FUN = sd, na.rm = TRUE)
step.temp <- merge(x = step.mean, y = step.sd, by = c("Group.1", "Group.2"))

step.amr <- aggregate(Yield.dat$MR, by = list(Yield.dat$PRODUCT.GROUP, Yield.dat$STEP.NAME), FUN = mean, na.rm = TRUE)
step.summary <- merge(x = step.temp, y = step.amr, by = c("Group.1", "Group.2"))
colnames(step.summary) = c("PRODUCT.GROUP", "STEP.NAME", "FRY.MEAN", "FRY.SD", "FRY.AMR")

step.summary$Stability <- step.summary$FRY.SD / (step.summary$FRY.AMR / 1.128)
step.summary$Stability[is.nan(step.summary$Stability)] <- 1 

#UCL
step.summary$UCL <- step.summary$FRY.MEAN + 3*(step.summary$FRY.AMR / 1.128)
ucl.conflict <- which(step.summary$UCL > 1)
step.summary$UCL[ucl.conflict] <- 1

#LCL
step.summary$LCL <- step.summary$FRY.MEAN - 3*(step.summary$FRY.AMR / 1.128)
lcl.conflict <- which(step.summary$LCL < 0)
step.summary$LCL[lcl.conflict] <- 0


## Apply step calculations to main data table ##
Yield.dat <- merge(Yield.dat, step.summary, by = c("PRODUCT.GROUP", "STEP.NAME"), all.x = TRUE)


## Indicator for Detection of Out of Control (OOC) Data ##
N <- length(Yield.dat[,1])

Yield.dat$OOC <- rep(FALSE, N)

ucl.ooc <- which(Yield.dat$FPY > Yield.dat$UCL)
Yield.dat$OOC[ucl.ooc] <- TRUE

lcl.ooc <- which(Yield.dat$FPY < Yield.dat$LCL)
Yield.dat$OOC[lcl.ooc] <- TRUE


##################################################################
### REMOVE OOC DATA FROM FIRST ITERATION & RECALCULATE LIMITS ####
##################################################################


# remove OOC data
Yield.IC <- Yield.dat[-which(Yield.dat$OOC == TRUE), ]


# mean & AMR
step.mean.IC <- aggregate(Yield.IC$FPY, by = list(Yield.IC$PRODUCT.GROUP, Yield.IC$STEP.NAME), FUN = mean, na.rm = TRUE)
step.sd.IC <- aggregate(Yield.IC$FPY, by = list(Yield.IC$PRODUCT.GROUP, Yield.IC$STEP.NAME), FUN = sd, na.rm = TRUE)
step.temp.IC <- merge(x = step.mean, y = step.sd, by = c("Group.1", "Group.2"))

step.amr.IC <- aggregate(Yield.IC$MR, by = list(Yield.IC$PRODUCT.GROUP, Yield.IC$STEP.NAME), FUN = mean, na.rm = TRUE)
step.summary.IC <- merge(x = step.temp.IC, y = step.amr.IC, by = c("Group.1", "Group.2"))
colnames(step.summary.IC) = c("PRODUCT.GROUP", "STEP.NAME", "FRY.MEAN.IC", "FRY.SD.IC", "FRY.AMR.IC")


#UCL
step.summary.IC$UCL.IC <- step.summary.IC$FRY.MEAN.IC + 3*(step.summary.IC$FRY.AMR.IC / 1.128)
ucl.conflict.IC <- which(step.summary.IC$UCL.IC > 1)
step.summary.IC$UCL.IC[ucl.conflict.IC] <- 1
step.summary$UCL.IC <- step.summary.IC$UCL.IC # add UCL.IC to step summary table (for spotfire results)

#LCL
step.summary.IC$LCL.IC <- step.summary.IC$FRY.MEAN.IC - 3*(step.summary.IC$FRY.AMR.IC / 1.128)
lcl.conflict.IC <- which(step.summary.IC$LCL.IC < 0)
step.summary.IC$LCL.IC[lcl.conflict.IC] <- 0
step.summary$LCL.IC <- step.summary.IC$LCL.IC # add LCL.IC to step summary table (for spotfire results)


## Apply step calculations to main data table ##
Yield.dat <- merge(Yield.dat, step.summary.IC, by = c("PRODUCT.GROUP", "STEP.NAME"), all.x = TRUE)   


### Indicator for Detection of 2nd Generation Out of Control (OOC) Data ####
N <- length(Yield.dat[,1])

Yield.dat$OOC_Gen2 <- rep(FALSE, N)

ucl.ooc2 <- which(Yield.dat$FPY > Yield.dat$UCL.IC)
Yield.dat$OOC_Gen2[ucl.ooc2] <- TRUE

lcl.ooc2 <- which(Yield.dat$FPY < Yield.dat$LCL.IC)
Yield.dat$OOC_Gen2[lcl.ooc2] <- TRUE




# Yield.dat
# step.summary

proc.time() - ptm