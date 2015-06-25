###########################################
### Numerical Threshold Pareto Trending ###
###########################################
#####################################################
### Author: Matt Beckman                          ###
### Date Created: 05/13/2013                      ###
### Last Modified:                                ###
#####################################################

rm(list = ls())

# Working Directory
setwd("H:/_Project and gCAPA Support/2013/Numerical Threshold (Jared Hanson)")
getwd()

# Load function to compute thresholds using Poisson Likelihood Ratio Test (LRT) #
source("PoissonLRTcomputethreshold.R")

ptm <- proc.time()


###############
### INPUTS ####
###############

# data table -- required fields are: Cat.Grp.PLI, Cat.Grp, Event.or.Notify.Dt (M/D/YYYY)
raw.dat <- read.csv("201306 Numerical Threshold Data.csv", header = TRUE)
raw.dat$Date <- as.POSIXct(format(strptime(raw.dat$EventOrNotifyDt, format = "%m/%d/%Y")))
raw.dat$index <- raw.dat$Cat.Grp.PLI
raw.dat$Grouping <- raw.dat$Cat.Grp
raw.dat <- raw.dat[!duplicated(raw.dat$index), ]

#########################################
### Specify time period cut-off dates ###
#########################################
# Q1: MAY, JUN, JUL #
# Q2: AUG, SEP, OCT #
# Q3: NOV, DEC, JAN #
# Q4: FEB, MAR, APR #
#####################

                            ################# Verify date range in raw.dat$Quarter
current <- "2013-04"        ### IMPORTANT ### corresponds to intended time period
                            ################# before each use.

### Identify Relevant Data ####
# Date Range of Current & Previous Time Period (yyyy-mm-dd)
raw.dat$Quarter[as.POSIXct("2013-04-01") <= raw.dat$Date & raw.dat$Date <= as.POSIXct("2013-04-30")] <- current
raw.dat$Quarter[as.POSIXct("2012-12-01") <= raw.dat$Date & raw.dat$Date < as.POSIXct("2013-04-01")] <- "Previous"

k <- 4                          # number of data cycles in baseline comparison period;
alpha <- 0.001                  # Set nominal Type-I error rate for Poisson Test 
zerohistorythreshold <- 2       # threshold for signal when no history exists


#####################
### CALCULATIONS ####
#####################

# exclude data out of relevant date range
dat.reduced <- raw.dat[is.na(raw.dat$Quarter) == FALSE, ]

tbl <- table(dat.reduced$Grouping, dat.reduced$Quarter)  # tabulate by reject code
trending.dat <- data.frame(rownames(tbl), tbl[,1], tbl[,2])     # trending.dat[,1] is current Q; trending.dat[,2] is prev 4Q sum
rownames(trending.dat) <- NULL
trending.dat <- trending.dat[order(-trending.dat[,2]), ]        # descending order by prev 4Q
colnames(trending.dat) <- c("Code_Group", "current", "Sum_Previous")          # column names
trending.dat$Avg_Previous <- trending.dat$Sum_Previous / k      # average for baseline

# compute thresholds for current quarter counts and flag signals #
trending.dat$Threshold <- sapply(trending.dat$Sum_Previous, FUN=PoissonLRTthreshold,
                                   tau1=k, tau2=1,
                                   alpha=alpha,
                                   zerohistorythreshold=zerohistorythreshold)
trending.dat$Signal <- trending.dat$current > trending.dat$Threshold


proc.time() - ptm

################
### OUTPUTS ####
################

# head(trending.dat, 20)
sum(trending.dat$Signal)
trending.dat[trending.dat$Signal == TRUE, ]
# write.csv(trending.dat, paste(current, "with", k, "Month_Baseline__Numerical_Threshold_Trending.csv", sep = "_"))


# trending.dat$OLD_Signal <- (trending.dat$Avg_Previous + 3*sqrt(trending.dat$Avg_Previous)) < trending.dat$current
# foo <- trending.dat[trending.dat$OLD_Signal == TRUE,]
# foo2 <- foo[!(foo$current < 2), ]
# foo2


 