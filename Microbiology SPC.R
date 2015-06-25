###################################
### Microbiology SPC            ###
###################################
###################################
### Author: Matt Beckman        ###
### Software: R version 2.14.1  ###
### Date Created: DEC-17-2012   ###
### Last Modified: N/A          ###
### Reason: N/A                 ###
###                             ###
###################################
###################################################################
## DESCRIPTION:                                                  ##
##    This function returns calculations required for individual ##
##        site monitoring and for building an x-bar chart with   ##
##        variable subgroup size appropriate for monthly control ##
##        charting of Microbiology data                          ##
##                                                               ##
## INPUTS:                                                       ##
##    -rawdat: [location, date (m/d/yyyy), Bacterial, Fungal]    ##
##    -LOCATION: string value to specify CEA (NorthCEA, etc)     ##
##    -phase1.b[optional]: beginning of baseline formatted as    ##
##        yyyymm; default value is first month available         ##
##    -phase1.e[optional]: formatted as YYYYMM; default value is ##
##        second to last month available in data set             ##
##    -phase1.exclude[optional]: baseline months (YYYYMM) to     ##
##        exclude from control limit calculations for Xbar chart ##
##    -site.ARL[optional]: defaults to 500                       ##
##    -UCL.multiplier: specifies number of standard deviations   ##
##        between chart mean and UCL                             ##
##                                                               ##
## OUTPUTS:                                                      ##
##    -dat: modified raw data with calculated columns            ##
##    -xbar.summary: monthly results with CL & alert indicators  ##
##        for monthly avg CFU total and abberrant site           ##
##                                                               ##
###################################################################


setwd("H:/_Project and gCAPA Support/2012/Microbiology SPC")
getwd()


### INPUTS ####
rawdat <- read.csv("Surface Raw Data.csv", header = TRUE, sep = ",")   ## Temperary line; input specified by spotfire analyst
colnames(rawdat) <- c("LOTID", "CREATETXNTIME", "SITE", "BACTERIAL", "FUNGAL", "Collected.Date", "Assigned.Location")

LOCATION <- "North CEA Main" ## Temperary line; input specified by spotfire analyst
phase1.exclude <- "201101, 201207"
# phase1.exclude <- NULL
# site.ARL <- NULL
site.ARL <- 1000

phase1.b <- 201012  ## Temperary line; input specified by spotfire analyst
phase1.e <- 201209  ## Temperary line; input specified by spotfire analyst
# phase1.b <- NULL  ## Temperary line; input specified by spotfire analyst
# phase1.e <- NULL  ## Temperary line; input specified by spotfire analyst

UCL.multiplier <- 4.5

### PREPARING DATA ####

# Subset Event Data to include User-specified YYYYMM values
if (!is.null(phase1.exclude)){
  phase1.exclude <- gsub(" *", "", unlist(strsplit(phase1.exclude, split=",")))
}


phase1.exclude <- as.numeric(phase1.exclude)

# preparations for the complete location data
dat <- subset(rawdat, subset = (rawdat$Assigned.Location == LOCATION))

dat$Collected.Date <- as.Date(dat$Collected.Date, format = "%m/%d/%Y")
dat$MM <- as.numeric(format(dat$Collected.Date, format="%m"))
dat$YYYY <- as.numeric(format(dat$Collected.Date, format="%Y"))
dat$YYYYMM <- dat$YYYY*100 + dat$MM # month & year for calculation and charting purposes
dat$Month <- format(dat$Collected.Date, format = "%b-%Y") # month and year for display purposes

dat$MM <- NULL
dat$YYYY <- NULL

dat$Total.CFU <- dat$BACTERIAL + dat$FUNGAL
dat$Total.CFU[dat$Total.CFU == 0] <- 1  ## cases for CFU < 1 should be nonzero; convert to 1

# preparations for the phase I (Baseline) data

if(is.null(phase1.e)) {
  phase1.end <- max(dat$YYYYMM) - 1
} else {
  phase1.end <- phase1.e
}

if(is.null(phase1.b)) {
  phase1.beg <- min(dat$YYYYMM)
} else {
  phase1.beg <- phase1.b
}


if(is.null(phase1.exclude)) {
  dat$phase1.included <- (phase1.beg <= dat$YYYYMM) & (dat$YYYYMM <= phase1.end)
} else {
  dat$phase1.included <- (phase1.beg <= dat$YYYYMM) & (dat$YYYYMM <= phase1.end) & !(dat$YYYYMM %in% phase1.exclude)
}


phase1.dat <- dat[(dat$phase1.included == TRUE), ]


### INDIVIDUAL SITE ALERT LIMIT #### 

# default site ARL is 500
if(is.null(site.ARL)) {site.ARL <- 500}

site.n <- nrow(phase1.dat)
prop.oc <- (1 / site.ARL)
alert.index <- ceiling(prop.oc * site.n)
site.limit <-  phase1.dat$Total.CFU[order(phase1.dat$Total.CFU, decreasing = TRUE)[alert.index]]
dat$site.UCL <- site.limit

dat$site.alert <- FALSE
dat$site.alert[dat$Total.CFU >= site.limit] <- TRUE

### X-BAR ALERT LIMIT ####
if(is.null(UCL.multiplier)) {UCL.multiplier <- 3}

phase1.mean <- aggregate(phase1.dat$Total.CFU, by = list(phase1.dat$Assigned.Location, phase1.dat$YYYYMM, phase1.dat$Month), FUN = mean, na.rm = TRUE)
phase1.sd <- aggregate(phase1.dat$Total.CFU, by = list(phase1.dat$Assigned.Location, phase1.dat$YYYYMM, phase1.dat$Month), FUN = sd, na.rm = TRUE)
phase1.merge1 <- merge(x = phase1.mean, y = phase1.sd, by = c("Group.1", "Group.2", "Group.3"))
colnames(phase1.merge1) <- c("LOCATION", "YYYYMM", "MONTH", "MEAN", "SD")

phase1.N <- aggregate(phase1.dat$YYYYMM, by = list(phase1.dat$Assigned.Location, phase1.dat$YYYYMM, phase1.dat$Month), FUN = length)
colnames(phase1.N) <- c("LOCATION", "YYYYMM", "MONTH", "N")

phase1.summary <- merge(x = phase1.merge1, y = phase1.N)

# see Montgomery, D.C. (2009).  Introduction to Statistical Quality Control (6th ed.). Jefferson City, MO: John Wiley & Sons.
GrandMean <- sum(phase1.summary$MEAN * phase1.summary$N) / sum(phase1.summary$N) # eqn 6.30; p. 255
sbar <- sqrt(sum((phase1.summary$N - 1) * phase1.summary$SD^2) / (sum(phase1.summary$N - 1))) # eqn 6.31; p. 255


dat.mean <- aggregate(dat$Total.CFU, by = list(dat$Assigned.Location, dat$YYYYMM, dat$Month), FUN = mean, na.rm = TRUE)
colnames(dat.mean) <- c("LOCATION", "YYYYMM", "MONTH", "MEAN.CFU")

dat.N <- aggregate(dat$YYYYMM, by = list(dat$Assigned.Location, dat$YYYYMM, dat$Month), FUN = length)
colnames(dat.N) <- c("LOCATION", "YYYYMM", "MONTH", "N")

xbar.summary <- merge(x = dat.mean, y = dat.N)
xbar.summary$Baseline.Included <- NA
xbar.summary$Baseline.Included[xbar.summary$YYYYMM %in% phase1.summary$YYYYMM] <- TRUE
xbar.summary$Baseline.Included[!(xbar.summary$YYYYMM %in% phase1.summary$YYYYMM)] <- FALSE


## Source: Minitab 16 >> Help >> Methods & Formulas >> Variables Charts for Groups >> Methods for estimating standard deviation
sp <- sqrt(sum((phase1.summary$N - 1) * phase1.summary$SD^2) / (sum(phase1.summary$N - 1))) # eqn 6.31; p. 255
df <- sum(phase1.summary$N - 1) 
c4 <- sqrt(2 / df) * exp(lgamma((df + 1) / 2) - lgamma(df / 2)) # exact solution
# c4 <- 4 * (sum(phase1.summary$N) - 1) / (4 * sum(phase1.summary$N) - 3) # Approximation; see Montgomery p.702
xbar.summary$Baseline.Sbar <- sp / c4
xbar.summary$Baseline.Mean <- GrandMean
xbar.summary$UCL <- xbar.summary$Baseline.Mean + UCL.multiplier * xbar.summary$Baseline.Sbar / sqrt(xbar.summary$N)
xbar.summary$LCL <- xbar.summary$Baseline.Mean - UCL.multiplier * xbar.summary$Baseline.Sbar / sqrt(xbar.summary$N)

xbar.summary$df <- NULL
xbar.summary$c4 <- NULL

### ALERTS ####
xbar.summary$xbarAlert <- FALSE
xbar.summary$xbarAlert[xbar.summary$MEAN.CFU > xbar.summary$UCL] <- TRUE # Alert for overall CFU count in CEA

xbar.summary$siteAlert <- FALSE
xbar.summary$siteAlert[xbar.summary$YYYYMM %in% dat$YYYYMM[dat$site.alert == TRUE]] <- TRUE # Alert for abberrant site


### OUTPUTS ####

# dat
# xbar.summary

dat[dat$site.alert == TRUE,]


# write.csv(phase1.dat, "BASELINE Surface Data.csv")