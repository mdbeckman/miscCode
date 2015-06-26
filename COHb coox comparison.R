rm(list = ls()) # clean up

###################################
### COHb Co-Oximeter Comparison ###
###################################
###################################
### Author: Matt Beckman        ###
### Software: R version 2.14.2  ###
### Date Created: 08-JAN-2013   ###
### Last Modified: N/A          ###
### Reason: N/A                 ###
###                             ###
###################################
####################################################################
## DESCRIPTION: co-oximeter comparison for COHb study (QATP2601)  ##
##                                                                ##
####################################################################
require(lme4)
require(ggplot2)
library(lattice) 


##################
### Functions ####
##################

identity_line <- function(x,y,...){
  points(x,y,...)
  abline(a = 0, b = 1, col = 3, ...)
  abline(a = 3, b = 1, lty = 2, ...)
  abline(a = -3, b = 1, lty = 2, ...)
}


###################
### Initialize ####
###################

setwd("U:/QATP2601 (COHb)/COOX Comparison")
getwd()

# read in raw data table
dat <- read.csv("cooxdata 08JAN14 (corrected).csv", header = TRUE, sep = ",") 
# dat <- read.csv("cooxdata 08JAN14.csv", header = TRUE, sep = ",") 

# corrections to co-oximeter naming
dat$COOX[dat$COOX == "ABL803"] <- "ABL830"
dat$COOX[dat$COOX == "Rapidpoint"] <- "RP405"
dat$SUBJECT <- factor(dat$SUBJECT)
    # > names(raw.dat)
    # [1] "STUDY"     "SITE"      "SUBJECT"   "SUBJECTID" "COOX"      "rdraw"     "COOX_TM"   "SAO2"      "HHB"       "O2HB"     
    # [11] "THB"       "COHB"      "METHB"     "PH"        "PCO2"      "PO2"       "NOTES"     "draw"   


### recast data set in wide format ###
dat.wide <- reshape(data = dat, idvar = c("SUBJECT", "draw"), timevar = "COOX", direction = "wide",
                    drop = c("STUDY", "SITE", "SUBJECTID", "rdraw", "COOX_TM", "PH", "PCO2", "PO2", "NOTES"))

    # > names(dat.wide)
    # [1] "SUBJECT"      "draw"         "SAO2.OSM3"    "HHB.OSM3"     "O2HB.OSM3"    "THB.OSM3"     "COHB.OSM3"    "METHB.OSM3"  
    # [9] "SAO2.ABL830"  "HHB.ABL830"   "O2HB.ABL830"  "THB.ABL830"   "COHB.ABL830"  "METHB.ABL830" "SAO2.RP405"   "HHB.RP405"   
    # [17] "O2HB.RP405"   "THB.RP405"    "COHB.RP405"   "METHB.RP405" 


###########################################
### Pairwise Comparison of SAO2 & COHb ####
###########################################


# Plot Matrix for pairwise comparison of SaO2 and COHb with identity line
pdf("Co-Oximeter Comparison - SaO2 COHb.pdf")
pairs(dat.wide[ , c(3, 9, 15)], lower.panel = NULL, upper.panel = identity_line) # SAO2
pairs(dat.wide[ , c(7, 13, 19)], lower.panel = NULL, upper.panel = identity_line) # COHb
dev.off()


#####################################################################
### test for COOX differences (repeated measures within subject) ####
#####################################################################

########################
### SAO2 Comparison ####

## linear mixed-effects approach
dat.SAO2 <- dat[is.na(dat$SAO2) == FALSE, ]
dat.SAO2$draw.nest <- interaction(dat.SAO2$SUBJECT, dat.SAO2$draw, drop = TRUE) # build the interaction term for nesting
sao2.lmer <- lmer(SAO2 ~ 1 + (1|SUBJECT) + (1|draw.nest), data = dat.SAO2) # COOX is pooled with resids
summary(sao2.lmer)

dat.SAO2$resids <- residuals(sao2.lmer)
dat.SAO2$fits <- fitted(sao2.lmer)

# BLand-Altman plot of residuals by COOX
pdf("Bland-Altman Plot of SAO2 by COOX.pdf")

qplot(fits, resids, col = COOX, data = dat.SAO2, geom = c("point"),
      main = "Bland-Altman Plot of SaO2 by Co-Oximeter", 
      xlab = "Average SaO2 Value", ylab = "Deviation") +  
  geom_hline(yintercept = 0) +    
  geom_smooth(method = "lm", fill = NA)          # loess smoother

dev.off()

# Kernel Density plot of errors by COOX
pdf("Density Plot of SAO2 by COOX.pdf")
with(dat.SAO2, densityplot(~resids|COOX, pch = "|",
                           main="Density Plot of SaO2 Error by Co-Oximeter", 
                           xlab="Difference from Average", 
                           layout=c(1,3)))
dev.off()


# evaluate differences b/w co-oximeters after adjusting for random effects of subject and draw
COOX.aov <- aov(resids ~ COOX, data = dat.SAO2)
summary(COOX.aov)
TukeyHSD(COOX.aov, which = "COOX", conf.level = 0.95, data = dat.SAO2)


# mean of difference
mean(dat.SAO2$resids[dat.SAO2$COOX == "ABL830"])
mean(dat.SAO2$resids[dat.SAO2$COOX == "OSM3"])
mean(dat.SAO2$resids[dat.SAO2$COOX == "RP405"])

# SD of difference
sd(dat.SAO2$resids[dat.SAO2$COOX == "ABL830"])
sd(dat.SAO2$resids[dat.SAO2$COOX == "OSM3"])
sd(dat.SAO2$resids[dat.SAO2$COOX == "RP405"])

# estimate repeatability by adjusting for linearity present among lmer residuals
repeatability.SAO2.lm <- lm(resids ~ fits * COOX, data = dat.SAO2)
dat.SAO2$repeatability <- residuals(repeatability.SAO2.lm) # removes slope and intercept over desat range

sd(dat.SAO2$repeatability[dat.SAO2$COOX == "ABL830"])
sd(dat.SAO2$repeatability[dat.SAO2$COOX == "OSM3"])
sd(dat.SAO2$repeatability[dat.SAO2$COOX == "RP405"])



########################
### COHB Comparison ####

## linear mixed-effects approach
dat.COHB <- dat[is.na(dat$COHB) == FALSE, ]
dat.COHB$draw.nest <- interaction(dat.COHB$SUBJECT, dat.COHB$draw, drop = TRUE) # build the interaction term for nesting
COHB.lmer <- lmer(COHB ~ 1 + (1|SUBJECT) + (1|draw.nest), data = dat.COHB) # COOX is pooled with resids
summary(COHB.lmer)

dat.COHB$resids <- residuals(COHB.lmer)
dat.COHB$fits <- fitted(COHB.lmer)

# residual plot by COOX

pdf("Bland-Altman Plot of COHB by COOX.pdf")

qplot(fits, resids, col = COOX, data = dat.COHB, geom = c("point"),
      main = "Bland-Altman Plot of COHB by Co-Oximeter", 
      xlab = "Average COHB Value", ylab = "Deviation") +  
  geom_hline(yintercept = 0) +    
  geom_smooth(method = "lm", fill = NA)          # loess smoother

dev.off()

# Kernel Density plot of errors by COOX
pdf("Density Plot of COHb by COOX.pdf")
with(dat.COHB, densityplot(~resids|COOX, pch = "|",
                           main="Density Plot of COHb Error by Co-Oximeter", 
                           xlab="Difference from Average", 
                           layout=c(1,3)))
dev.off()

# evaluate differences b/w co-oximeters after adjusting for random effects of subject and draw
COOX.aov <- aov(resids ~ COOX, data = dat.COHB)
summary(COOX.aov)
TukeyHSD(COOX.aov, which = "COOX", conf.level = 0.95, data = dat.COHB)


# Mean of difference
mean(dat.COHB$resids[dat.COHB$COOX == "ABL830"])
mean(dat.COHB$resids[dat.COHB$COOX == "OSM3"])
mean(dat.COHB$resids[dat.COHB$COOX == "RP405"])

# SD of difference
sd(dat.COHB$resids[dat.COHB$COOX == "ABL830"])
sd(dat.COHB$resids[dat.COHB$COOX == "OSM3"])
sd(dat.COHB$resids[dat.COHB$COOX == "RP405"])

# estimate repeatability by adjusting for linearity present among lmer residuals
repeatability.COHB.lm <- lm(resids ~ fits * COOX, data = dat.COHB)
dat.COHB$repeatability <- residuals(repeatability.COHB.lm) # removes slope and intercept over desat range

sd(dat.COHB$repeatability[dat.COHB$COOX == "ABL830"])
sd(dat.COHB$repeatability[dat.COHB$COOX == "OSM3"])
sd(dat.COHB$repeatability[dat.COHB$COOX == "RP405"])




#########################
### METHB Comparison ####

## linear mixed-effects approach
dat.METHB <- dat[is.na(dat$METHB) == FALSE, ]
METHB.lmer <- lmer(METHB ~ 1 + (1|SUBJECT), data = dat.METHB) # COOX variability is pooled with resids
summary(METHB.lmer)

dat.METHB$resids <- residuals(METHB.lmer)
dat.METHB$fits <- fitted(METHB.lmer)

# residual plot by COOX

pdf("Bland-Altman Plot of METHb by COOX.pdf")

qplot(fits, resids, col = COOX, data = dat.METHB, geom = c("point"),
      main = "Bland-Altman Plot of METHb by Co-Oximeter", 
      xlab = "Average METHb Value", ylab = "Deviation") +  
  geom_hline(yintercept = 0) +    
  geom_smooth(method = "lm", fill = NA)          # loess smoother

dev.off()

# Kernel Density plot of errors by COOX
pdf("Density Plot of METHb by COOX.pdf")
with(dat.METHB, densityplot(~resids|COOX, pch = "|",
                           main="Density Plot of METHb Error by Co-Oximeter", 
                           xlab="Difference from Average", 
                           layout=c(1,3)))
dev.off()

# evaluate differences b/w co-oximeters after adjusting for random effects of subject and draw
COOX.aov <- aov(resids ~ COOX, data = dat.METHB)
summary(COOX.aov)
TukeyHSD(COOX.aov, which = "COOX", conf.level = 0.95, data = dat.METHB)


# Mean of difference
mean(dat.METHB$resids[dat.METHB$COOX == "ABL830"])
mean(dat.METHB$resids[dat.METHB$COOX == "OSM3"])
mean(dat.METHB$resids[dat.METHB$COOX == "RP405"])

# SD of difference
sd(dat.METHB$resids[dat.METHB$COOX == "ABL830"])
sd(dat.METHB$resids[dat.METHB$COOX == "OSM3"])
sd(dat.METHB$resids[dat.METHB$COOX == "RP405"])

# estimate repeatability by adjusting for linearity present among lmer residuals
repeatability.METHB.lm <- lm(resids ~ fits * COOX, data = dat.METHB)
dat.METHB$repeatability <- residuals(repeatability.METHB.lm) # removes slope and intercept over desat range

sd(dat.METHB$repeatability[dat.METHB$COOX == "ABL830"])
sd(dat.METHB$repeatability[dat.METHB$COOX == "OSM3"])
sd(dat.METHB$repeatability[dat.METHB$COOX == "RP405"])



#######################
### tHb Comparison ####

## linear mixed-effects approach
dat.THB <- dat[is.na(dat$THB) == FALSE, ]
THB.lmer <- lmer(THB ~ 1 + (1|SUBJECT), data = dat.THB) # COOX variability is pooled with resids
summary(THB.lmer)

dat.THB$resids <- residuals(THB.lmer)
dat.THB$fits <- fitted(THB.lmer)

# residual plot by COOX

pdf("Bland-Altman Plot of THB by COOX.pdf")

qplot(fits, resids, col = COOX, data = dat.THB, geom = c("point"),
      main = "Bland-Altman Plot of THB by Co-Oximeter", 
      xlab = "Average THB Value", ylab = "Deviation") +  
  geom_hline(yintercept = 0) +    
  geom_smooth(method = "lm", fill = NA)          # loess smoother

dev.off()

# Kernel Density plot of errors by COOX
pdf("Density Plot of THB by COOX.pdf")
with(dat.THB, densityplot(~resids|COOX, pch = "|",
                            main="Density Plot of THB Error by Co-Oximeter", 
                            xlab="Difference from Average", 
                            layout=c(1,3)))
dev.off()

# evaluate differences b/w co-oximeters after adjusting for random effects of subject and draw
COOX.aov <- aov(resids ~ COOX, data = dat.THB)
summary(COOX.aov)
TukeyHSD(COOX.aov, which = "COOX", conf.level = 0.95, data = dat.THB)


# Mean of difference
mean(dat.THB$resids[dat.THB$COOX == "ABL830"])
mean(dat.THB$resids[dat.THB$COOX == "OSM3"])
mean(dat.THB$resids[dat.THB$COOX == "RP405"])

# SD of difference
sd(dat.THB$resids[dat.THB$COOX == "ABL830"])
sd(dat.THB$resids[dat.THB$COOX == "OSM3"])
sd(dat.THB$resids[dat.THB$COOX == "RP405"])

# estimate repeatability by adjusting for linearity present among lmer residuals
repeatability.THB.lm <- lm(resids ~ fits * COOX, data = dat.THB)
dat.THB$repeatability <- residuals(repeatability.THB.lm) # removes slope and intercept over desat range

sd(dat.THB$repeatability[dat.THB$COOX == "ABL830"])
sd(dat.THB$repeatability[dat.THB$COOX == "OSM3"])
sd(dat.THB$repeatability[dat.THB$COOX == "RP405"])


#############################################
### SAO2 vs measured O2HB / (O2HB + HHB) ####

## calculate difference between SAO2 and O2HB / (O2HB + HHB) 
dat$CalcSAO2 <- dat$O2HB / (dat$O2HB + dat$HHB) * 100
dat$SAO2minusCalc <- dat$SAO2 - dat$CalcSAO2


## linear mixed-effects approach
dat.SAO2minusCalc <- dat[is.na(dat$SAO2minusCalc) == FALSE, ]
dat.SAO2minusCalc$draw.nest <- interaction(dat.SAO2minusCalc$SUBJECT, dat.SAO2minusCalc$draw, drop = TRUE) # build the interaction term for nesting
SAO2minusCalc.lmer <- lmer(SAO2minusCalc ~ 1 + (1|SUBJECT) + (1|draw.nest), data = dat.SAO2minusCalc) # COOX is pooled with resids
summary(SAO2minusCalc.lmer)

dat.SAO2minusCalc$resids <- residuals(SAO2minusCalc.lmer)
dat.SAO2minusCalc$fits <- fitted(SAO2minusCalc.lmer)

# Kernel Density plot of errors by COOX
pdf("Density Plot of (SAO2 - Calc) by COOX.pdf")
with(dat.SAO2minusCalc, densityplot(~resids|COOX, pch = "|",
                           main="Density Plot of (SAO2 - Calc) Error by Co-Oximeter", 
                           xlab="Difference from Average", 
                           layout=c(1,2)))
dev.off()


# evaluate differences b/w co-oximeters after adjusting for random effects of subject and draw
COOX.aov <- aov(resids ~ COOX, data = dat.SAO2minusCalc)
summary(COOX.aov)
TukeyHSD(COOX.aov, which = "COOX", conf.level = 0.95, data = dat.SAO2minusCalc)


# mean of difference
mean(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "ABL830"])
mean(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "OSM3"])
mean(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "RP405"])

# SD of difference
sd(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "ABL830"])
sd(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "OSM3"])
sd(dat.SAO2minusCalc$resids[dat.SAO2minusCalc$COOX == "RP405"])

# estimate repeatability by adjusting for linearity present among lmer residuals
repeatability.SAO2minusCalc.lm <- lm(resids ~ fits * COOX, data = dat.SAO2minusCalc)
dat.SAO2minusCalc$repeatability <- residuals(repeatability.SAO2minusCalc.lm) # removes slope and intercept over desat range

sd(dat.SAO2minusCalc$repeatability[dat.SAO2minusCalc$COOX == "ABL830"])
sd(dat.SAO2minusCalc$repeatability[dat.SAO2minusCalc$COOX == "OSM3"])
sd(dat.SAO2minusCalc$repeatability[dat.SAO2minusCalc$COOX == "RP405"])


