rm(list = ls()) # clean up
ptm <- proc.time() # timer

######################################
### Jugular Draw Analysis (8005CA) ###
######################################
###################################
### Author: Matt Beckman        ###
### Software: R version 2.14.2  ###
### Date Created: 07-APR-2014   ###
### Last Modified: See SVN      ###
###                             ###
###################################
require(ggplot2)
require(lme4)
require(multcomp)

setwd("U:/Studies/QATP2654 (8005CA)")
jugular <- read.csv("Jugular 14APR14.csv")
jugular$resolved_so2[jugular$resolved_so2>85] <- NA  ## clear aberration with only one coox available

# check out extreme observations 
head(jugular[order(-jugular$resolved_so2), ], 10)
tail(jugular[order(-jugular$resolved_so2), ], 10)


### plot SO2 data ####
# jugular <- aggregate(dat$sO2, by=list(dat$subject, dat$SyringeBarcode), FUN=mean, na.rm=TRUE)
# colnames(jugular) <- c("subject", "draw", "so2")

# boxplot by Jugular Location with IVP overlay
qplot(SyringeBarcode, resolved_so2, data=jugular, geom=c("boxplot", "point"), 
      fill=SyringeBarcode, main="O2 Saturation by Jugular Bulb Location", 
      ylab="O2 Saturation (%)", xlab="Jugular Bulb Location")

# Line chart for each subject (a little chaotic)
ggplot(data=jugular, aes(x=jugular$SyringeBarcode, y=jugular$resolved_so2, group=jugular$subject, color=jugular$subject))+
  geom_point() + geom_line()


### Average over ABL measurements for each draw where available 
mmod2 <- lmer(resolved_so2 ~ SyringeBarcode + (1|subject), data = jugular)
summary(mmod2)

comp.jugular2 <- glht(mmod2, linfct=mcp(SyringeBarcode="Tukey"))
summary(comp.jugular2)


### Fit radiometer as repeats (need to reshape data and redefine the model stmt)
# mmod <- lmer(sO2 ~ SyringeBarcode + (1|subject radiometerid), data = dat)
# summary(mmod)
# 
# comp.jugular <- glht(mmod, linfct=mcp(SyringeBarcode="Tukey"))
# summary(comp.jugular)

### Analysis of J1 placement 
J1.dat <- jugular[jugular$SyringeBarcode=="J1" , ]
J1.dat$tip <- as.factor(c("Body C1", "Jugular Foramen or Higher", rep("Body C1", 13), "Jugular Foramen or Higher", rep("C1-C2", 3), "Body C1", 
                NA, "C1-C2", NA, rep("C1-C2", 2)))
J1.dat[order(-J1.dat$resolved_so2), ]

J1.anova <- aov(resolved_so2~tip, data=J1.dat)
summary(J1.anova)

# Kruskal-Wallis 
kw.dat <- J1.dat[!is.na(J1.dat$tip), ]
J1.kw <- kruskal.test(kw.dat$resolved_so2, kw.dat$tip)
J1.kw

# paired analysis of J1-J2 difference 
j1.dat <- jugular[jugular$SyringeBarcode=="J1",]
j2.dat <- jugular[jugular$SyringeBarcode=="J2",]

paired.dat <- merge(j1.dat, j2.dat, by = "subject")
colnames(paired.dat) <- c("subject", "drop1", "drop2", "so2.J1", "drop3", "drop4", "drop5", "so2.J2", "drop6")
paired.dat$drop1 <- paired.dat$drop2 <- paired.dat$drop3 <- paired.dat$drop4 <- paired.dat$drop5 <- paired.dat$drop6 <- NULL

t.test(paired.dat$so2.J1, paired.dat$so2.J2, paired = TRUE)
wilcox.test(paired.dat$so2.J1, paired.dat$so2.J2, paired = TRUE)


proc.time() - ptm
