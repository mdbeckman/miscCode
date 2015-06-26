rm(list = ls())  # clean up

####################
### Draw Points ####
####################

samples <- c(1:32)  
n.draws <- length(samples) # total number of draws

SaO2 <- c(100, 93, 93, 85, 85, 80, 80, 75, 75, 70, 70, 100,  # Run 1: desat without COHb
          rep(100, 10),                                      # Run 2: COHb introduction
          93, 93, 90, 90, 85, 85, 79, 79, 100, 100)          # Run 3: desat with elevated COHb 

COHb <- c(rep(1, 12),                                   # Run 1: desat without COHb 
          3, 3, 5, 5, 7, 7, 9, 9, 11, 11,               # Run 2: COHb introduction
          rep(12, 10))                                  # Run 3: desat with elevated COHb

tHb <- rep(15, n.draws)         # total hemaglobin; not manipulated during study

# offset for building IV line graph of draws
ticks.low <-  samples - 0.25  
ticks.high <- samples + 0.25


######################
### Connect Lines ####
######################

dat.wide <- data.frame(samples, SaO2, COHb, tHb, ticks.low, ticks.high)
ticks <- c(ticks.low, ticks.high)
dat.long <- data.frame(rep(samples, 2), rep(SaO2, 2), rep(COHb, 2), rep(tHb, 2), ticks)
colnames(dat.long) <- c("samples", "SaO2", "COHb", "tHb", "ticks")

dat.lines <- dat.long[order(dat.long$ticks), ]



######################
### Plot controls ####
######################
require(Hmisc) # Add minor tick marks

setwd("U:/QATP2601 (COHb)")
pdf("Desaturation Plateaus.pdf")

plot(dat.lines$ticks, dat.lines$SaO2, col='white', ylim=c(0,100), xlab='Sample Number', ylab='%')
minor.tick(nx = 0, ny = 2)

## SaO2
points(samples, SaO2, col = 'red')
lines(dat.lines$ticks, dat.lines$SaO2, col = 'red')
text(2,90,expression(SaO[2]))

# COHb
points(samples, COHb, col = 'blue')
lines(dat.lines$ticks, dat.lines$COHb, col = 'blue')
text(2,5,'COHb')

# tHb
points(samples,tHb,col='green')
lines(dat.lines$ticks, dat.lines$tHb, col = 'green')
text(2,20,'tHb')

legend('right',legend=c('Draw'),col=c('black'),pch=c(1))

dev.off()

