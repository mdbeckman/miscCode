rm(list = ls()) # clean up
ptm <- proc.time() # start timer

#####################################
### COHb Randomization (QATP2601) ###
#####################################
#####################################
### Author: Matt Beckman          ###
### Software: R version 2.14.2    ###
### Date Created: 08-JAN-2013     ###
### Last Modified: N/A            ###
### Reason: N/A                   ###
###                               ###
#####################################
####################################################################
## DESCRIPTION: example of the treatment structure for QATP(2601) ##
##      No randomization has been implemented yet                 ##
####################################################################

N.subjects <- 12  # number of subjects
# sensor.labels <- c("NONIN_COF_Xspring", "NONIN_COF_REGspring", "NONIN_TECHWIN_Xspring", "NONIN_TECHWIN_REGspring", "NONIN_8100AA", "MASIMO_RAD7")  # for use on preferred fingers
sensor.pref <-    c("RD16834", "RD16836", "RD16838", "RD16840", "RD16845", "Masimo-03")
sensor.nonpref <- c("RD16835", "RD16837", "RD16839", "RD16841")

finger.pref <- c("ARTERIAL_INDEX", "ARTERIAL_MIDDLE", "ARTERIAL_RING", # preferred fingers
            "CONTRALATERAL_INDEX", "CONTRALATERAL_MIDDLE", "CONTRALATERAL_RING")
finger.nonpref <- c("ARTERIAL_PINKY", "CONTRALATERAL_PINKY", "ARTERIAL_THUMB", "CONTRALATERAL_THUMB")  # non-preferred fingers

# define a sensor pool for each level of random assignment
sensor <- c(1:6, 1:6) # for use on preferred fingers
sensor.repl <- rep(c(1:4), 3) # for use on non-preferred fingers (i.e. pinky, thumb)



################################################
#### Random Assignment to Preferred Fingers ####
################################################

# initialize the randomization matrix
rand.matrix.pref <- as.data.frame(matrix(nrow = N.subjects, ncol = 6))

# establish randomization seed for preferred finger placement
seed <- sample(sensor)
seed

rand.matrix.pref[, 1] <- seed

#### Preferred Fingers 1:5 ####
  # generate new samples for fingers 2, 3, 4, 5 (finger 6 is deterministic)
  # use product of finger differences to identify redundancies

for (i in 2:5){
  repeat {
    rand.matrix.pref[, i] <- sample(sensor)
    redundant <- 0
    for (j in 1:(i-1)){
      if (prod(rand.matrix.pref[, j] - rand.matrix.pref[, i]) == 0) {redundant <- redundant + 1}
    }
    if (redundant == 0) {break}
  }
}

# Preferred Finger 6 
for (i in 1:12){
  foo <- c(1:6)
  for (j in 1:5){
    foo <- foo[-which(foo == rand.matrix.pref[i, j])]
  }
  rand.matrix.pref[i, 6] <- sum(foo)
}


# Sensor Key
rand.matrix.pref2 <- rand.matrix.pref

for (i in 1:12){
  for (j in 1:6){
    rand.matrix.pref2[i, j] <- sensor.pref[rand.matrix.pref[i, j]]
  }
}

rand.matrix.pref2
colnames(rand.matrix.pref2) <- finger.pref

####################################################
#### Random Assignment to Non-Preferred Fingers ####
####################################################

# initialize the randomization matrix
rand.matrix.nonpref <- as.data.frame(matrix(nrow = N.subjects, ncol = 4))

# establish randomization seed for non-preferred finger placement
seed.repl <- sample(sensor.repl)
seed.repl

rand.matrix.nonpref[, 1] <- seed.repl

for (i in 2:3){
  repeat {
    rand.matrix.nonpref[, i] <- sample(sensor.repl)
    redundant <- 0
    for (j in 1:(i-1)){
      if (prod(rand.matrix.nonpref[, j] - rand.matrix.nonpref[, i]) == 0) {redundant <- redundant + 1}
    }
    if (redundant == 0) {break}
  }
}

rand.matrix.nonpref2 <- rand.matrix.nonpref

# Sensor Key
for (i in 1:12){
  for (j in 1:3){
    rand.matrix.nonpref2[i, j] <- sensor.nonpref[rand.matrix.nonpref[i, j]]
  }
}

# Assignment of Lab Safety Sensor 
rand.matrix.nonpref2[, 4] <- "UCSF_LAB_Sensor"

rand.matrix.nonpref2
colnames(rand.matrix.nonpref2) <- finger.nonpref

##############################
#### Combine and Re-order ####
##############################

rand.matrix <- data.frame(rand.matrix.pref2, rand.matrix.nonpref2)
attach(rand.matrix)

# change order of fingers for convenience
rand.matrix2 <- data.frame(CONTRALATERAL_PINKY, 
                                CONTRALATERAL_RING, 
                                CONTRALATERAL_MIDDLE, 
                                CONTRALATERAL_INDEX, 
                                CONTRALATERAL_THUMB, 
                                ARTERIAL_THUMB, 
                                ARTERIAL_INDEX, 
                                ARTERIAL_MIDDLE, 
                                ARTERIAL_RING, 
                                ARTERIAL_PINKY)


rand.matrix3 <- as.data.frame(t(rand.matrix2))

for (i in 1:N.subjects){
  num <- substr(as.character(1000 + i), 2, 4)
  colnames(rand.matrix3)[i] <- paste("Subject", num, sep = "_")
    
}


Hand <- c(rep("CONTRALATERAL", 5), rep("ARTERIAL", 5))
Finger <- c("PINKY", "RING", "MIDDLE", "INDEX", "THUMB", "THUMB", "INDEX", "MIDDLE", "RING", "PINKY")

rand.matrix.final <- data.frame(Hand, Finger, rand.matrix3)


proc.time() - ptm


###############################
#### Export Table to Excel ####
###############################
                                   

dt <- format(Sys.Date(), format = "%m-%d-%Y")

setwd("U:/QATP2601 (COHb)")
write.csv(rand.matrix.final, paste("QATP2601 (COHb) RANDOMIZATION ", dt, ".CSV", sep = ""))

# r2wd or RTF package to export for MS Word