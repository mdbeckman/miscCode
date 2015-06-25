###################################################################
### CS&V Quality Monitoring Stratified Random Sample Generator  ###
###################################################################
###################################################################
### Author: Matt Beckman                                        ###
### Software: R version 2.14.1                                  ###
### Date Created: OCT-05-2012                                   ###
### Last Modified: N/A                                          ###
### Reason: N/A                                                 ###
###                                                             ###
###################################################################
###################################################################
## DESCRIPTION:                                                  ##
## This function executes stratified random sampling for Quality ##
##    Monitoring of PE data.  Files are contrubuted by all users ##
##    in the population, then they are randomized and            ##
##    redistributed to the group for quality review.  All users  ##
##    participate in the quality review, and no user reviews any ##
##    file which they have contributed to the sample.            ##
##                                                               ##
## REQUIRED INPUTS:                                              ##
## "data" is a matrix with two columns, the first column is a    ##
##    list of PE numbers and the second column identifies the    ## 
##    production user responsible for the PE.                    ##
## "x" is the number of files to select from each user.          ##
## "y" is the number of independent reviews for every file       ##
##    contributed.                                               ##
##                                                               ##
## OUTPUTS:                                                      ##
## "QM.quality.reviews" includes quality reviews only            ##
## "QM.sample" includes all file reviews (production and quality)##
###################################################################


# Fabricated Data for POC
x <- 3
y <- 2
operators <- c("A", "B", "C", "D")#, "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
names <- rep(operators, x)

# Prepate 
data <- cbind(c(seq(1, length(names), 1)), names)
dat <- data.frame(data)
dat$PK <- paste(dat[,1], dat[,2], sep = " ")
  
ptm <- proc.time()

QM.sample <- dat
colnames(QM.sample) <- c("PE", "Reviewer", "PK")
  
for (i in 1:y){
  resample <- sample(dat[,1], size = nrow(dat), replace = F)
  resample.dat <- data.frame(resample, dat[,2])
  colnames(resample.dat) <- c("PE", "Reviewer")
  resample.dat$PK <- paste(resample.dat[,1], resample.dat[,2], sep = " ")
  
  # correct duplication of PE reviews
  while(sum(duplicated(c(QM.sample$PK, resample.dat$PK))) > 0){
    dups <- which(duplicated(c(QM.sample$PK, resample.dat$PK))) - nrow(QM.sample)  # extract row numbers of duplicates in resample data
    mixer <- sample(1:(x*y), size = 1) # additional unique PE for permutation along with duplicates
    while(mixer %in% dups){mixer <- sample(1:(x*y), size = 1)} # correction if mixer PE is one of the duplicates

    # Permutation of duplicates + mixer PE
    permutation.PEs <- c(mixer, dups)
    resample.dat$PE[permutation.PEs] <- sample(resample.dat$PE[permutation.PEs], size = length(permutation.PEs), replace = F) # permute duplicate PE numbers
    resample.dat$PK <- paste(resample.dat[,1], resample.dat[,2], sep = " ")
    }
  colnames(resample.dat) <- c("PE", "Reviewer", "PK")
  QM.sample <- rbind(QM.sample, resample.dat)
}    

QM.quality.reviews <- QM.sample[-which(QM.sample$PK == dat$PK),]

# Output tables
# QM.quality.reviews
# QM.sample

proc.time() - ptm


