
################################
#### Zero Allowable Defects ####
################################

# solved from binomial distribution
alpha <- c(rep(0.1, 4), rep(0.05, 4))
LQ <- rep(c(0.2, 0.1, 0.05, 0.01), 2)

sample.size <- rep(NA, length(alpha))
for(i in 1:length(sample.size)){
  n <- log(alpha[i])/log(1-LQ[i]) 
  sample.size[i] <- ceiling(n)
} 

# sample sizes with zero defects
cbind(alpha, LQ, sample.size)


##############################
#### One Allowable Defect ####
##############################

# solution from binomial distribution requires lambert's W
require(LambertW)

fcn <- function(a, p){
  # a = alpha; i.e. (1 - confidence)
  # p = LQ; i.e. (1 - reliability)
  LW <- (a*(1-p)^(1/p)*log(1-p))/p
  n <- 1 - (1/p) + lambert_Wm1(LW) / log(1-p)
  print(ceiling(n))
}

alpha <- c(rep(0.1, 4), rep(0.05, 4))
LQ <- rep(c(0.2, 0.1, 0.05, 0.01), 2)

sample.size <- rep(NA, length(alpha))

for(i in 1:length(sample.size)){
  sample.size[i] <- fcn(a = alpha[i], p = LQ[i])  
}

# sample sizes with zero or one defect
cbind(alpha, LQ, sample.size)