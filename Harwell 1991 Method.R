rm(list = ls())     # clear workspace 
ptm <- proc.time()  # stop watch begin

##########################################
### Simulation Study for Harwell Test  ###
##########################################
##############################################
### Author: Matt Beckman                   ###
### Date Created: 07/12/2013               ###
### Last Modified:                         ###
##############################################

levels.a <- 2
levels.b <- 3
repl <- 10

N <- levels.a * levels.b * repl

# Build Matrix of Factor-Level Combinations
Blk <- rep(NA, N)
A <- rep(NA, N)
B <- rep(NA, N)
AB <- rep(NA, N)
  
counter <- 0
for (i in 1:repl){
  for (j in 1:levels.b){
    for (k in 1:levels.a){
      counter <- counter + 1
      
      Blk[counter] <- i
      B[counter] <- j
      A[counter] <- k
      AB[counter] <- paste(k, j)
    }
  }
}

Y <- sample(1:N, replace = FALSE) # random simulation of rank response

# create data frame, convert to factors, & name columns
dat <- data.frame(Y, as.factor(Blk), as.factor(A), as.factor(B), as.factor(AB))
colnames(dat) <- c("Response", "Block", "A", "B", "AB")

# fit models in order to isolate each effect
AB.iso <- with(dat, lm(Y ~ Blk + A + B))
A.iso <- with(dat, lm(Y ~ Blk + B + A:B))
B.iso <- with(dat, lm(Y ~ Blk + A + A:B))

# AB: extract and rank residuals
dat$AB.resid.raw <- AB.iso$resid
dat$AB.rank.resid <- rank(AB.iso$resid)

# AB: calculate sums of squares 
AB.trt.mean <- aggregate(dat$AB.rank.resid, by = list(dat$AB), FUN = mean)
colnames(AB.trt.mean) <- c("AB", "AB.trt.mean")
dat <- merge(dat, AB.trt.mean, by = "AB")

AB.SS.TRT <- with(dat, sum((AB.rank.resid - AB.trt.mean)^2))
AB.SS.TOT <- with(dat, sum((AB.rank.resid - mean(AB.rank.resid))^2))

# AB: test statistic & p-value
AB.TS <- AB.SS.TRT / AB.SS.TOT * (N - 1)
AB.p.value <- pchisq(AB.TS, df = (levels.a-1)*(levels.b-1), lower.tail = FALSE)


# A: extract and rank residuals
dat$A.resid.raw <- A.iso$resid
dat$A.rank.resid <- rank(A.iso$resid)

# A: calculate sums of squares 
A.trt.mean <- aggregate(dat$A.rank.resid, by = list(dat$A), FUN = mean)
colnames(A.trt.mean) <- c("A", "A.trt.mean")
dat <- merge(dat, A.trt.mean, by = "A")

A.SS.TRT <- with(dat, sum((A.rank.resid - A.trt.mean)^2))
A.SS.TOT <- with(dat, sum((A.rank.resid - mean(A.rank.resid))^2))

# A: test statistic & p-value
A.TS <- A.SS.TRT / A.SS.TOT * (N - 1)
A.p.value <- pchisq(A.TS, df = (levels.a-1)*(levels.b-1), lower.tail = FALSE)



# B: extract and rank residuals
dat$B.resid.raw <- B.iso$resid
dat$B.rank.resid <- rank(B.iso$resid)

# B: calculate sums of squares 
B.trt.mean <- aggregate(dat$B.rank.resid, by = list(dat$B), FUN = mean)
colnames(B.trt.mean) <- c("B", "B.trt.mean")
dat <- merge(dat, B.trt.mean, by = "B")

B.SS.TRT <- with(dat, sum((B.rank.resid - B.trt.mean)^2))
B.SS.TOT <- with(dat, sum((B.rank.resid - mean(B.rank.resid))^2))

# B: test statistic & p-value
B.TS <- B.SS.TRT / B.SS.TOT * (N - 1)
B.p.value <- pchisq(B.TS, df = (levels.a-1)*(levels.b-1), lower.tail = FALSE)


### Result

A.TS; A.p.value
B.TS; B.p.value
AB.TS; AB.p.value


proc.time() - ptm  # stop watch end
