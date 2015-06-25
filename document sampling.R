nsims <- 100000
n <- 22

TESTPASS <- rep(NA, nsims)

for (i in 1:nsims){
  u <- rbinom(n, 1,0.9)
  X <- u*rbinom(n, 1, (1/90)) + (1-u)*rbinom(n, 1, 0.9)
  TESTPASS[i] <- !any(X > 0)
}
mean(TESTPASS)



