
# Function that collects sample means and sample sd into a matrix
# X = vector of values to resample from
# N = sample size of each resample
# TIMES = the number of resamples to generate





gen.cis = function(X, N, TIMES){
  ci = NULL
  for(i in 1:TIMES){
    s = sample(X,N,replace = T)
    ci = rbind(ci,c(N, mean(s), sd(s)))
  }
  ci=ci[order(ci[,2]),]
  colnames(ci) <- c("N", "Sample.Mean", "Sample.SD")
  return(ci)
}

gen.cis2 <- function(X, N, TIMES){
  resample.dat <- sample(X, size = N*TIMES, replace = TRUE) # generate vector of random resamples
  resample.matrix <- matrix(data = resample.dat, nrow = TIMES, ncol = N) # organize as matrix
  # calculate sample mean and sd for rows of resample.matrix; 
  N <- rep(N, TIMES)
  Sample.Mean <- apply(X = resample.matrix, MARGIN = 1, FUN = mean)
  Sample.SD <- apply(X = resample.matrix, MARGIN = 1, FUN = sd)
  ci <- cbind(N, Sample.Mean, Sample.SD)
  ci <- ci[order(ci[ ,2]), ]
  return(ci)  
}


# Function that plots the CIs based on a matrix of sample means and sd
# The function creates the matrix of CIs
# The user provides the following arguments

# X = vector of values to resample from
# N = sample size of each resample
# TIMES = the number of resamples to generate
# LEVEL = the confidence level as a whole number (e.g., 95 for a 95% CI)

# White vertical line in plot locates the value of the population mean
# Black horizontal line = CI includes the population mean
# Red horizontal line = CI does NOT include the population mean
# A yellow bar marks the sample mean for each CI (at the center of each CI)

plot.cis = function(X, N = 5, TIMES = 500, LEVEL = 95, XLABEL = "VARIABLE"){
  X = X[!is.na(X)]
  CI = gen.cis(X, N, TIMES)
  lowx = mean(X) - 5*sd(X)
  hix = mean(X) + 5*sd(X)
  lowx = round(lowx/5)*5
  hix = round(hix/5)*5
  plot(c(lowx,hix), c(0,nrow(CI)), col="white", xlab=XLABEL, ylab="CIs ORDERED FROM LOWEST TO HIGHEST MEAN", main = paste(LEVEL, "% Conf. Intervals, n = ", CI[1,1],sep=""))
  include.mean = nrow(CI)
  for(i in 1:nrow(CI)){
    me = qt((LEVEL/100 + (100-LEVEL)/200),CI[i,1]-1)*CI[i,3]/sqrt(CI[i,1])
    low = CI[i,2] - me
    high = CI[i,2] + me
    the.color = "black"
    if(low > mean(X) | high < mean(X)){
      the.color = "red"
      include.mean = include.mean - 1
    }
    lines(c(low,high),c(i,i),col = the.color)
    lines(c(CI[i,2]-.2,CI[i,2]+.2),c(i,i),col="yellow")
  }
  lines(c(mean(X),mean(X)),c(0,nrow(CI)),col="white")
  pct = round(100*include.mean/nrow(CI),1)
  text(mean(X) + 2*sd(X), 1, paste(pct,"% of CIs include\nthe population mean"),pos=4)
  #	return(X)
}


plot.cis2 = function(X, N = 5, TIMES = 500, LEVEL = 95, XLABEL = "VARIABLE"){
  X = X[!is.na(X)]
  foo = gen.cis2(X, N, TIMES)
  CI <- as.data.frame(foo)
  lowx = mean(X) - 5*sd(X)
  hix = mean(X) + 5*sd(X)
  lowx = round(lowx/5)*5
  hix = round(hix/5)*5
  plot(c(lowx,hix), c(0,nrow(CI)), col="white", xlab=XLABEL, ylab="CIs ORDERED FROM LOWEST TO HIGHEST MEAN", main = paste(LEVEL, "% Conf. Intervals, n = ", N,sep=""))
  CI$Margin.Error = qt((LEVEL/100 + (100-LEVEL)/200), N - 1) * CI$Sample.SD / sqrt(CI$N)
  CI$Low = CI$Sample.Mean - CI$Margin.Error
  CI$High = CI$Sample.Mean + CI$Margin.Error
  CI$Color = "black"
  CI$Color[CI$Low > mean(X) | CI$High < mean(X)] = "red"
  
  # count CI's that do not include the mean of parent sample
  include.mean = nrow(CI)
  include.mean = include.mean - sum(CI$Color == "red") 
  CI$INDEX <- seq(1, nrow(CI), by = 1)
  
  # plot confidence intervals
  apply(X = CI[ , c("Low", "INDEX", "High", "INDEX")], MARGIN = 1, FUN = lines, col = CI$Color) #############
#   lines(c(CI$Low, CI$High), c(CI$INDEX, CI$INDEX), col = CI$Color)                                         #############
  apply(X = CI, MARGIN = 1, FUN = lines,  = CI$Sample.Mean - .2, y = CI$Sample.Mean + .2, col = "yellow")  #############

  lines(c(mean(X),mean(X)),c(0,nrow(CI)),col="white")
  pct = round(100*include.mean/nrow(CI),1)
  text(mean(X) + 2*sd(X), 1, paste(pct,"% of CIs include\nthe population mean"),pos=4)
  #  return(X)
}


dat <- runif(30)

ptm <- proc.time()
out <- gen.cis(dat, 30, 25000)
head(out)

proc.time() - ptm

ptm <- proc.time()
out2 <- gen.cis2(dat, 30, 25000)
head(out2)

proc.time() - ptm


plot.new()
lines(c(1, 2, 3, 4))

