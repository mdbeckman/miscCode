# 
# Matthew Beckman
# Last updated 3/5/08
#
############ multivariate functions ##############

box.cox <- function(x){
	# this function performs a box-cox transformation 
	# for univariate data.

	lambda <- c(seq(-3, 3, 0.1))
	n <- length(x)
	m <- length(lambda)
	y <- double(n)
	l.lambda <- double(m)
	# Calculate variance for each lambda value
	for (i in 1:m) {
		if (lambda[i] == 0) y <- log(x)
		else y <- (x^lambda[i] - 1) / lambda[i]
		l.lambda[i] <- (-1)*(n/2)*log((1/n)*sum((y - mean(y))^2)) + (lambda[i] - 1)*sum(log(x))
		}
	# Plot l.Lambda vs. Lambda
	plot(lambda, l.lambda, 
		xlab = "Lambda", 
		ylab = "l(lambda)", 
		main = "Box-Cox Analysis")
	# Mark Best Lambda for Transformation
	l.max <- max(l.lambda)
	for (i in 1:m) {
		if (l.lambda[i] == l.max) abline(v = lambda[i])
		}
	}

chi.square.plot <- function(data.matrix){
	# This function constructs a Chi-square plot for assessing 
	# multivariate normality among several variables.

	X <- as.matrix(data.matrix)

	# number of observations (n) and variables (p)
	n <- length(X[,1])
	p <- length(X[1,])

	# vector of sample means and sample covariance matrix
	x.bar <- colMeans(X)
	S <- var(X)

	S.inverse <- solve(S)
	one.n <- matrix(rep(1, n), ncol = 1)
	x.center <- (diag(n) - (one.n %*% t(one.n)/n)) %*% X

	# statistical distances
	d.sq <- diag(x.center %*% S.inverse %*% t(x.center))
	d.squared <- sort(d.sq)
	
	# Chi-square plot
	pp <- (seq(1, n) - 0.5)/n
	qc <- qchisq(pp, p)
	
	plot(qc, d.squared, 
		xlab = "Theoretical Quantiles", 
		ylab = "Statistical Distances (d.squared)",
		main = "Chi-Square Plot")
	
	y <- c(1, 2)
	x <- c(1, 2)
	line <- lm(y~x)
	abline(line)
	}


hotelling <- function(X, mu0, alpha){
	# This function performs Hotelling's T-squared test
	# X is the data matrix; mu0 is the null hypothesized mean
	# vector; alpha is the significance level
	
	# number of observations (n) and variables (p)
	n <- length(X[,1])
	p <- length(X[1,])
	conf.level <- 1 - alpha

	# vector of sample means and sample covariance matrix
	x.bar <- colMeans(X)
	S <- var(X)

	S.inverse <- solve(S)
	
	# Hotelling's T-squared test
	t.sq <- n * t(x.bar - mu0) %*% S.inverse %*% (x.bar - mu0)
	
	# critical value for test
	crit <- qf(conf.level, p, (n-p)) * (p*(n-1) / (n-p))
	
	# P-value
	ts <- t.sq * (n-p) / (p*(n-1))
	p.val <- pf(ts, p, (n-p), lower.tail = FALSE)
	
	# print results
	cat("\n \t \t HOTELLING'S T-SQUARE TEST \n \n", 
		"Test statistic: \t \t", t.sq[1,1], "\n",
		"Critical value: \t \t", crit, "\n", 
		"Significance level: \t", alpha, "\n \n", 
		"P-value:", p.val, "\n")
	}

hotelling.ci <- function(X, a, alpha){
	# This function calculates T.squared simultaneous 
	# confidence intervals for any linear combination of means
	# X is the data matrix; a is the linear combination of means;
	# alpha is the significance level
	
	X <- as.matrix(X)
	a <- cbind(a)
	# number of observations (n) and variables (p)
	n <- length(X[,1])
	p <- length(X[1,])
	conf.level <- 1 - alpha

	# center of ellipsoid
	x.bar <- colMeans(X)
	
	S <- cov(X)
	
	# critical value for test
	crit <- qf(conf.level, p, (n-p)) * (p*(n-1) / (n*(n-p)))

	pivot <- t(a) %*% x.bar
	margin <- sqrt(crit * t(a) %*% S %*% a)

	cat("\n \t \t HOTELLING'S T-SQUARE SIMULTANEOUS CONFIDENCE INTERVAL \n \n", 
		"Confidence Interval: \t", pivot - margin, pivot + margin, "\n",
		"Confidence level: \t \t", conf.level, "\n")
	}

bonferroni.ci <- function(X, total.alpha){
	# This function calculates simultaneous confidence intervals
	# using bonferroni correction.
	# X is the data matrix; total.alpha is the family-wise alpha
	
	n <- length(X[,1])
	p <- length(X[1,])
	alpha.bon <- total.alpha / p
	conf.level <- 1 - alpha.bon
	
	x.bar <- colMeans(X)
	S <- cov(X)
	
	cat("\n \t \t BONFERRONI SIMULTANEOUS CONFIDENCE INTERVALS \n \n") 
	
	for(i in 1:p){
		pivot <- x.bar[i]
		margin <- qt(1 - alpha.bon/2, (n-1)) * sqrt(S[i,i] / n)
		
		cat("Confidence interval for the mean of variable", i, "is:", 
			"\t", pivot - margin, pivot + margin, "\n \n")
		}
	cat("The confidence level for each interval is", conf.level, " with the Bonferroni correction for", p, "intervals.  The family-wise confidence level is", 1 - total.alpha, "for the Bonferroni simultaneous confidence intervals. \n \n")
	}
	
two.sample.Tsquare <- function(X1, X2, mu0, alpha, pooled = TRUE){
	# This function tests for a difference in means between two
	# independent populations.
	# X1 and X2 are the data matrices for the two populations; 
	# mu0 is the null hypothesized mean vector; alpha is the significance 
	# level for testing.
	
	mu0 <- matrix(mu0, ncol = 1)
	conf.level <- 1 - alpha	
	p <- length(X1[1,])
	n1 <- length(X1[,1]); 
	n2 <- length(X2[,2]);
	x.bar1 <- matrix(colMeans(X1), ncol = 1);
	x.bar2 <- matrix(colMeans(X2), ncol = 1);
	S1 <- cov(X1);
	S2 <- cov(X2);
	
	if(pooled == TRUE){
		S.pooled <- (((n1 - 1) * S1) + ((n2 - 1) * S2)) / (n1 + n2 - 2)
		S.pooled.inv <- solve(((1/n1) + (1/n2)) * S.pooled)
		
		T.sq <- t((x.bar1 - x.bar2) - mu0) %*% S.pooled.inv %*% ((x.bar1 - x.bar2) - mu0)
		crit <- qf(conf.level, 2, (n1 + n2 - p - 1)) * (p * (n1 + n2 - 2)) / (n1 + n2 - p - 1)
		
		ts <- T.sq * (n1 + n2 - p - 1) / (p * (n1 + n2 - 2))
		p.val <- pf(ts, p, (n1 + n2 - p - 1), lower.tail = FALSE)
		
		# print results
		cat("\n \t \t TWO-SAMPLE TEST for MULTIVARIATE DATA\n (based on Hotelling's T-square using pooled sample covariance matrix) \n \n", 
			"Test statistic: \t \t", T.sq[1,1], "\n",
			"Critical value: \t \t", crit, "\n", 
			"Significance level: \t", alpha, "\n \n", 
			"P-value:", p.val, "\n \n ")
		}
	if(pooled == FALSE){
		print("Function under construction.")
		}
	}

two.sample.hotelling.ci <- function(X1, X2, a, alpha){
	# This function simultaneous confidence intervals for testing two
	# independent populations.
	# X1 and X2 are the data matrices for the two populations; a is any 
	# linear combination of the means; alpha is significance level
	
	a <- matrix(a, ncol = 1)
	conf.level <- 1 - alpha	
	p <- length(X1[1,])
	n1 <- length(X1[,1]); 
	n2 <- length(X2[,2]);
	x.bar1 <- matrix(colMeans(X1), ncol = 1);
	x.bar2 <- matrix(colMeans(X2), ncol = 1);
	S1 <- cov(X1);
	S2 <- cov(X2);
	
	S.pooled <- (((n1 - 1) * S1) + ((n2 - 1) * S2)) / (n1 + n2 - 2)
	crit <- qf(conf.level, 2, (n1 + n2 - p - 1)) * (p * (n1 + n2 - 2)) / (n1 + n2 - p - 1)
	
	pivot <- t(a) %*% (x.bar1 - x.bar2)
	margin <- sqrt(crit * t(a) %*% (((1/n1) + (1/n2)) * S.pooled) %*% a)

	# print results
	cat("\n \t TWO-SAMPLE SIMULTANEOUS CONFIDENCE INTERVALS for MULTIVARIATE DATA\n (based on Hotelling's T-square using pooled sample covariance matrix) \n \n", 
		"Confidence Interval: \t", pivot - margin, pivot + margin, "\n",
		"Confidence level: \t \t", conf.level, "\n \n") 
	}

single.contrast <- function(X, contrast, alpha){
	# The purpose of this function is to calculate 
	# simultaneous 100(1-alpha)% confidence intervals 
	# for a single contrast [Johnson p. 281]
	# X is the data matrix; contrast is the contrast; alpha
	# is the significance level

	conf.level <- 1 - alpha
	n <- length(X[,1])
	q <- length(X[1,])
	x.bar <- colMeans(X)
	S <- cov(X)
	
	pivot <- t(contrast) %*% x.bar
	half.width <- sqrt(qf(conf.level, (q-1), (n-q+1)) * (t(contrast) %*% S %*% contrast) * ((n-1)*(q-1)) / (n*(n-q+1)))
	
	cat("\n \t SIMULTANEOUS CONFIDENCE INTERVAL for SINGLE CONTRAST \n \n", 
		"Confidence Interval: \t", pivot - half.width, pivot + half.width, "\n",
		"Confidence level: \t \t", conf.level, "\n") 
	}

repeated.measures.test <- function(X, C, alpha){
	# this function performs a test for equality of treatments
	# in a repeated measures design [Johnson, 280].
	# X is the data matrix; C is the contrast matrix; alpha
	# is the significance level for testing
	
	conf.level <- 1 - alpha
	n <- length(X[,1])
	q <- length(X[1,])
	x.bar <- colMeans(X)
	S <- cov(X)
	
	CSC.inv <- solve(C %*% S %*% t(C))
	
	# Hotelling's T-squared statistic
	t.sq <- n * t(C %*% x.bar) %*% CSC.inv %*% (C %*% x.bar)
	
	# critical value for test
	crit <- qf(conf.level, (q-1), (n-q+1)) * ((q-1)*(n-1) / (n-q+1))
	
	# P-value
	ts <- t.sq * (n-q+1) / ((q-1)*(n-1))
	p.val <- pf(ts, (q-1), (n-q+1), lower.tail = FALSE)
	
	cat("\n \t TEST for EQUALITY of MEANS in a REPEATED MEASURES DESIGN \n \t \t \t (based on Hotelling's T-square) \n \n", 
		"Test statistic: \t \t", t.sq[1,1], "\n",
		"Critical value: \t \t", crit, "\n", 
		"Significance level: \t", alpha, "\n \n", 
		"P-value:", p.val, "\n \n",
		"Contrast matrix used for testing: \n")
	print(C)
	cat("\n \n")
	}

univariate.pi <- function(fit, y, Z, z0, alpha){
	# This function calculates 100(1-alpha)% prediction
	# intervals for univariate responses (p.379).
	#

	z0 <- matrix(z0, ncol = 1)
	level <- 1 - alpha/2
	n <- length(y)
	r <- ncol(Z)

	beta.hat <- fit$coef
	s.sq <- t(y - Z %*% beta.hat) %*% (y - Z %*% beta.hat) / (n-r-1)

	pivot <- t(z0) %*% beta.hat
	half.width <- qt(level, n-r-1) * sqrt((1 + t(z0) %*% solve(t(Z) %*% Z) %*% z0) * s.sq)
	lower <- pivot - half.width
	upper <- pivot + half.width

	cat("\n", "\t", 100*(1-alpha), "% Prediction Interval \n",  
		"\n PI:", lower, ",", upper,
		"\n z0 =", z0, "\n " )
	}

pred.ellipse <- function(mfit,Y, Z, z0, alpha){
	
	# This function calculates and displays 100(1-alpha)% 
	# prediction ellipse for bivariate multiple regressions
	# (p. 399).
	#
	
	require(ellipse)

	level <- 1 - alpha
	n <- nrow(Y)
	m <- ncol(Y)
	r <- ncol(Z)

	n.sigma.hat <- SSD(mfit)$SSD
	c.sq <- qf(level, m, (n-r-m)) * (m * (n-r-1)) / (n-r-m)
	C <- (1 + t(z0) %*% solve(t(Z) %*% Z) %*% z0) * c.sq
	beta.hat <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
	yhat <- t(z0) %*% beta.hat
	
	M <- n.sigma.hat / (n-r-1)

	pts <- ellipse(M, centre = yhat, t = sqrt(C))
	plot(pts, type = "l", xlab = expression(Y[1]), ylab = expression(Y[2]),
			main = "Prediction Ellipse")

	}


