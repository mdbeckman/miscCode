### Score confidence interval calculations

score.interval <- function(y, n, alpha){

crit.value <- qnorm(1-alpha/2)
crit.sq <- crit.value^2
p.hat <- y / n

pivot <- p.hat * (n / (n + crit.sq)) + 0.5 * (crit.sq / (n + crit.sq))
pivot.round <- round(pivot, 4)
margin <- crit.value * sqrt((1 / (n + crit.sq)) * ((p.hat * (1 - p.hat) * (n / (n + crit.sq))) + (0.25 * (crit.sq / (n + crit.sq)))))
margin.round <- round(margin, 4)
cat(100 * (1-alpha), "% confidence interval:", "\n", "\t", pivot.round, "+/-", margin.round, "\n", "\t", pivot.round + c(-1, 1) * margin.round, "\n")

}

##################################################
## Confidence intervals for Pocket Fill Results ##

a <- 0.05

# successful access to refill port on 1st needle stick

score.interval(44, 47, a) # SM2; Normal
score.interval(38, 48, a) # SMEL; Normal
score.interval(17, 24, a) # SM2; Deep
score.interval(15, 24, a) # SMEL; Deep

# aspiration of unsuccessful attempt on first needle stick

score.interval(4, 10, a) # SM2
score.interval(1, 19, a) # SMEL

# aspiration attempt given an unsuccessful needle stick (all 5 sticks attempts)

score.interval(2, 8, a) # SM2; Template; Normal
score.interval(2, 13, a) # SMEL; Template; Normal
score.interval(1, 13, a) # SM2; no Template; Normal
score.interval(0, 33, a) # SMEL; no Template; Normal
score.interval(6, 17, a) # SM2; Template; Deep
score.interval(0, 17, a) # SMEL; Template; Deep
score.interval(2, 8, a) # SM2; no Template; Deep
score.interval(3, 20, a) # SMEL; no Template; Deep


