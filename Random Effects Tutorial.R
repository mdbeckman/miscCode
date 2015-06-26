rm(list = ls()) # clean up

#####################################
### Random/Mixed Effects Tutorial ###
#####################################
require(faraway)
require(lme4)

################################
### Faraway (2006) Chapter 8 ###
################################
 
data(pulp) # paper brightness by shift operator

## Fixed-effects one-way ANOVA (p. 156)
op <- options(contrasts = c("contr.sum", "contr.poly"))
lmod <- aov(bright ~ operator, pulp)
summary(lmod)

coef(lmod)
options(op)

## Random effects estimation from ANOVA model; eqn on p. 154
# variance of operator effects: (MSA - MSE) / n; where n = # obs per operator
(0.4467 - 0.1062) / 5  

## Maximum Likelihood Estimation using lme4 package (p. 157)
mmod <- lmer(bright ~ 1 + (1|operator), data = pulp) # fits random intercept for each operator; defaults to REML
summary(mmod)

# MLEs
smod <- lmer(bright ~ 1 + (1|operator), data = pulp, REML = FALSE) # fits random intercept for each operator; ML estimation
summary(smod) # note that the b/w subj variance (0.0482) is smaller with ML than REML so w/in subj variance (o.1118) is larger

## model comparison
nullmod <- lm(bright ~ 1, data = pulp)

# chisq method
as.numeric(2*(logLik(smod) - logLik(nullmod))) # 2.568
pchisq(2.568, 1, lower = FALSE) # p-value is large, but given concerns about assumptions etc; better to bootstrap

# bootstrap

nboot <- 1000
lrstat <- numeric(nboot)
for( i in 1:nboot){
  y <- unlist(simulate(nullmod))  # y <- simulate(nullmod); simulates responses under the null
  bnull <- lm(y ~ 1)
  balt <- lmer(y ~ 1 + (1|operator), data = pulp, REML = FALSE)
  lrstat[i] <- as.numeric(2*(logLik(balt) - logLik(bnull)))
}

mean(lrstat < 0.00001)  # too many near zero; use of chisq is probably strained

# bootstrapped p-value (p. 159)
p.val <- mean(lrstat > 2.5684); p.val
sqrt(p.val * (1-p.val) / nboot) # bootstrap SE of p.val



## Nested Effects ####

data(eggs)
summary(eggs)

# samples nested w/in Tech; Tech nested w/in Lab; Lab is random
cmod <- lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician) + (1|Lab:Technician:Sample), data = eggs)
summary(cmod) # note that s.d. is similar for lab (0.077), tech (0.084), and sample (0.055)

# VarCorr(cmod) # extracts variance components etc from lmer object (p. 172)


## Crossed Effects ####

data(abrasion)
summary(abrasion)

# fixed effects analysis (p.173)
lmod <- aov(wear ~ material + run + position, data = abrasion)
summary(lmod) # everything is sig, but run and position should be random effects

# mixed effects analysis (p.173)
mmod <- lmer(wear ~ material + (1|run) + (1|position), data = abrasion)
anova(mmod)
summary(mmod)











