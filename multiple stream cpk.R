

## average nonconforming from process ##
p.hat.lsl <- mean(c(0.1, 0.0, 1.5, 0.0))/100
p.hat.usl <- mean(c(0.4, 0.8, 0.0, 0.0))/100

## z-score for % nonconforming from each stream ##
z.lsl <- abs(qnorm(p.hat.lsl))
z.usl <- abs(qnorm(p.hat.usl))

## empirical Cpk ##
emp.cpk <- min(c(z.lsl, z.usl))/3; 

emp.cpk

