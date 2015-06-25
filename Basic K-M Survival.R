###################################################
### S+ Script for Basic Kaplan Meier Survival   ###
###################################################
###################################################
### Created by: Matt Beckman    ###
### Created on: FEB-23-2012 	  ###
### Last Modified: 	          	###
### Modified by: 		            ###
###################################

require(survival)

## Establish working directory ##
getwd()
setwd("H:/Spotfire S+/Project1")
list.files()

## Read Data Table ##
InputTable <- read.csv("KM dat3.csv", header = TRUE)

## Drop Observations with missing time to event ##
not.missing <- which(!is.na(InputTable[,1]))
InputTable <- as.data.frame(InputTable[not.missing,])


##############################################################
### Survival Curve for Single Population without Censoring ###
##############################################################

if(ncol(InputTable) == 1){
  
  foo <- Surv(InputTable[,1])
  out <- survfit(foo ~ 1)
  
  result <- summary(out)
  
  ## Extract results from survfit object for output table ##
  Time <- result$time;
  N.Risk <- result$n.risk;
  N.Events <- result$n.event;
  Survival <- result$surv;
  Std.Error <- result$std.err;
  Lower.95.CI <- result$lower;
  Upper.95.CI <- result$upper;
  
  
  ## Build output table ##
  OutputTable <- cbind(Time,N.Risk, N.Events, Survival, Std.Error, Lower.95.CI, Upper.95.CI)
  
}



###########################################################
### Survival Curve for Single Population with Censoring ###
###########################################################

if(ncol(InputTable) == 2){

  foo <- Surv(InputTable[,1], InputTable[,2])
  out <- survfit(foo ~ 1)
  
  result <- summary(out)
  
  ## Extract results from survfit object for output table ##
  Time <- result$time;
  N.Risk <- result$n.risk;
  N.Events <- result$n.event;
  Survival <- result$surv;
  Std.Error <- result$std.err;
  Lower.95.CI <- result$lower;
  Upper.95.CI <- result$upper;
  
  
  ## Build output table ##
  OutputTable <- cbind(Time,N.Risk, N.Events, Survival, Std.Error, Lower.95.CI, Upper.95.CI)
  
}



#####################################
### Survival Comparison of Groups ###
#####################################



if (ncol(InputTable) > 2){
    
  ## Calculate Survival ##
  foo <- Surv(InputTable[,1], InputTable[,2])
  out <- survfit(foo ~ InputTable[,-c(1, 2)])
  
  result <- summary(out)
  
  ## Extract results from survfit object for output table ##
  Time <- result$time;
  N.Risk <- result$n.risk;
  N.Events <- result$n.event;
  Survival <- result$surv;
  Std.Error <- result$std.err;
  Lower.95.CI <- result$lower;
  Upper.95.CI <- result$upper;
  Group <- result$strata;
  
  
  ## Build output table ##
  OutputTable <- cbind(Time,N.Risk, N.Events, Survival, Std.Error, Lower.95.CI, Upper.95.CI, Group)
  
  ## Cox Proportional Hazards Regression ## 
  cph <- summary(coxph(foo ~ InputTable[,-c(1, 2)])) 
}
