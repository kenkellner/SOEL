#####################################################
## Estimate Acorn Dispersal Probability pDispersal ##
#####################################################

#Based on data from:
#Kellner, K. F., J. K. Riegel, and R. K. Swihart (2014). Effects of silvicultural disturbance 
#on acorn infestation and removal. New Forests 45:265–281. doi: 10.1007/s11056-014-9409-9

#Note that throughout this file (and in some of the SOEL code), 
#"acorn dispersal" is equivalent to "acorn removal"

#Read in data
rem = read.csv('data/ibm_removal.csv',header=T)

#Fit full model (binomial regression): 
#species + shelterwood treatment (TE) + effect of weevil status + random year effect (YE)
library(MASS)

rm <- glmmPQL(rem~species+shelter+weev,data=rem,family='binomial',random= ~ 1 | yearcode)
summary(rm)

#Test full model
remprob <- function(n, shelter,wo,weev){
  hold <- 0.519 - 0.162*wo + 0.129*shelter - 2.00*weev + rnorm(n,0,0.6322)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}
hist(remprob(100,1,1,1)) #distribution of pDispersal

