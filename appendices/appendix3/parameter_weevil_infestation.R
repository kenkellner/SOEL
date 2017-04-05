#####################################################
## Estimate Weevil Infestation Probability pWeevil ##
#####################################################

#Based on data from:
#Kellner, K. F., J. K. Riegel, and R. K. Swihart (2014). Effects of silvicultural disturbance 
#on acorn infestation and removal. New Forests 45:265–281. doi: 10.1007/s11056-014-9409-9

#Read in weevil, production, and tree data
weevil = read.csv('data/ibm_weevils.csv', header=TRUE)
treedata = read.csv('data/ibm_treedata.csv',header=TRUE)
prod = read.csv('data/ibm_production.csv',header=TRUE)

#Summarize acorn production
mastbo <- 1/(colMeans(prod[treedata$species==0,2:10],na.rm=T)/0.34)
mastwo <- 1/(colMeans(prod[treedata$species==1,2:10],na.rm=T)/0.34)

#Generate observation-level covariates for mast availability and harvest treatment
mast = i.shelter = rep(0,dim(weevil)[1])
for (i in 1:dim(weevil)[1]){
  i.shelter[i] <- treedata$shelter[weevil$Treecode[i]]
  mast[i] <- mean(mastbo[weevil$Year[i]],mastwo[weevil$Year[i]])
}

#Fit mixed-effects weevil infestation model (logistic regression)
#Full model: acorn species + shelterwood treatment (TE) + mast availability, random year effect
#In SOEL, "year effect" (YE) is the combination of the random year effect and mast availability
library(MASS)
wi <- glmmPQL(Infested ~ Species + i.shelter + mast, data=weevil,family='binomial',
              random=~1|Year)
summary(wi)

#Test full model
weevprob <- function(n, shelter,wo,lambda){
  hold <- -1.08 - 0.8249*wo - 0.44380*shelter + 0.7324*lambda + rnorm(n,0,1.010)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}
hist(weevprob(100,0,0,0.15)) #Distribution of pWeevil