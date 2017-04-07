##################################################
## Estimate Seedling Survival Probability pSurv ##
##################################################

#Based on data from:
#Kellner, K. F. and R. K. Swihart (2016). Timber harvest and drought interact to 
#impact oak seedling growth and survival in the Central Hardwood Forest. 
#Ecosphere 7: e01473. doi: 10.1002/ecs2.1473

#Read in raw data
ibm.survival <- read.csv('data/ibm_survival.csv',header=T)
canopy <- read.csv('data/ibm_canopy.csv',header=T)[,1]

nseedlings <- dim(ibm.survival)[1]
nsamples <- ibm.survival$nsamples
species <- ibm.survival$species
seed.plotcode <- ibm.survival$seed.plotcode
surv <- ibm.survival[,c(4:8)]
age <- ibm.survival[,c(9:12)]
is.sprout <- ibm.survival[,c(13:16)]

###############################

#Bundle data for JAGS

jags.data <- c('surv','nseedlings','nsamples'
               ,'seed.plotcode','species','is.sprout'
               ,'canopy','age')

################################

#JAGS model file
writeLines("
model {
  #Likelihood
  for (i in 1:nseedlings){
    for (j in 2:nsamples[i]){
      
      surv[i,j] ~ dbern(psi[i,j])
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- grand.mean
      + b.sprout*is.sprout[i,j-1]
      + b.species*species[i]
      + b.shade*canopy[seed.plotcode[i]]
      + b.age*age[i,j-1]
    }
  }
  
  #Priors
  grand.mean ~ dunif(-100,100)  
  b.age ~ dnorm(0,0.01)
  b.species ~ dnorm(0,0.01)
  b.shade ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
}
", con='model_seedling_survival.txt')

modFile <- 'model_seedling_survival.txt'

################################

#Parameters to save

params <- c('grand.mean','b.species','b.sprout'
            ,'b.shade','b.age')

################################

#Run analysis (all years drought and non-drought)
#Model: species + canopy cover + age
library(jagsUI)

surv.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

############

#First 2 years only (drought)
for (i in 1:length(nsamples)){
  if(nsamples[i]>3){nsamples[i] = 3}
}

surv12.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                        n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

#####################

#Years 3-4 only (non-drought)
#Read in raw data again
ibm.survival <- read.csv('data/ibm_survival.csv',header=T)
canopy <- read.csv('data/ibm_canopy.csv',header=T)[,1]

nseedlings <- dim(ibm.survival)[1]
species <- ibm.survival$species
seed.plotcode <- ibm.survival$seed.plotcode
surv <- ibm.survival[,c(4:8)]
age <- ibm.survival[,c(9:12)]
is.sprout <- ibm.survival[,c(13:16)]

keep <- which(surv[,3]==1)
surv <- surv[keep,3:5]
species <- species[keep]
nseedlings <- length(species)
age <- age[keep,3:4]
seed.plotcode <- seed.plotcode[keep]

nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
}

surv34.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                          n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)
