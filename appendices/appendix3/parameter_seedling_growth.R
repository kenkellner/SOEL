###################################################
## Estimate Seedling Survival Probability meanGr ##
###################################################

#Based on data from:
#Kellner, K. F. and R. K. Swihart (2016). Timber harvest and drought interact to 
#impact oak seedling growth and survival in the Central Hardwood Forest. 
#Ecosphere 7: e01473. doi: 10.1002/ecs2.1473

#Read in raw data
ibm.growth <- read.csv('data/ibm_growth.csv',header=T)
canopy <- read.csv('data/ibm_canopy.csv',header=T)[,1]

#Format for JAGS
nseedlings <- dim(ibm.growth)[1]
species <- ibm.growth$species
growth <- ibm.growth[,4:7]
nsamples <- ibm.growth$nsamples
seed.plotcode <- ibm.growth$seed.plotcode
browse <- ibm.growth[,8:11]

#Neglog transformation on growth data for better model fit
#Need to back-transform when generating growth values in SOEL
neglog <- function(x){return (sign(x)*log(abs(x)+1))}

growth <- apply(growth,c(1,2),neglog)

#Bundle data for JAGS
jags.data <- c('growth','nseedlings','nsamples','seed.plotcode',
               'canopy','browse','species')

################################

#JAGS model file
writeLines("
model {
  #Likelihood
  for (i in 1:nseedlings){
           
    seed.mean[i] ~ dnorm(0, seed.tau)
           
    for (j in 1:nsamples[i]){
      growth[i,j] ~ dnorm(mu[i,j],obs.tau)
      mu[i,j] <- grand.mean + seed.mean[i] 
                + b.browse*browse[i,j]
                + b.canopy*canopy[seed.plotcode[i]]
                + b.species*species[i]
    }
  }
           
  #Priors
  grand.mean ~ dunif(-100,100)
  seed.tau <- pow(seed.sd,-2)
  seed.sd ~ dunif(0,100)
  obs.tau <- pow(obs.sd,-2)
  obs.sd ~ dunif(0,100)
  b.canopy ~ dnorm(0,0.01)    
  b.species ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
}
", con='model_seedling_growth.txt')

modFile <- 'model_seedling_growth.txt'

################################

#Parameters to save

params <- c('seed.sd','obs.sd','grand.mean'
            ,'b.browse','b.canopy','b.species')

################################

#Run analysis on all years
#Model: browsed? + canopy cover + species + random seedling effect + random observation effect

library(jagsUI)

ibm.growth.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=TRUE)

save(ibm.growth.output,file="output/development/ibm_growth_output.Rda")

########################################################################################

#First 2 years only (drought years)

for (i in 1:length(nsamples)){
  if(nsamples[i]>2){nsamples[i] = 2}
}

ibm.growth12.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                            n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=TRUE)

save(ibm.growth12.output,file="output/ibm_growth12_output.Rda")

########################################################################################

#Second 2 years only (non-drought)

keep <- which(!is.na(growth[,3]))
growth <- growth[keep,3:4]
browse <- browse[keep,3:4]
seed.plotcode <- seed.plotcode[keep]
species <- species[keep]
nseedlings <- length(species)

nsamples <- vector(length=length(species))
for(i in 1:length(species)){
  if(is.na(growth[i,2])){
    nsamples[i] <- 1
  } else {nsamples[i] <- 2}
}

ibm.growth34.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                            n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=TRUE)

save(ibm.growth34.output,file="output/ibm_growth34_output.Rda")
