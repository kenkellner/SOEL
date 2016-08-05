
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