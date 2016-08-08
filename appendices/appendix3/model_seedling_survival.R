
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