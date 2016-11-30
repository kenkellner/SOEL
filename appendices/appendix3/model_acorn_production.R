#BUGS model of acorn production

model {
  
  for (i in 1:nyears){
    year.eff[i] ~ dnorm(0,year.tau)
  }
  
  for (i in 1:ntrees){
    
    for (j in 1:nyears){
    
      log(dm[i,j]) <- grand.mean 
        + year.eff[j]
      
      prod[i,j] ~ dexp(1/dm[i,j])
    
    }
  }
  
  
  #priors
  
  grand.mean ~ dunif(-10,10)
  
  year.sd ~ dunif(0,100)
  year.tau <- pow(year.sd,-2)

}