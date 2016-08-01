model {
  
  for (i in 1:nyears){
    year.eff[i] ~ dnorm(0,year.tau)
  }
  
  for (i in 1:ntrees){
    
    #tree.eff[i] ~ dnorm(0,tree.tau)
    
    for (j in 1:nyears){
    
      log(dm[i,j]) <- grand.mean 
        + year.eff[j]
        #+ tree.eff[i]
        #+ b.shelter*shelter[i,j] 
        #+ b.species*species[i] 
    
      #lam[i,j] <- pow((1/dm[i,j]),1)
      
      #prod[i,j] ~ dweib(1,lam[i,j])
      
      prod[i,j] ~ dexp(1/dm[i,j])
    
    }
  }
  
  
  #priors
  
  #tree.sd ~ dunif(0,100)
  #tree.tau <- pow(tree.sd,-2)
  
  grand.mean ~ dunif(-10,10)
  
  year.sd ~ dunif(0,100)
  year.tau <- pow(year.sd,-2)
  
  #b.shelter ~ dnorm(0,0.01)
  
  #b.species ~ dnorm(0,0.01)

  
}