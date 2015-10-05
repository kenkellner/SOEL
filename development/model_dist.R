model {

  for (i in 1:ndispacorns){
    
    log(dm[i]) <- disp.mean 
    + disp.shelter*shelter[i] 
    #+ disp.species*species[i] 
    + disp.mast*mast[i]
    
    lam[i] <- pow((1/dm[i]),sh)
    
    dist[i] ~ dweib(sh,lam[i])
    
  }
  
  
#priors
  
disp.mean ~ dunif(-10,10)

disp.shelter ~ dnorm(0,0.01)

#disp.species ~ dnorm(0,0.01)

disp.mast ~ dnorm(0,0.01)

sh ~ dgamma(1.0,1.0E-3)

}