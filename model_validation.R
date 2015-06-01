###################################
## IBM Validation Working Script ##
###################################

source('sim_function.R')

#Stock JABOWA 3 model
jabowa.out <- forest.sim(model='jabowa', nreps=4, nyears=150, 
                         harvests=c('clearcut','shelterwood','singletree','none'))

#Spatially-explicit IBM with JABOWA regeneration process for all species
noseedlings.out <- forest.sim(model='ibm', nreps=4, nyears=150, 
                              harvests=c('clearcut','shelterwood','singletree','none'),seedlings='none')

#IBM with oak seed/seedling processes; seedling growth simplified based on lit
seedlingslit.out <- forest.sim(model='ibm', nreps=4, nyears=150, 
                               harvests=c('clearcut','shelterwood','singletree','none'),seedlings='simple')

#Same as above but with seedling growth based on HEE data
seedlings.out <- forest.sim(model='ibm', nreps=4, nyears=150, 
                               harvests=c('clearcut','shelterwood','singletree','none'),seedlings='hee')

#Basal area comparison plots
plot(rowMeans(jabowa.out$clear$BA),type='l',col='red',xlab="Year",ylab="Basal Area",lwd=2,
     main="Clearcut")
lines(rowMeans(noseedlings.out$clear$BA),type='l',col='orange',lwd=2)
lines(rowMeans(seedlingslit.out$clear$BA),type='l',col='green',lwd=2)
lines(rowMeans(seedlings.out$clear$BA),type='l',col='blue',lwd=2)
legend("bottomright",col=c('red','orange','green','blue'), lwd=2,
       legend=c('JABOWA','NoSeedlings','LitValues','HEEValues'))

plot(rowMeans(jabowa.out$none$BA),type='l',col='red',xlab="Year",ylab="Basal Area",lwd=2,
     main="No Harvest")
lines(rowMeans(noseedlings.out$none$BA),type='l',col='orange',lwd=2)
lines(rowMeans(seedlingslit.out$none$BA),type='l',col='green',lwd=2)
lines(rowMeans(seedlings.out$none$BA),type='l',col='blue',lwd=2)
legend("right",col=c('red','orange','green','blue'), lwd=2,
       legend=c('JABOWA','NoSeedlings','LitValues','HEEValues'))

plot(rowMeans(jabowa.out$single$BA),type='l',col='red',xlab="Year",ylab="Basal Area",lwd=2,
     main="Single Tree")
lines(rowMeans(noseedlings.out$single$BA),type='l',col='orange',lwd=2)
lines(rowMeans(seedlingslit.out$single$BA),type='l',col='green',lwd=2)
lines(rowMeans(seedlings.out$single$BA),type='l',col='blue',lwd=2)
legend("right",col=c('red','orange','green','blue'), lwd=2,
       legend=c('JABOWA','NoSeedlings','LitValues','HEEValues'))

plot(rowMeans(jabowa.out$shelter$BA),type='l',col='red',xlab="Year",ylab="Basal Area",lwd=2,
     main="Shelterwood")
lines(rowMeans(noseedlings.out$shelter$BA),type='l',col='orange',lwd=2)
lines(rowMeans(seedlingslit.out$shelter$BA),type='l',col='green',lwd=2)
lines(rowMeans(seedlings.out$shelter$BA),type='l',col='blue',lwd=2)
legend("right",col=c('red','orange','green','blue'), lwd=2,
       legend=c('JABOWA','NoSeedlings','LitValues','HEEValues'))