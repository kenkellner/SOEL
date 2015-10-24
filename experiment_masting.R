##Comparison of different masting scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
mast.average <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                       burnin=30,nyears=40,
                                       harvests = c('none','clearcut','shelterwood'),
                                       mast.scenario = "random",
                                       weevil.scenario = "fixedaverage",
                                       dispersal.scenario = "fixedaverage",
                                       force.processors = 12,
                                       ram.max = 5000)
save(mast.average,file='output/mast/mast_average.Rdata')

mast.priorgood1.1SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                           burnin=30,nyears=40,
                           harvests = c('none','clearcut','shelterwood'),
                           mast.scenario = "priordifference",
                           prior.years = 1, mast.sd = 1,
                           weevil.scenario = "fixedaverage",
                           dispersal.scenario = "fixedaverage",
                           force.processors = 12,
                           ram.max = 5000)
save(mast.priorgood1.1SD,file='output/mast/mast_priorgood1.1SD.Rdata')

mast.priorgood1.2SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 1, mast.sd = 2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorgood1.2SD,file='output/mast/mast_priorgood1.2SD.Rdata')

mast.priorgood2.1SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = 1,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorgood2.1SD,file='output/mast/mast_priorgood2.1SD.Rdata')

mast.priorgood2.2SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = 2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorgood2.2SD,file='output/mast/mast_priorgood2.2SD.Rdata')

##################################################################################

mast.priorbad1.1SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 1, mast.sd = -1,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorbad1.1SD,file='output/mast/mast_priorbad1.1SD.Rdata')

mast.priorbad1.2SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 1, mast.sd = -2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorbad1.2SD,file='output/mast/mast_priorbad1.2SD.Rdata')

mast.priorbad2.1SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = -1,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorbad2.1SD,file='output/mast/mast_priorbad2.1SD.Rdata')

mast.priorbad2.2SD <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = -2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)
save(mast.priorbad2.2SD,file='output/mast/mast_priorbad2.2SD.Rdata')

##################################################################################

end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Mast experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance
system('sudo shutdown -h now') 

##################################################

datalist = list(pg11=mast.priorgood1.1SD,pg21=mast.priorgood2.1SD,pg12=mast.priorgood1.2SD,
                pg22=mast.priorgood2.2SD,pga=mast.average)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

s <- gen.dataset(datalist,'seedclass123',30)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

test = aov(seedclass123~harvest*scenario,data=s)

TukeyHSD(test)

####

s <- gen.dataset(datalist,'seedclass4',37)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

test = aov(seedclass4~harvest*scenario,data=s)

TukeyHSD(test)

c('A','X','X','A','X','X','A','X','X','A','X','X','A','X','X')

