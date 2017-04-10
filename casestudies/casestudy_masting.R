###############################################################
## Case study: interaction of good mast year(s) with harvest ##
###############################################################

source('run_SOEL.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
mast.average <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                       burnin=30,nyears=40,
                                       harvests = c('none','clearcut','shelterwood'),
                                       mast.scenario = "random",
                                       weevil.scenario = "fixedaverage",
                                       dispersal.scenario = "fixedaverage",
                                       force.processors = 12,
                                       ram.max = 5000)

mast.priorgood1.1SD <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                           burnin=30,nyears=40,
                           harvests = c('none','clearcut','shelterwood'),
                           mast.scenario = "priordifference",
                           prior.years = 1, mast.sd = 1,
                           weevil.scenario = "fixedaverage",
                           dispersal.scenario = "fixedaverage",
                           force.processors = 12,
                           ram.max = 5000)

mast.priorgood1.2SD <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 1, mast.sd = 2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)

mast.priorgood2.1SD <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = 1,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)

mast.priorgood2.2SD <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                  burnin=30,nyears=40,
                                  harvests = c('none','clearcut','shelterwood'),
                                  mast.scenario = "priordifference",
                                  prior.years = 2, mast.sd = 2,
                                  weevil.scenario = "fixedaverage",
                                  dispersal.scenario = "fixedaverage",
                                  force.processors = 12,
                                  ram.max = 5000)

save(mast.average,mast.priorgood1.1SD,mast.priorgood1.2SD,mast.priorgood2.1SD,mast.priorgood2.2SD,
     file='output/casestudy_masting.Rdata')

##################################################################################

end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Mast experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance
system('sudo shutdown -h now') 

##################################################

#Test for differences among scenarios

source('utility_functions.R')

load('output/casestudy_masting.Rdata')

datalist = list(pg11=mast.priorgood1.1SD,pg21=mast.priorgood2.1SD,pg12=mast.priorgood1.2SD,
                pg22=mast.priorgood2.2SD,pga=mast.average)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

#Seedlings
test = analyze.ibm(datalist,metric='seedclass123',year=30)
summary(test$anova)
test$anova.mc

#Saplings
test = analyze.ibm(datalist,metric='seedorigin',year=37)
summary(test$anova)
test$anova.mc

