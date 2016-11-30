##Comparison of different seed predation scenarios

source('run_SOEL.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
weevil.dispersal.average <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=30,nyears=40,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.dispersal.average,file='output/predation/weevil_dispersal_average.Rdata')

weevil.dispersal.yearlyeff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-diff',
                                        dispersal.scenario = "yearly-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.yearlyeff,file='output/predation/weevil_dispersal_yearlyeff.Rdata')

weevil.dispersal.treateff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'treat-diff',
                                        dispersal.scenario = "treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treateff,file='output/predation/weevil_dispersal_treateff.Rdata')

weevil.dispersal.treatyearlyeff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-treat-diff',
                                        dispersal.scenario = "yearly-treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treatyearlyeff,file='output/predation/weevil_dispersal_treatyearlyeff.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)

setwd('output/predation')
zip('predation.zip',files=list.files())
setwd('../..')

pbPost('note','Analysis Complete',
       paste('Predation experiment on rbrutus16 complete after',runtime,'minutes. Sending files and shutting down instance.'),
       devices='Nexus 6')

pbPost('file',url='output/predation/predation.zip')

#Shut down instance 
system('sudo shutdown -h now')

##################################################################

#Test for differences among scenarios

source('utility_functions.R')

lapply(c('output/predation/weevil_dispersal_average.Rdata',
         'output/predation/weevil_dispersal_treateff.Rdata',
         'output/predation/weevil_dispersal_treatyearlyeff.Rdata',
         'output/predation/weevil_dispersal_yearlyeff.Rdata'
         ),
       load,.GlobalEnv)

datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,37)
datalist <- add.pctgermmean(datalist,30,37)

##Total acorns produced
test = analyze.ibm(datalist,metric='acornsum')
summary(test$anova)
test$anova.mc

#Total New Seedlings over 10 Years
test = analyze.ibm(datalist,metric='seedlingsum')

#Overall harvest effect
newseed <- gen.dataset(datalist,metric='seedlingsum')
nm <- mean(newseed$seedlingsum[newsap$harvest=='none'])
mm <- mean(newseed$seedlingsum[newsap$harvest=='shelterwood'])
(nm-mm)/nm

#Effect of TE in midstory removal
nm <- mean(newseed$seedlingsum[newseed$harvest=='shelterwood'&newseed$scenario=='avg'])
mm <- mean(newseed$seedlingsum[newseed$harvest=='shelterwood'&newseed$scenario=='trt'])
(mm-nm)/nm

#Yearly effects
nm <- mean(newseed$seedlingsum[newseed$scenario=='avg'])
mm <- mean(newseed$seedlingsum[newseed$scenario=='treat.yrly'])
(mm-nm)/nm

##Saplings
test = analyze.ibm(datalist,metric='seedorigin',37)

#Overall harvest effects
newsap <- gen.dataset(datalist,metric='seedorigin',37)
mean(newsap$seedorigin[newsap$harvest=='none'])
mean(newsap$seedorigin[newsap$harvest=='shelterwood'])

#Treatment effects in midstory removal
nm <- mean(newsap$seedorigin[newsap$scenario=='avg'])
mm <- mean(newsap$seedorigin[newsap$scenario=='treat.yrly'])
(mm-nm)/nm


#######
#Pct Germ
test = analyze.ibm(datalist,metric='pctgermmean')

#Overall harvest effects
newgerm <- gen.dataset(datalist,metric='pctgermmean')
nm <- mean(newgerm$pctgermmean[newsap$harvest=='none'])
mm <- mean(newgerm$pctgermmean[newsap$harvest=='shelterwood'])
(nm-mm)/nm

#Treatment effects in midstory removal
nm <- mean(newgerm$pctgermmean[newgerm$harvest=='shelterwood'&newgerm$scenario=='avg'])
mm <- mean(newgerm$pctgermmean[newgerm$harvest=='shelterwood'&newgerm$scenario=='trt.yearly'])
(mm-nm)/nm

#Yearly effects
nm <- mean(newgerm$pctgermmean[newgerm$scenario=='avg'])
mm <- mean(newgerm$pctgermmean[newgerm$scenario=='treat.yrly'])
(mm-nm)/nm

