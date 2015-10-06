##Comparison of different seed predation scenarios

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
weevil.dispersal.average <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=20,nyears=30,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.dispersal.average,file='output/predation/weevil_dispersal_average.Rdata')

weevil.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=20,nyears=30,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "yearly-diff",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.yearlyeff,file='output/predation/weevil_yearlyeff.Rdata')

dispersal.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=20,nyears=30,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "yearly-diff",
                                force.processors = 12,
                                ram.max = 5000)
save(dispersal.yearlyeff,file='output/predation/dispersal_yearlyeff.Rdata')


weevil.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                              burnin=20,nyears=30,
                              harvests = c('none','shelterwood'),
                              mast.scenario = "hee",
                              weevil.scenario = "treat-diff",
                              dispersal.scenario = "fixedaverage",
                              force.processors = 12,
                              ram.max = 5000)
save(weevil.treateff,file='output/predation/weevil_treateff.Rdata')

dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                   burnin=20,nyears=30,
                                   harvests = c('none','shelterwood'),
                                   mast.scenario = "hee",
                                   weevil.scenario = "fixedaverage",
                                   dispersal.scenario = "treat-diff",
                                   force.processors = 12,
                                   ram.max = 5000)
save(dispersal.treateff,file='output/predation/dispersal_treateff.Rdata')

weevil.dispersal.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=20,nyears=30,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-diff',
                                        dispersal.scenario = "yearly-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.yearlyeff,file='output/predation/weevil_dispersal_yearlyeff.Rdata')

weevil.dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=20,nyears=30,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'treat-diff',
                                        dispersal.scenario = "treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treateff,file='output/predation/weevil_dispersal_treateff.Rdata')

weevil.dispersal.treatyearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=20,nyears=30,
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