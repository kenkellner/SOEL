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

weevil.notreateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=20,nyears=30,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "hee",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.notreateff,file='output/predation/weevil_notreateff.Rdata')

dispersal.notreateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=20,nyears=30,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "hee",
                                dispersal.scenario = "yearly-diff",
                                force.processors = 12,
                                ram.max = 5000)
save(dispersal.notreateff,file='output/predation/dispersal_notreateff.Rdata')


weevil.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                              burnin=20,nyears=30,
                              harvests = c('none','shelterwood'),
                              mast.scenario = "hee",
                              weevil.scenario = "treat-diff",
                              force.processors = 12,
                              ram.max = 5000)
save(weevil.treateff,file='output/predation/weevil_treateff.Rdata')

dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                   burnin=20,nyears=30,
                                   harvests = c('none','shelterwood'),
                                   mast.scenario = "hee",
                                   weevil.scenario = "hee",
                                   dispersal.scenario = "treat-yearly-diff",
                                   force.processors = 12,
                                   ram.max = 5000)
save(dispersal.treateff,file='output/predation/dispersal_treateff.Rdata')

weevil.dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=20,nyears=30,
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'treat-diff',
                                        dispersal.scenario = "treat-yearly-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treateff,file='output/predation/weevil_dispersal_treateff.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Predation experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')