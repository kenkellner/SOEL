##Comparison of different drought scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
drought.prob2 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           seed.scenario = "randomdrought",
                           drought.prob = 0.2,
                           force.processors = 6)
save(drought.prob2,file='output/drought_prob2.Rdata')
drought.prob4 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                            burnin=20,nyears=40,
                            seed.scenario = "randomdrought",
                            drought.prob = 0.4,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob4,file='output/drought_prob4.Rdata')
drought.prob6 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                            burnin=20,nyears=40,
                            seed.scenario = "randomdrought",
                            drought.prob = 0.6,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob6,file='output/drought_prob6.Rdata')
drought.prob8 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                            burnin=20,nyears=40,
                            seed.scenario = "randomdrought",
                            drought.prob = 0.8,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob8,file='output/drought_prob8.Rdata')
drought.prob10 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                            burnin=20,nyears=40,
                            seed.scenario = "randomdrought",
                            drought.prob = 1,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob10,file='output/drought_prob10.Rdata')
drought.prob0 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                             burnin=20,nyears=40,
                             seed.scenario = "randomdrought",
                             drought.prob = 0,
                             force.processors = 12,
                             ram.max = 5000)
save(drought.prob0,file='output/drought_prob0.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Drought experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')

