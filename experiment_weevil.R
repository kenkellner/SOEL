##Comparison of different weevil scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results

weevil.prob0 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           weevil.scenario = "custom",
                           prob.weevil = 0,
                           force.processors = 12,
                           ram.max = 5000)
save(weevil.prob0,file='output/weevil_prob0.Rdata')
weevil.prob2 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           weevil.scenario = "custom",
                           prob.weevil = 0.2,
                           force.processors = 12,
                           ram.max = 5000)
save(weevil.prob2,file='output/weevil_prob2.Rdata')
weevil.prob4 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           weevil.scenario = "custom",
                           prob.weevil = 0.4,
                           force.processors = 12,
                           ram.max = 5000)
save(weevil.prob4,file='output/weevil_prob4.Rdata')
weevil.prob6 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           weevil.scenario = "custom",
                           prob.weevil = 0.6,
                           force.processors = 12,
                           ram.max = 5000)
save(weevil.prob6,file='output/weevil_prob6.Rdata')
weevil.prob8 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                           burnin=20,nyears=40,
                           weevil.scenario = "custom",
                           prob.weevil = 0.8,
                           force.processors = 12,
                           ram.max = 5000)
save(weevil.prob8,file='output/weevil_prob8.Rdata')
weevil.prob10 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                            burnin=20,nyears=40,
                            weevil.scenario = "custom",
                            prob.weevil = 1.0,
                            force.processors = 12,
                            ram.max = 5000)
save(weevil.prob10,file='output/weevil_prob10.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Weevil experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')