##Comparison of different herbivory scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
browse.hee <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                            burnin=20,nyears=40,
                            browse.scenario = "hee",
                            force.processors = 10)
save(browse.hee,file='output/browse_hee.Rdata')
browse.prob0 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                         burnin=20,nyears=40,
                         browse.scenario = "custom",
                         prob.browsed = 0,
                         force.processors = 10)
save(browse.prob0,file='output/browse_prob0.Rdata')
browse.prob2 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           browse.scenario = "custom",
                           prob.browsed = 0.2,
                           force.processors = 10)
save(browse.prob2,file='output/browse_prob2.Rdata')
browse.prob4 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           browse.scenario = "custom",
                           prob.browsed = 0.4,
                           force.processors = 10)
save(browse.prob4,file='output/browse_prob4.Rdata')
browse.prob6 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           browse.scenario = "custom",
                           prob.browsed = 0.6,
                           force.processors = 10)
save(browse.prob6,file='output/browse_prob6.Rdata')
browse.prob8 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           browse.scenario = "custom",
                           prob.browsed = 0.8,
                           force.processors = 10)
save(browse.prob8,file='output/browse_prob8.Rdata')
browse.prob10 <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           browse.scenario = "custom",
                           prob.browsed = 1.0,
                           force.processors = 10)
save(browse.prob10,file='output/browse_prob10.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Browse experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')