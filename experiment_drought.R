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

lapply(c('output/drought_prob0.Rdata','output/drought_prob2.Rdata','output/drought_prob4.Rdata',
         'output/drought_prob6.Rdata','output/drought_prob8.Rdata','output/drought_prob10.Rdata'),
         load,.GlobalEnv)

source('utility_functions.R')

datalist = list(drought.prob0=drought.prob0,drought.prob2=drought.prob2,drought.prob4=drought.prob4,
                drought.prob6=drought.prob6,drought.prob8=drought.prob8,drought.prob10=drought.prob10)

gen.figures(datalist,'seedclass123',25,ylim=c(1000,6000),cont=TRUE,vals=c(0,0.2,0.4,0.6,0.8,1),singleplot=T)
gen.figures(datalist,'seedclass4',25,ylim=c(0,500),cont=TRUE,vals=c(0,0.2,0.4,0.6,0.8,1),singleplot=T)

out <- analyze.ibm(datalist,'clearcut','seedclass4',25,cont=TRUE,vals=c(0,0.2,0.4,0.6,0.8,1))
summary(out)