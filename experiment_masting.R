##Comparison of different masting scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
mast.average <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                    burnin=20,nyears=40,
                    mast.scenario='fixedaverage', force.processors = 10)
save(mast.average,file='output/mast_average.Rdata')
pbPost('note','first phase complete','nothing here',devices='Nexus 6')
mast.good <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           mast.scenario='fixedgood', force.processors = 10)
save(mast.good,file='output/mast_good.Rdata')
mast.bad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                        burnin=20,nyears=40,
                        mast.scenario='fixedbad', force.processors = 10)
save(mast.bad,file='output/mast_bad.Rdata')
mast.hee <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='hee', force.processors = 10)
save(mast.hee,file='output/mast_hee.Rdata')
mast.random <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='random', force.processors = 10)
save(mast.random,file='output/mast_random.Rdata')
mast.priorgood <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorgood', force.processors = 10)
save(mast.priorgood,file='output/mast_priorgood.Rdata')
mast.priorbad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorbad', force.processors = 10)
save(mast.priorbad,file='output/mast_priorbad.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Mast experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance
system('sudo shutdown -h now') 
