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

#Reload data
lapply(c('output/mast_average.Rdata','output/mast_bad.Rdata','output/mast_good.Rdata',
         'output/mast_hee.Rdata','output/mast_priorbad.Rdata','output/mast_priorgood.Rdata',
         'output/mast_random.Rdata'),load,.GlobalEnv)

source('utility_functions.R')

datalist <- list(mast.good=mast.good,mast.average=mast.average,mast.bad=mast.bad,mast.hee=mast.hee,
                 mast.random=mast.random,mast.priorgood=mast.priorgood,mast.priorbad=mast.priorbad)

gen.figures(datalist,'seedclass123',25,c(0,11000),singleplot=T)
gen.figures(datalist,'seedclass4',25,c(0,150),singleplot=T)
gen.figures(datalist,'ba',25,c(0,35),singleplot=T)

out <- analyze.ibm(datalist,'clearcut','seedclass4',25)
summary(out$anova)
out$kruskal
out$anova.mc

