
source('sim_function.R')

library(RPushbullet)

#24 reps with 8 processors works

mast.average <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                    burnin=20,nyears=40,
                    mast.scenario='fixedaverage', force.processors = 15)
save(mast.average,file='output/mast_average.Rdata')
pbPost('note','first phase complete','nothing here',devices='Nexus 6')
mast.good <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           mast.scenario='fixedgood', force.processors = 15)
save(mast.good,file='output/mast_good.Rdata')
mast.bad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                        burnin=20,nyears=40,
                        mast.scenario='fixedbad', force.processors = 15)
save(mast.bad,file='output/mast_bad.Rdata')
mast.hee <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='hee', force.processors = 15)
save(mast.hee,file='output/mast_hee.Rdata')
mast.random <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='random', force.processors = 15)
save(mast.random,file='output/mast_random.Rdata')
mast.priorgood <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorgood', force.processors = 15)
save(mast.priorgood,file='output/mast_priorgood.Rdata')
mast.priorbad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorbad', force.processors = 15)
save(mast.priorbad,file='output/mast_priorbad.Rdata')


pbPost('note','Analysis Complete','Simple mast analysis on rbrutus32 complete',devices='Nexus 6')
