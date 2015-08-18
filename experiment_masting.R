
source('sim_function.R')

mast.average <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                    burnin=20,nyears=40,
                    mast.scenario='fixedaverage')
save(mast.average,file='output/mast_average.Rdata')
mast.good <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                           burnin=20,nyears=40,
                           mast.scenario='fixedgood')
save(mast.good,file='output/mast_good.Rdata')
mast.bad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                        burnin=20,nyears=40,
                        mast.scenario='fixedbad')
save(mast.bad,file='output/mast_bad.Rdata')
mast.hee <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='hee')
save(mast.hee,file='output/mast_hee.Rdata')
mast.random <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='random')
save(mast.random,file='output/mast_random.Rdata')
mast.priorgood <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorgood')
save(mast.priorgood,file='output/mast_priorgood.Rdata')
mast.priorbad <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=30,
                       burnin=20,nyears=40,
                       mast.scenario='priorbad')
save(mast.priorbad,file='output/mast_priorbad.Rdata')


library(RPushbullet)
pbPost('note','Analysis Complete','Simple mast analysis on rbrutus32 complete',devices='Nexus 6')
