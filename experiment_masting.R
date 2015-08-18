
source('sim_function.R')

mast.average <- forest.sim(xcorewidth=50, ycorewidth=50, nreps=2,
                    burnin=5,nyears=10,
                    mast.scenario='fixedaverage')


library(RPushbullet)
pbPost('note','testing','testing more text',devices='Nexus 6')
