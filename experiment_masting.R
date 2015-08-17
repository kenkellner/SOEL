
source('sim_function.R')

mast.average <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=1,
                    burnin=20,nyears=40,
                    mast.scenario='fixedaverage')