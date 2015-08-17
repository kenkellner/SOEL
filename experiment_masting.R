
source('sim_function.R')

mast.average <- forest.sim(xcorewidth=200, ycorewidth=200, nreps=2,
                    burnin=20,nyears=25,
                    mast.scenario='fixedaverage')