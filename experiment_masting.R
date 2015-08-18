
source('sim_function.R')
library(notifyR)

notifyR(

mast.average <- forest.sim(xcorewidth=100, ycorewidth=100, nreps=1,
                    burnin=5,nyears=10,
                    mast.scenario='fixedaverage',force.processors=1)

,

email = 'ken.kellner@gmail.com'

)