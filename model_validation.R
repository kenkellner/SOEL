
source('sim_function.R')

jabowa.out <- forest.sim(model='jabowa', nreps=4, nyears=150, 
                      harvests=c('clearcut','shelterwood','single-tree','none'))