#################################################
## Forest Structure Validation Experiment Code ##
#################################################

source('sim_function.R')

#Run models

ba.jabowa <- forest.sim(model='jabowa', nreps=30, nyears=122, burnin=1, 
                                       harvests=c('clearcut'),seedlings='none',
                                       acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                    disperse.eaten=0.704,cache.prob=0.288,
                                                    undisperse.eaten=0.538),
                                       mastscenario="hee")

ba.none <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                        harvests=c('clearcut'),seedlings='none',
                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                     disperse.eaten=0.704,cache.prob=0.288,
                                     undisperse.eaten=0.538),
                        mastscenario="hee")

ba.simple <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                      harvests=c('clearcut'),seedlings='simple',
                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                   disperse.eaten=0.704,cache.prob=0.288,
                                   undisperse.eaten=0.538),
                      mastscenario="hee")

ba.hee <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                      harvests=c('clearcut'),seedlings='hee',
                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                   disperse.eaten=0.704,cache.prob=0.288,
                                   undisperse.eaten=0.538),
                      mastscenario="hee")

save('ba.jabowa','ba.none','ba.simple','ba.hee',file='data/structure_val_figure.Rdata')
