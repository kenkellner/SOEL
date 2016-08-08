#########################################
## Seedling Validation Experiment Code ##
#########################################

source('run_SOEL.R')

#Run model(s) for no harvest treatment

seedlingsval.out <- run.SOEL(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='hee',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")
seedlingsval.none.simple <- run.SOEL(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='simple',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")
seedlingsval.none.simple6 <- run.SOEL(model='ibm', nreps=30, nyears=40, 
                                       harvests=c('none'),seedlings='simple',maxgrowth=0.6,
                                       acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                    disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                       mastscenario="hee")
seedlingsval.none.none <- run.SOEL(model='ibm', nreps=30, nyears=40, 
                                        harvests=c('none'),seedlings='none',
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.none.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=40, 
                                     harvests=c('none'),seedlings='none',
                                     acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                  disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                     mastscenario="hee")

############################################################
#Run model(s) for clearcut treatment and format output

seedlingsval.clear <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                               harvests=c('clearcut'),seedlings='hee',
                                acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                mastscenario="hee")
seedlingsval.clear.simple <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('clearcut'),seedlings='simple',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.clear.simple6 <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                        harvests=c('clearcut'),seedlings='simple',maxgrowth=0.6,
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.clear.none <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                         harvests=c('clearcut'),seedlings='none',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")
seedlingsval.clear.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=30, burnin=20,
                                      harvests=c('clearcut'),seedlings='none',
                                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                   disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                      mastscenario="hee")

############################################################
#Run model(s) for shelterwood treatment and format output
seedlingsval.shelt <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('shelterwood'),seedlings='hee',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.shelt.simple <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('shelterwood'),seedlings='simple',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.shelt.simple6 <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                        harvests=c('shelterwood'),seedlings='simple',maxgrowth=0.6,
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.shelt.none <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                         harvests=c('shelterwood'),seedlings='none',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")
seedlingsval.shelt.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=30, burnin=20,
                                         harvests=c('shelterwood'),seedlings='simple',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")



#############################

#Save all model output
save('seedlingsval.out','seedlingsval.none.simple','seedlingsval.none.simple6',
     'seedlingsval.none.none','seedlingsval.none.jabowa','seedlingsval.clear',
     'seedlingsval.clear.simple','seedlingsval.clear.simple6','seedlingsval.clear.none',
     'seedlingsval.clear.jabowa','seedlingsval.shelt','seedlingsval.shelt.simple',
     'seedlingsval.shelt.simple6','seedlingsval.shelt.none','seedlingsval.shelt.jabowa',
     file='output/development/seedlings_val_figure.Rdata')

