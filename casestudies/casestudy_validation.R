###########################
## Validation Case Study ##
###########################

##############################################
## Seedling & Sapling Validation Case Study ##
##############################################

source('run_SOEL.R')

#No harvest treatment
seedlingsval.out <- run.SOEL(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='hee',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")
seedlingsval.none.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=40, 
                                     harvests=c('none'),seedlings='none',
                                     acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                  disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                     mastscenario="hee")

#Clearcut treatment
seedlingsval.clear <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                               harvests=c('clearcut'),seedlings='hee',
                                acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                mastscenario="hee")
seedlingsval.clear.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=30, burnin=20,
                                      harvests=c('clearcut'),seedlings='none',
                                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                   disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                      mastscenario="hee")

#Shelterwood/midstory removal treatment
seedlingsval.shelt <- run.SOEL(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('shelterwood'),seedlings='hee',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.shelt.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=30, burnin=20,
                                         harvests=c('shelterwood'),seedlings='simple',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")

############################################
## Forest Structure Validation Case Study ##
############################################

#SOEL, based on HEE data
ba.hee <- run.SOEL(model='ibm', nreps=30, nyears=122, burnin=1,
                   harvests=c('clearcut'),seedlings='hee',
                   acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                disperse.eaten=0.704,cache.prob=0.288,
                                undisperse.eaten=0.538),
                   mastscenario="hee")

#JABOWA
ba.jabowa <- run.SOEL(model='jabowa', nreps=30, nyears=122, burnin=1, 
                      harvests=c('clearcut'),seedlings='none',
                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                   disperse.eaten=0.704,cache.prob=0.288,
                                   undisperse.eaten=0.538),
                      mastscenario="hee")

#############################

#Save all model output
save('seedlingsval.out','seedlingsval.none.jabowa',
     'seedlingsval.clear','seedlingsval.clear.jabowa',
     'seedlingsval.shelt','seedlingsval.shelt.jabowa',
     'ba.hee','ba.jabowa',
     file='output/casestudy_validation.Rdata')

