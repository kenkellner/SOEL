###########################
## Validation Case Study ##
###########################

##############################################
## Seedling & Sapling Validation Case Study ##
##############################################

source('run_SOEL.R')
source('notify.R')

seedlingsval.soel <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                             burnin=30,nyears=40,
                             harvests = c('none','clearcut','shelterwood'),
                             seedlings = "hee",
                             seedling.scenario = "fixedaverage",
                             mast.scenario = "hee",
                             weevil.scenario = "custom",
                             prob.weevil = 0.31,
                             dispersal.scenario = "custom",
                             force.processors = 15,
                             ram.max = 4000)

seedlingsval.jabowa <- run.SOEL(model='jabowa',xcorewidth=140, ycorewidth=140, nreps=30,
                              burnin=30,nyears=40,
                              harvests = c('none','clearcut','shelterwood'),
                              seedlings = "none",
                              force.processors = 15,
                              ram.max = 4000)

#Notify with simplepush
notify("seedlings done")

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
save('seedlingsval.soel','seedlingsval.jabowa','ba.hee','ba.jabowa',
     file='output/casestudy_validation.Rdata')

#Notify with simplepush
notify("structure done")

