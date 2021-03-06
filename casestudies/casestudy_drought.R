#############################################
## Case study: varying drought probability ##
#############################################

source('run_SOEL.R')
source('notify.R')

#Run experiment and save results

drought.average <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                        burnin=30,nyears=40,
                        harvests = c('none','clearcut','shelterwood'),
                        mast.scenario = "hee",
                        weevil.scenario = "fixedaverage",
                        dispersal.scenario = "fixedaverage",
                        seedling.scenario = "fixedaverage",
                        force.processors = 15,
                        ram.max = 4000)

drought.prob0 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                              burnin=30,nyears=40,
                              harvests = c('none','clearcut','shelterwood'),
                              mast.scenario = "hee",
                              weevil.scenario = "fixedaverage",
                              dispersal.scenario = "fixedaverage",
                              seedling.scenario = "randomdrought",
                              prob.drought = 0,
                              force.processors = 15,
                              ram.max = 4000)

drought.prob2 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.2,
                            force.processors = 15,
                            ram.max = 4000)

drought.prob4 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.4,
                            force.processors = 15,
                            ram.max = 4000)

drought.prob6 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.6,
                            force.processors = 15,
                            ram.max = 4000)

drought.prob8 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.8,
                            force.processors = 15,
                            ram.max = 4000)

drought.prob10 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=30,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 1.0,
                            force.processors = 15,
                            ram.max = 4000)

save(drought.prob0,drought.prob2,drought.prob4,drought.prob6,drought.prob8,drought.prob10,
     file='output/casestudy_drought.Rdata')

#Notify with simplepush
notify("drought done")

#Shut down instance 
system('sudo shutdown -h now')

###############################################################

#Analysis

source('utility_functions.R')

load('output/casestudy_drought.Rdata')

datalist = list(d00=drought.prob0,d02=drought.prob2,d04=drought.prob4,
                d06=drought.prob6,d08=drought.prob8,d10=drought.prob10)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

sa <- gen.dataset(datalist,'seedorigin',37)
sa$harvest <- as.factor(sa$harvest)
sa$harvest <- relevel(sa$harvest,ref='none')

dcont <- rep(0,dim(sa)[1])
dcont[sa$scenario=='d02'] = 0.2
dcont[sa$scenario=='d04'] = 0.4
dcont[sa$scenario=='d06'] = 0.6
dcont[sa$scenario=='d08'] = 0.8
dcont[sa$scenario=='d10'] = 1

#Saplings
test <- aov(seedorigin~harvest*dcont,data=sa)
summary.lm(test)

#Seedlings
sa <- gen.dataset(datalist,'seedlingsum')
sa$harvest <- as.factor(sa$harvest)
sa$harvest <- relevel(sa$harvest,ref='none')
test <- aov(seedlingsum~harvest*dcont,data=sa)
summary.lm(test)
