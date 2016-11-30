##Comparison of different drought scenarios given average conditions otherwise

source('run_SOEL.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results

drought.average <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                        burnin=30,nyears=40,
                        harvests = c('none','clearcut','shelterwood'),
                        mast.scenario = "hee",
                        weevil.scenario = "fixedaverage",
                        dispersal.scenario = "fixedaverage",
                        seedling.scenario = "fixedaverage",
                        force.processors = 12,
                        ram.max = 5000)
save(drought.average,file='output/drought/drought_average.Rdata')

drought.prob0 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                              burnin=30,nyears=40,
                              harvests = c('none','clearcut','shelterwood'),
                              mast.scenario = "hee",
                              weevil.scenario = "fixedaverage",
                              dispersal.scenario = "fixedaverage",
                              seedling.scenario = "randomdrought",
                              prob.drought = 0,
                              force.processors = 12,
                              ram.max = 5000)
save(drought.prob0,file='output/drought/drought_prob0.Rdata')


drought.prob2 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.2,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob2,file='output/drought/drought_prob2.Rdata')

drought.prob4 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.4,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob4,file='output/drought/drought_prob4.Rdata')

drought.prob6 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.6,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob6,file='output/drought/drought_prob6.Rdata')

drought.prob8 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.8,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob8,file='output/drought/drought_prob8.Rdata')

drought.prob10 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "fixedaverage",
                            dispersal.scenario = "fixedaverage",
                            seedling.scenario = "randomdrought",
                            prob.drought = 1.0,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.prob10,file='output/drought/drought_prob10.Rdata')

drought.varying.prob0 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','clearcut','shelterwood'),
                                    mast.scenario = "hee",
                                    weevil.scenario = "yearly-treat-diff",
                                    dispersal.scenario = "yearly-treat-diff",
                                    seedling.scenario = "randomdrought",
                                    prob.drought = 0,
                                    force.processors = 12,
                                    ram.max = 5000)
save(drought.varying.prob0,file='output/drought/drought_varying_prob0.Rdata')

drought.varying.prob2 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                            burnin=30,nyears=40,
                            harvests = c('none','clearcut','shelterwood'),
                            mast.scenario = "hee",
                            weevil.scenario = "yearly-treat-diff",
                            dispersal.scenario = "yearly-treat-diff",
                            seedling.scenario = "randomdrought",
                            prob.drought = 0.2,
                            force.processors = 12,
                            ram.max = 5000)
save(drought.varying.prob2,file='output/drought/drought_varying_prob2.Rdata')

drought.varying.prob4 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','clearcut','shelterwood'),
                                    mast.scenario = "hee",
                                    weevil.scenario = "yearly-treat-diff",
                                    dispersal.scenario = "yearly-treat-diff",
                                    seedling.scenario = "randomdrought",
                                    prob.drought = 0.4,
                                    force.processors = 12,
                                    ram.max = 5000)
save(drought.varying.prob4,file='output/drought/drought_varying_prob4.Rdata')

drought.varying.prob6 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','clearcut','shelterwood'),
                                    mast.scenario = "hee",
                                    weevil.scenario = "yearly-treat-diff",
                                    dispersal.scenario = "yearly-treat-diff",
                                    seedling.scenario = "randomdrought",
                                    prob.drought = 0.6,
                                    force.processors = 12,
                                    ram.max = 5000)
save(drought.varying.prob6,file='output/drought/drought_varying_prob6.Rdata')

drought.varying.prob8 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','clearcut','shelterwood'),
                                    mast.scenario = "hee",
                                    weevil.scenario = "yearly-treat-diff",
                                    dispersal.scenario = "yearly-treat-diff",
                                    seedling.scenario = "randomdrought",
                                    prob.drought = 0.8,
                                    force.processors = 12,
                                    ram.max = 5000)
save(drought.varying.prob8,file='output/drought/drought_varying_prob8.Rdata')

drought.varying.prob10 <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','clearcut','shelterwood'),
                                    mast.scenario = "hee",
                                    weevil.scenario = "yearly-treat-diff",
                                    dispersal.scenario = "yearly-treat-diff",
                                    seedling.scenario = "randomdrought",
                                    prob.drought = 1.0,
                                    force.processors = 12,
                                    ram.max = 5000)
save(drought.varying.prob10,file='output/drought/drought_varying_prob10.Rdata')


#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)

setwd('output/drought')
zip('drought.zip',files=list.files())
setwd('../..')

pbPost('note','Analysis Complete',
       paste('Drought experiment on rbrutus16 complete after',runtime,'minutes.'),
       devices='Nexus 6')

pbPost('file',url='output/drought/drought.zip')

#Shut down instance 
system('sudo shutdown -h now')

###############################################################

lapply(c('output/drought/drought_prob0.Rdata','output/drought/drought_prob2.Rdata','output/drought/drought_prob4.Rdata',
         'output/drought/drought_prob6.Rdata','output/drought/drought_prob8.Rdata','output/drought/drought_prob10.Rdata'),
         load,.GlobalEnv)

###############################################################

#Analysis

source('utility_functions.R')

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
