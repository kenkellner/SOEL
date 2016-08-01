##Comparison of different drought scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results

drought.average <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                        burnin=30,nyears=40,
                        harvests = c('none','clearcut','shelterwood'),
                        mast.scenario = "hee",
                        weevil.scenario = "fixedaverage",
                        dispersal.scenario = "fixedaverage",
                        seedling.scenario = "fixedaverage",
                        force.processors = 12,
                        ram.max = 5000)
save(drought.average,file='output/drought/drought_average.Rdata')

drought.prob0 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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


drought.prob2 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.prob4 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.prob6 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.prob8 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.prob10 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob0 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob2 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob4 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob6 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob8 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

drought.varying.prob10 <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
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

source('utility_functions.R')

datalist = list(dn=drought.average,d00=drought.prob0,d02=drought.prob2,d04=drought.prob4,
                d06=drought.prob6,d08=drought.prob8,d10=drought.prob10)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)

s <- gen.dataset(datalist,'seedlingsum')
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10','dn'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedlingsum~harvest*scenario,data=s,col=gray.colors(3),
             xlab="Drought Probability",
             at=c(1:18,20,21,22),
             ylab=expression("New Seedlings "~ ha^{-1}~"(10 Year Sum)"),
             xaxt='n')
axis(1,at=c(2,5,8,11,14,17,21),tick=T,
     labels=c("0","0.2",'0.4','0.6','0.8','1.0','Avg'))
abline(v=19)
legend(12,32000,legend=c('No Harvest','Shelterwood','Clearcut'),fill=gray.colors(3))




     
#######################################################################


s <- gen.dataset(datalist,'seedorigin',36)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10','dn'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedorigin~harvest*scenario,data=s,col=gray.colors(3),
             xlab="Drought Probability",
             at=c(1:18,20,21,22),
              ylab=expression("Seed-origin Saplings "~ ha^{-1}),
              xaxt='n')
axis(1,at=c(2,5,8,11,14,17,21),tick=T,
     labels=c("0","0.2",'0.4','0.6','0.8','1.0','Avg'))
abline(v=19)
legend(11.5,3500,legend=c('No Harvest','Shelterwood','Clearcut'),fill=gray.colors(3))

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


test <- aov(seedorigin~harvest*dcont,data=sa)
summary.lm(test)



sa <- gen.dataset(datalist,'seedlingsum')
sa$harvest <- as.factor(sa$harvest)
sa$harvest <- relevel(sa$harvest,ref='none')
test <- aov(seedlingsum~harvest*dcont,data=sa)
summary.lm(test)



####################################################################
datalist = list(d00=drought.varying.prob0,d02=drought.varying.prob2,
                d04=drought.varying.prob4,
                d06=drought.varying.prob6,d08=drought.varying.prob8,
                d10=drought.varying.prob10)
datalist <- add.newseedlings(datalist,30,40)
datalist <- add.seedorigin(datalist)

s <- gen.dataset(datalist,'regen',36)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(regen~harvest*scenario,data=s,col=gray.colors(3),
             xlab="Drought Probability",
             ylab=expression("Seed-origin Saplings "~ ha^{-1}),
             xaxt='n')
axis(1,at=c(2,5,8,11,14,17),tick=T,
     labels=c(0,0.2,0.4,0.6,0.8,1.0))
legend(11.5,3500,legend=c('No Harvest','Shelterwood','Clearcut'),fill=gray.colors(3))

