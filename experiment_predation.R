##Comparison of different seed predation scenarios

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
weevil.dispersal.average <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=30,nyears=40,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.dispersal.average,file='output/predation/weevil_dispersal_average.Rdata')

weevil.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=30,nyears=40,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "yearly-diff",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)
save(weevil.yearlyeff,file='output/predation/weevil_yearlyeff.Rdata')

dispersal.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=30,nyears=40,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "yearly-diff",
                                force.processors = 12,
                                ram.max = 5000)
save(dispersal.yearlyeff,file='output/predation/dispersal_yearlyeff.Rdata')


weevil.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                              burnin=30,nyears=40,
                              harvests = c('none','shelterwood'),
                              mast.scenario = "hee",
                              weevil.scenario = "treat-diff",
                              dispersal.scenario = "fixedaverage",
                              force.processors = 12,
                              ram.max = 5000)
save(weevil.treateff,file='output/predation/weevil_treateff.Rdata')

dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                   burnin=30,nyears=40,
                                   harvests = c('none','shelterwood'),
                                   mast.scenario = "hee",
                                   weevil.scenario = "fixedaverage",
                                   dispersal.scenario = "treat-diff",
                                   force.processors = 12,
                                   ram.max = 5000)
save(dispersal.treateff,file='output/predation/dispersal_treateff.Rdata')

weevil.dispersal.yearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-diff',
                                        dispersal.scenario = "yearly-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.yearlyeff,file='output/predation/weevil_dispersal_yearlyeff.Rdata')

weevil.dispersal.treateff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'treat-diff',
                                        dispersal.scenario = "treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treateff,file='output/predation/weevil_dispersal_treateff.Rdata')

weevil.treatyearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-treat-diff',
                                        dispersal.scenario = "fixedaverage",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.treatyearlyeff,file='output/predation/weevil_treatyearlyeff.Rdata')

dispersal.treatyearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                    burnin=30,nyears=40,
                                    harvests = c('none','shelterwood'),
                                    mast.scenario = 'hee',
                                    weevil.scenario = 'fixedaverage',
                                    dispersal.scenario = "yearly-treat-diff",
                                    force.processors = 12,
                                    ram.max = 5000)
save(dispersal.treatyearlyeff,file='output/predation/dispersal_treatyearlyeff.Rdata')

weevil.dispersal.treatyearlyeff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-treat-diff',
                                        dispersal.scenario = "yearly-treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)
save(weevil.dispersal.treatyearlyeff,file='output/predation/weevil_dispersal_treatyearlyeff.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)

setwd('output/predation')
zip('predation.zip',files=list.files())
setwd('../..')

pbPost('note','Analysis Complete',
       paste('Predation experiment on rbrutus16 complete after',runtime,'minutes. Sending files and shutting down instance.'),
       devices='Nexus 6')

pbPost('file',url='output/predation/predation.zip')

#Shut down instance 
system('sudo shutdown -h now')

source('utility_functions.R')

lapply(c('output/predation/dispersal_treateff.Rdata','output/predation/dispersal_treatyearlyeff.Rdata',
         'output/predation/dispersal_yearlyeff.Rdata',
         'output/predation/weevil_dispersal_average.Rdata','output/predation/weevil_dispersal_treateff.Rdata',
         'output/predation/weevil_dispersal_treatyearlyeff.Rdata','output/predation/weevil_dispersal_yearlyeff.Rdata',
         'output/predation/weevil_treateff.Rdata','output/predation/weevil_treatyearlyeff.Rdata',
         'output/predation/weevil_yearlyeff.Rdata'),
       load,.GlobalEnv)

##################################################################

#Dispersal Effects

datalist <- list(avg=weevil.dispersal.average,trt=dispersal.treateff,yrly=dispersal.yearlyeff,
                 treat.yrly=dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,38)

#Total New Seedlings over 10 Years
test = analyze.ibm(datalist,metric='seedlingsum')

h = gen.dataset(datalist,'seedlingsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedlingsum~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(12000,31000),ylab=expression("New Seedlings "~ ha^{-1}~"(10 Year Sum)"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,31000,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+1000),c('A','A','A','B','C','C','C','D'))

##Saplings

test = analyze.ibm(datalist,metric='seedorigin',40)

h = gen.dataset(datalist,'seedorigin',40)

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedorigin~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylab=expression("Seed-origin Saplings "~ ha^{-1}))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,250,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+c(10,10,15,20,20,10,15,10)),
     c('A','B','A','B','C','D','C','D'))

#######
#Pct Germ
test = analyze.ibm(datalist,metric='pctgerm',36)

h = gen.dataset(datalist,'pctgerm',36)

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(pctgerm~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(0.01,0.04),ylab=expression("Yearly Percent Emergence"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,0.04,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+c(0.002,0.003,0.004,0.002,0.002,0.0015,0.002,0.002)),
     c('A','A','A','B','C','C','C','C'))

##Total acorns

test = analyze.ibm(datalist,metric='acornsum')

h = gen.dataset(datalist,'acornsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(acornsum/1000~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(600,875),
             ylab=expression("Acorns Produced "~ ha^{-1}~"(10 Year Sum) x 1000"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,0.04,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+10),"A")


##################################################################

##Weevils

datalist <- list(avg=weevil.dispersal.average,trt=weevil.treateff,yrly=weevil.yearlyeff,
                 treat.yrly=weevil.treatyearlyeff)

datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,38)

#Total New Seedlings over 10 Years
test = analyze.ibm(datalist,metric='seedlingsum')

h = gen.dataset(datalist,'seedlingsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedlingsum~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(12000,31000),ylab=expression("New Seedlings "~ ha^{-1}~"(10 Year Sum)"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,31000,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+1000),c('A','A','A','B','C','C','C','D'))

##Saplings

test = analyze.ibm(datalist,metric='seedorigin',40)

h = gen.dataset(datalist,'seedorigin',40)

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(seedorigin~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylab=expression("Seed-origin Saplings "~ ha^{-1}))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,250,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+c(10,10,15,20,20,10,15,10)),
     c('A','B','A','B','C','D','C','D'))

#######
#Pct Germ
test = analyze.ibm(datalist,metric='pctgerm',36)

h = gen.dataset(datalist,'pctgerm',36)

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(pctgerm~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(0.01,0.04),ylab=expression("Yearly Percent Emergence"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,0.04,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+c(0.002,0.003,0.004,0.002,0.002,0.0015,0.002,0.002)),
     c('A','A','A','B','C','C','C','C'))

##Total acorns

test = analyze.ibm(datalist,metric='acornsum')

h = gen.dataset(datalist,'acornsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,2,2) + 0.1)
bx = boxplot(acornsum/1000~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Model",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(600,875),
             ylab=expression("Acorns Produced "~ ha^{-1}~"(10 Year Sum) x 1000"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("Constant",'Treat','Year','Treat x Year'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,0.04,legend=c("No Harvest","Shelterwood"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+10),"A")

#Combined

datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,38)

test <- analyze.ibm(datalist,metric='acornsum')
test$anova.mc

test <- analyze.ibm(datalist,metric='seedlingsum')
