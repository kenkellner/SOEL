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

datalist <- list(avg=weevil.dispersal.average,trt=weevil.treateff,yrly=weevil.yearlyeff,
                 treat.yrly=weevil.treatyearlyeff)
gen.figures(datalist,'seedclass123',36,c(0,15000))


datalist <- list(avg=weevil.dispersal.average,trt=dispersal.treateff,yrly=dispersal.yearlyeff,
                 treat.yrly=dispersal.treatyearlyeff)

datalist <- add.newseedlings(datalist,30,40)
gen.figures(datalist,'seedlingsum',36,c(0,30000),singleplot=T)
test1 = analyze.ibm(datalist,'pctgerm',year=36)
gen.figures(datalist,'seedclass123',36,c(0,17000),singleplot=T)
test2 = analyze.ibm(datalist,'seedclass123',year=36)
gen.figures(datalist,'seedlingsum',36,c(0,30000),singleplot=T)
test3 = analyze.ibm(datalist,metric='seedlingsum')
print(model.tables(test3$anova,"means"),digits=3)

test4 = analyze.ibm(datalist,metric='seedclass4',year=36)
##################################################################
datalist <- list(avg=weevil.dispersal.average,trt=dispersal.treateff,yrly=dispersal.yearlyeff,
                 treat.yrly=dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,40)
gen.barplot(datalist,"seedlingsum",ylim=c(0,29000),
            ylab="Total New Seedlings / ha Over 10 Yr",xlab="Model",
            names=c('Constant','Treat','Year','Treat x Year'),
            legend=c('No Harvest','Shelterwood'),
            legx=1,legy=25000)

test = analyze.ibm(datalist,metric='seedlingsum')
print(model.tables(test3$anova,"means"),digits=3)

t = gen.dataset(datalist,'seedlingsum')
t$harvest = as.factor(t$harvest)

time = rep(0,length=length(t$seedlingsum))
trt = rep(0,length=length(t$seedlingsum))

trt[which(t$scenario%in%c('trt','treat.yrly'))] = 1
time[which(t$scenario%in%c('yrly','treat.yrly'))] = 1


t$scenario = as.factor(t$scenario)

contrasts(t$scenario) = c(0,0,1,-1)

a = aov(seedlingsum~harvest + trt + time + harvest*trt + harvest*time 
       ,data=t)
a = aov(seedlingsum~harvest*trt*time,data=t)
summary.lm(a)




##Weevils
datalist <- list(avg=weevil.dispersal.average,trt=weevil.treateff,yrly=weevil.yearlyeff,
                 treat.yrly=weevil.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,40)
gen.barplot(datalist,"seedlingsum",ylim=c(0,25000),
            ylab="Total New Seedlings / ha Over 10 Yr",xlab="Model",
            names=c('Constant','Treat','Year','Treat x Year'),
            legend=c('No Harvest','Shelterwood'),
            legx=1,legy=25000)
test = analyze.ibm(datalist,metric='seedlingsum')


##################################################################
datalist <- list(avg=weevil.dispersal.average,trt=dispersal.treateff,yrly=dispersal.yearlyeff,
                 treat.yrly=dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,40)
gen.barplot(datalist,"seedclass4",ylim=c(0,300),year=40,
            ylab="Saplings (> 1.4 m) / ha",xlab="Model",
            names=c('Constant','Treat','Year','Treat x Year'),
            legend=c('No Harvest','Shelterwood'),
            legx=1,legy=300)

t2 <- gen.dataset(datalist,'seedclass4',40)
mod <- aov(seedclass4~harvest+trt+time+harvest*time+harvest*trt,data=t2)
summary.lm(mod)

test = analyze.ibm(datalist,metric='seedclass4',year=40)
print(model.tables(test3$anova,"means"),digits=3)

##Weevils
datalist <- list(avg=weevil.dispersal.average,trt=weevil.treateff,yrly=weevil.yearlyeff,
                 treat.yrly=weevil.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,40)
gen.barplot(datalist,"seedclass4",ylim=c(0,300),year=40,
            ylab="Saplings (> 1.4 m) / ha",xlab="Model",
            names=c('Constant','Treat','Year','Treat x Year'),
            legend=c('No Harvest','Shelterwood'),
            legx=1,legy=300)
test = analyze.ibm(datalist,metric='seedclass4',year=40)


