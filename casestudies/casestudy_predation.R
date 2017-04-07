####################################################################
## Case study: interaction of seed predation and midstory removal ##
####################################################################

source('run_SOEL.R')
source('notify.R')

#Run experiment and save results
weevil.dispersal.average <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                burnin=30,nyears=40,
                                harvests = c('none','shelterwood'),
                                mast.scenario = "hee",
                                weevil.scenario = "fixedaverage",
                                dispersal.scenario = "fixedaverage",
                                force.processors = 12,
                                ram.max = 5000)

weevil.dispersal.yearlyeff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-diff',
                                        dispersal.scenario = "yearly-diff",
                                        force.processors = 12,
                                        ram.max = 5000)

weevil.dispersal.treateff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'treat-diff',
                                        dispersal.scenario = "treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)

weevil.dispersal.treatyearlyeff <- run.SOEL(xcorewidth=140, ycorewidth=140, nreps=36,
                                        burnin=30,nyears=40,
                                        harvests = c('none','shelterwood'),
                                        mast.scenario = 'hee',
                                        weevil.scenario = 'yearly-treat-diff',
                                        dispersal.scenario = "yearly-treat-diff",
                                        force.processors = 12,
                                        ram.max = 5000)

save(weevil.dispersal.average,weevil.dispersal.treateff,weevil.dispersal.yearlyeff,
     weevil.dispersal.treatyearlyeff,file='output/casestudy_predation.Rdata')

#Notify with simplepush
notify("predation done")

#Shut down instance 
system('sudo shutdown -h now')

##################################################################

#Test for differences among scenarios

source('utility_functions.R')

load('output/casestudy_predation.Rdata')

datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,37)
datalist <- add.pctgermmean(datalist,30,37)

##Total acorns produced
test = analyze.ibm(datalist,metric='acornsum')
summary(test$anova)
test$anova.mc

#Total New Seedlings over 10 Years
test = analyze.ibm(datalist,metric='seedlingsum')

#Overall harvest effect
newseed <- gen.dataset(datalist,metric='seedlingsum')
nm <- mean(newseed$seedlingsum[newsap$harvest=='none'])
mm <- mean(newseed$seedlingsum[newsap$harvest=='shelterwood'])
(nm-mm)/nm

#Effect of TE in midstory removal
nm <- mean(newseed$seedlingsum[newseed$harvest=='shelterwood'&newseed$scenario=='avg'])
mm <- mean(newseed$seedlingsum[newseed$harvest=='shelterwood'&newseed$scenario=='trt'])
(mm-nm)/nm

#Yearly effects
nm <- mean(newseed$seedlingsum[newseed$scenario=='avg'])
mm <- mean(newseed$seedlingsum[newseed$scenario=='treat.yrly'])
(mm-nm)/nm

##Saplings
test = analyze.ibm(datalist,metric='seedorigin',37)

#Overall harvest effects
newsap <- gen.dataset(datalist,metric='seedorigin',37)
mean(newsap$seedorigin[newsap$harvest=='none'])
mean(newsap$seedorigin[newsap$harvest=='shelterwood'])

#Treatment effects in midstory removal
nm <- mean(newsap$seedorigin[newsap$scenario=='avg'])
mm <- mean(newsap$seedorigin[newsap$scenario=='treat.yrly'])
(mm-nm)/nm


#######
#Pct Germ
test = analyze.ibm(datalist,metric='pctgermmean')

#Overall harvest effects
newgerm <- gen.dataset(datalist,metric='pctgermmean')
nm <- mean(newgerm$pctgermmean[newsap$harvest=='none'])
mm <- mean(newgerm$pctgermmean[newsap$harvest=='shelterwood'])
(nm-mm)/nm

#Treatment effects in midstory removal
nm <- mean(newgerm$pctgermmean[newgerm$harvest=='shelterwood'&newgerm$scenario=='avg'])
mm <- mean(newgerm$pctgermmean[newgerm$harvest=='shelterwood'&newgerm$scenario=='trt.yearly'])
(mm-nm)/nm

#Yearly effects
nm <- mean(newgerm$pctgermmean[newgerm$scenario=='avg'])
mm <- mean(newgerm$pctgermmean[newgerm$scenario=='treat.yrly'])
(mm-nm)/nm

