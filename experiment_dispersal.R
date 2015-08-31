##Comparison of different dispersal scenarios given average conditions otherwise

source('sim_function.R')

library(RPushbullet)

start.time <- Sys.time()

#Run experiment and save results
disp.average <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                             burnin=20,nyears=40,
                             dispersal.scenario = "fixedaverage",
                             force.processors = 12,
                             ram.max = 5000)
save(disp.average,file='output/disp_average.Rdata')
disp.treatdiff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                         burnin=20,nyears=40,
                         dispersal.scenario = "treat-diff",
                         force.processors = 12,
                         ram.max = 5000)
save(disp.treatdiff,file='output/disp_treatdiff.Rdata')
disp.yearlydiff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                             burnin=20,nyears=40,
                             mast.scenario = 'hee',
                             dispersal.scenario = "yearly-diff",
                             force.processors = 12,
                             ram.max = 5000)
save(disp.yearlydiff,file='output/disp_yearlydiff.Rdata')
disp.yearlytreatdiff <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                              burnin=20,nyears=40,
                              mast.scenario = 'hee',
                              dispersal.scenario = "treat-yearly-diff",
                              force.processors = 12,
                              ram.max = 5000)
save(disp.yearlytreatdiff,file='output/disp_yearlytreatdiff.Rdata')
disp.yearlytreatdiffweev <- forest.sim(xcorewidth=140, ycorewidth=140, nreps=24,
                                   burnin=20,nyears=40,
                                   mast.scenario = 'hee',
                                   weevil.scenario = 'hee',
                                   dispersal.scenario = "treat-yearly-diff",
                                   force.processors = 12,
                                   ram.max = 5000)
save(disp.yearlytreatdiffweev,file='output/disp_yearlytreatdiffweev.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Dispersal experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')

#Reload data
lapply(c('output/disp_average.Rdata','output/disp_treatdiff.Rdata','output/disp_yearlydiff.Rdata',
         'output/disp_yearlytreatdiff.Rdata',
         'output/disp_yearlytreatdiffweev.Rdata'),load,.GlobalEnv)

source('utility_functions.R')

for (i in 1:4){
  for (j in 1:39){
    mast.average[[i]][[j]] <- mast.average[[i]][[j]][,1:24]
  }
}

datalist <- list(disp.average=disp.average,disp.treatdiff=disp.treatdiff,disp.yearlydiff=disp.yearlydiff,
                 disp.yearlytreatdiff=disp.yearlytreatdiff,disp.ytweev=disp.yearlytreatdiffweev)

gen.figures(datalist,'seedclass123',25,c(0,8000),singleplot=T)
gen.figures(datalist,'seedclass4',25,c(0,100),singleplot=T)
gen.figures(datalist,'seedboclass4',25,c(0,40),singleplot=T)
gen.figures(datalist,'seedwoclass4',25,c(0,60),singleplot=T)
gen.figures(datalist,'ba',25,c(0,35),singleplot=T)