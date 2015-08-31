
source('sensitivity_test.R')

library(RPushbullet)

start.time <- Sys.time()

sens.test <- sensitivity.test(nreps=96,burnin=20,length=26,
                         harvests = c('none','clearcut'),
                         force.processors=12, ram.max=5000)
save(sens.test,file='output/sens_test.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

source('utility_functions.R')

sens.analysis <- compare.sensitivity(sens.test,10,'none','seedclass123')