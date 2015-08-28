
source('sensitivity_test.R')

sens.test <- sensitivity.test(nreps=96,burnin=20,length=26,
                         harvests = c('none','clearcut'),
                         force.processors=12, ram.max=5000)
save(sens.test,file='output/sens_test.Rdata')