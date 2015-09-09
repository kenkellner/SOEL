
source('sensitivity_test.R')

library(RPushbullet)

start.time <- Sys.time()

sens.test <- sensitivity.test(nreps=96,burnin=20,length=26,
                         harvests = c('none','clearcut'),
                         force.processors=12, ram.max=5000)
save(sens.test,file='output/sens_test.Rdata')

sens.test.sh <- sensitivity.test(nreps=96,burnin=20,length=26,
                              harvests = c('shelterwood'),
                              force.processors=12, ram.max=5000)
save(sens.test.sh,file='output/sens_test_sh.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

source('utility_functions.R')

sens.analysis <- compare.sensitivity(sens.test,10,'none','seedclass123')

sens.sh.analysis <- compare.sensitivity(sens.test.sh,10,'shelterwood','seedclass4')

summary(lm(seedclass123~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
           +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob,
           data=as.data.frame(scale(sens.test$clearcut))))



summary(lm(seedclass123~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
           +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil*mast.val+disperse.prob*mast.val
           +disp.eaten.prob*mast.val+undisp.eaten.prob*mast.val+cache.prob*mast.val,
           data=as.data.frame(scale(sens.test$clearcut))))

test = lm(seedclass123~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
           +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil*mast.val+disperse.prob*mast.val
           +disp.eaten.prob*mast.val+undisp.eaten.prob*mast.val+cache.prob*mast.val,
           data=as.data.frame(scale(sens.test$clearcut)))

test2 = lm(seedclass4~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
          +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil*mast.val+disperse.prob*mast.val
          +disp.eaten.prob*mast.val+undisp.eaten.prob*mast.val+cache.prob*mast.val,
          data=as.data.frame(scale(sens.test$clearcut)))

summary(step(test,scope=seedclass123~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
        +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil*mast.val+disperse.prob*mast.val
        +disp.eaten.prob*mast.val+undisp.eaten.prob*mast.val+cache.prob*mast.val,direction="both"))


summary(step(test2,scope=seedclass4~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
        +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil*mast.val+disperse.prob*mast.val
        +disp.eaten.prob*mast.val+undisp.eaten.prob*mast.val+cache.prob*mast.val,direction="both"))

#Testing a method to setup a Latin Hypercube with correlated predictor variables
library(pse)

covs = c('prob.browse','prob.weevil','prob.dispersal')

corm <- matrix(data=c(1,0,0.5,0,1,0,0.5,0,1),nrow=3,ncol=3)

q = 'qunif'

qarg <- list(prob.browse=list(min=0,max=1),prob.weevil=list(min=0,max=0.5),prob.dispersal=list(min=0,max=1))

test <- LHS(model=NULL,factors=covs,N=20,q=q,q.arg=qarg,opts=list(COR=corm))

#Testinga method to calculate commonality coefficients from sensitivity regression output
library(yhat)

attach(sens.test$clearcut)

test = lm(seedclass4~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
          +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil)

out.test <- calc.yhat(test)

detach(sens.test$clearcut)

