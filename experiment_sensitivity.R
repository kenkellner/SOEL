
source('sensitivity_test.R')

library(RPushbullet)

start.time <- Sys.time()

sens.test <- sensitivity.test(nreps=96,burnin=20,length=26,
                         harvests = c('none','clearcut','shelterwood'),
                         force.processors=12, ram.max=5000, qarg=qarg.actual)
save(sens.test,file='output/sens_test.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

source('utility_functions.R')

sens.analysis <- compare.sensitivity(sens.test,10,'none','seedclass123')

sens.sh.analysis <- compare.sensitivity(sens.test.sh,10,'shelterwood','seedclass4')

summary(lm(seedclass4~pBrowse+pWeevil+pDrought+lamAcorn+pDispersal+pCache
           +weibSc+weibSh+pDispEaten+pUndispEaten,
           data=as.data.frame(scale(sens.test$none))))



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


#Testinga method to calculate commonality coefficients from sensitivity regression output
library(yhat)

attach(sens.test$clearcut)

test = lm(seedclass4~prob.browse+prob.weevil+prob.drought+mast.val+disperse.prob+cache.prob
          +weibSc+weibSh+disp.eaten.prob+undisp.eaten.prob+prob.weevil)

out.test <- calc.yhat(test)

detach(sens.test$clearcut)

