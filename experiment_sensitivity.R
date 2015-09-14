
source('sim_function.R')

library(RPushbullet)

#Generate correlation matrix for Latin hypercube sampling
psummary <- read.csv('data/param_summary.csv',header=T)[1:9,]

covs = c('pDispersal','weibSc','weibSh','pDispEaten','pCache','pUndispEaten','pWeevil',
         'lamAcorn','pBrowse','pDrought')

corm <- matrix(data=NA,nrow=10,ncol=10)
corm[,10] <- corm[10,] <- 0
corm[10,10] <- 1

for (i in 1:(length(covs)-1)){
  for (j in 1:(length(covs)-1)){
    
    if(i==j){corm[i,j]=1
    }else{
      hold1 <- psummary[,i]
      hold2 <- psummary[,j]
      if(is.na(hold1[1])|is.na(hold2[1])){
        hold1 <- hold1[6:9]
        hold2 <- hold2[6:9]
      }
      corm[i,j] <- cor(hold1,hold2)
    }
  }}

#Parameter sampling distribution arguments
#Based on actual ranges
qarg.actual <- list(pDispersal=list(min=min(psummary[,1],na.rm=T),max=max(psummary[,1],na.rm=T)),
                    weibSc=list(min=min(psummary[,2],na.rm=T),max=max(psummary[,2],na.rm=T)),
                    weibSh=list(min=min(psummary[,3],na.rm=T),max=max(psummary[,3],na.rm=T)),
                    pDispEaten=list(min=min(psummary[,4],na.rm=T),max=max(psummary[,4],na.rm=T)),
                    pCache=list(min=min(psummary[,5],na.rm=T),max=max(psummary[,5],na.rm=T)),
                    pUndispEaten=list(min=min(psummary[,6],na.rm=T),max=max(psummary[,6],na.rm=T)),
                    pWeevil=list(min=min(psummary[,7],na.rm=T),max=max(psummary[,7],na.rm=T)),
                    lamAcorn=list(min=min(psummary[,8],na.rm=T),max=max(psummary[,8],na.rm=T)),
                    pBrowse=list(min=min(psummary[,9],na.rm=T),max=max(psummary[,9],na.rm=T)),
                    pDrought=list(min=0,max=1))

#Wider ranges
qarg.wide <- list(pDispersal=list(min=0,max=1),
                  weibSc=list(min=4.216,max=11.962),
                  weibSh=list(min=0.957,max=1.88),
                  pDispEaten=list(min=0,max=1),
                  pCache=list(min=0,max=1),
                  pUndispEaten=list(min=0,max=1),
                  pWeevil=list(min=0,max=1),
                  lamAcorn=list(min=min(psummary[,8],na.rm=T),max=max(psummary[,8],na.rm=T)),
                  pBrowse=list(min=0,max=1),
                  pDrought=list(min=0,max=1))

start.time <- Sys.time()

sens.test.actual <- forest.sim(nreps=1008,burnin=20,nyears=26,
                              harvests = c('none','clearcut','shelterwood'),
                              force.processors=12, ram.max=5000, 
                              sensitivity=TRUE,
                              covs=covs,q="qunif",
                              qarg=qarg.actual,
                              corm=corm)


save(sens.test.actual,file='output/sens_test_actual.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')

source('utility_functions.R')

focus.year = 25

#Simple correlation test
correlation.test(sens.test,'none','seedclass4',focus.year)

#Sensitivity partitioning (Xu and Gertner 2008)
inp.covs <- as.data.frame(scale(sens.test$lhc))
out.vals <- sens.test$out$none$seedclass4[focus.year,]

varPart(out.vals,inp.covs)

#Communality coefficients
library(yhat)

attach(inp.covs)

inp.lm <- lm(out.vals~pDispersal+weibSc)

com.coeff <- calc.yhat(inp.lm)

detach(inp.covs)

