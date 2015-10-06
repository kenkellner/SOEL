
source('sim_function.R')

library(RPushbullet)

#Generate correlation matrix for Latin hypercube sampling
psummary <- read.csv('data/param_summary.csv',header=T)[1:9,]

covs = c('pDispersal','weibSc','pDispEaten','pCache','pUndispEaten','pWeevil',
         'lamAcorn','pBrowse','meanGr','meanSurv')

corm <- matrix(data=NA,nrow=10,ncol=10)
corm[,9:10] <- corm[9:10,] <- 0
corm[9,9] <- 1
corm[10,10] <- 1

for (i in 1:(length(covs)-2)){
  for (j in 1:(length(covs)-2)){
    
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

#Narrower ranges (1 sd around mean)
mn <- colMeans(psummary,na.rm=T)
adj <- apply(psummary,2,sd,na.rm=T)
up <- mn+adj
lw <- mn-adj
qarg.sd <- list(pDispersal=list(min=lw[1],max=up[1]),
                    weibSc=list(min=lw[2],max=up[2]),
                    pDispEaten=list(min=lw[3],max=up[3]),
                    pCache=list(min=lw[4],max=1),
                    pUndispEaten=list(min=lw[5],max=1),
                    pWeevil=list(min=lw[6],max=up[6]),
                    lamAcorn=list(min=0,max=up[7]),
                    pBrowse=list(min=lw[8],max=up[8]),
                    meanGr=list(min=(1.24-0.125),max=(1.24+0.125)),
                    meanSurv=list(min=(-.6-0.114),max=(-.6+0.114))
                    )



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
                    meanGr=list(mean=1.24,sd=0.125),meanSurv=list(mean=-0.600,sd=0.114))

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

#Narrower ranges (10% around mean)
mn <- colMeans(psummary,na.rm=T)

apply(psummary,2,sd,na.rm=T)

adj <- .1*colMeans(psummary,na.rm=T)
up <- mn+adj
lw <- mn-adj
qarg.narrow <- list(pDispersal=list(min=lw[1],max=up[1]),
                    weibSc=list(min=lw[2],max=up[2]),
                    weibSh=list(min=lw[3],max=up[3]),
                    pDispEaten=list(min=lw[4],max=up[4]),
                    pCache=list(min=lw[5],max=up[5]),
                    pUndispEaten=list(min=lw[6],max=up[6]),
                    pWeevil=list(min=lw[7],max=up[7]),
                    lamAcorn=list(min=lw[8],max=up[8]),
                    pBrowse=list(min=lw[9],max=up[9]),
                    pDrought=list(min=0.5-0.1*0.5,max=0.5+0.1*0.5))

######################################################################

start.time <- Sys.time()

sens.test.sd <- forest.sim(nreps=504,burnin=50,nyears=60,
                              harvests = c('none'),
                              force.processors=12, ram.max=5000, 
                              sensitivity=TRUE,
                              covs=covs,q="qunif",
                              qarg=qarg.sd,
                              corm=corm)


save(sens.test.sd,file='output/sens_test_sd.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)

pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

pbPost('file',url='output/sens_test_sd.Rdata')

#Shut down instance 
system('sudo shutdown -h now')

source('utility_functions.R')

focus.year = 26

#Simple correlation test
correlation.test(sens.test,'none','seedclass4',focus.year)

#Sensitivity partitioning (Xu and Gertner 2008)
inp.covs <- as.data.frame(scale(sens.test.actual.shelter$lhc))
out.vals <- sens.test.actual.shelter$out$shelter$seedclass123[focus.year,]

varPart(out.vals,inp.covs,4)

#Communality coefficients
library(yhat)

attach(inp.covs)

inp.lm <- lm(out.vals~pDispersal+weibSc)

com.coeff <- calc.yhat(inp.lm)

detach(inp.covs)



