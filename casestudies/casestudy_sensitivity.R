#############################
## Case study: sensitivity ##
#############################

source('run_SOEL.R')

library(RPushbullet)

#Generate correlation matrix for Latin hypercube sampling
psummary <- read.csv('data/param_summary.csv',header=T)[1:9,]

covs = c('meanAcorn','pWeevil','pDispersal','weibSc','pCache','pDispEaten','pUndispEaten',
         'pBrowse','meanGr','meanSurv')

corm <- matrix(data=NA,nrow=10,ncol=10)

for (i in 1:(length(covs))){
  for (j in 1:(length(covs))){
    
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

#Intercept approach
prange <- read.csv('data/param_ranges.csv',h=T)
mn <- prange$intercept
sds <- prange$se

qarg.int <- list(meanAcorn=list(mean=mn[1],sd=sds[1]),
                pWeevil=list(mean=mn[2],sd=sds[2]),
                pDispersal=list(mean=mn[3],sd=sds[3]),
                weibSc=list(mean=mn[4],sd=sds[4]),
                pCache=list(mean=mn[5],sd=sds[5]),
                pDispEaten=list(mean=mn[6],sd=sds[6]),
                pUndispEaten=list(mean=mn[7],sd=sds[7]),
                pBrowse=list(mean=mn[8],sd=sds[8]),
                meanGr=list(mean=mn[9],sd=sds[9]),
                meanSurv=list(mean=mn[10],sd=sds[10])
)

######################################################################

start.time <- Sys.time()

sens.test.int <- run.SOEL(nreps=504,burnin=30,nyears=40,
                              harvests = c('none'),
                              force.processors=12, ram.max=5000, 
                              sensitivity=TRUE,
                              covs=covs,q="qnorm",
                              qarg=qarg.int,
                              corm=corm)


save(sens.test.int,file='output/casestudy_sensitivity.Rdata')

#Calculate runtime and push alert message
end.time <- Sys.time() 
runtime <- round(as.numeric(end.time-start.time,units="mins"),digits=3)

pbPost('note','Analysis Complete',
       paste('Sensitivity experiment on rbrutus16 complete after',runtime,'minutes. Shutting down instance.'),
       devices='Nexus 6')

#Shut down instance 
system('sudo shutdown -h now')


##########################################################

#Sensitivity partitioning (Xu and Gertner 2008)

source('utility_functions.R')

load('output/casestudy_sensitivity.Rdata')

#Pctgerm (year 37)
inp.covs <- as.data.frame(scale(sens.test.int$lhc))
out.vals <- sens.test.int$out$none$pctgerm[37,]
varPart(out.vals,inp.covs,4)[,c(1,5,8)]

#Seedlings (total new seedlings accumulated 30-37)
out.vals <- colSums(sens.test.int$out$none$newseedlings[30:37,])
varPart(out.vals,inp.covs,4)[,c(1,5,8)]

#Saplings (total at year 37)
out.vals <- sens.test.int$out$none$seedclass4[37,]
varPart(out.vals,inp.covs,4)[,c(1,5,8)]
