#Seed fate models

library(dplyr)
disp = read.csv('data/ibm_dispersal.csv',header=T)
disp <- disp[disp$clear==0,]
disp$dist[is.na(disp$dist)] <- 0
disp$dist[1973] <- 24.5
disp <- rbind(filter(disp,Treecode!=4),
              filter(disp,Treecode==4,year12==0))
disp <- disp[disp$squirrel==0,]

#Augment
p.surface <- 0.75
p.buried <- 0.4
add.surface.no <- round((1-p.surface)*length(disp$removed[disp$removed==1&disp$buried==0]))
select.surface <- sample(c(1:length(disp$removed[disp$removed==1&disp$buried==0])),add.surface.no)
add.surface <- disp[disp$removed==1&disp$buried==0,][select.surface,]

add.buried.no <- round((1-p.buried)*length(disp$removed[disp$removed==1&disp$buried==1]))
select.buried <- sample(c(1:length(disp$removed[disp$removed==1&disp$buried==1])),add.buried.no)
add.buried <- disp[disp$removed==1&disp$buried==1,][select.buried,]

disp <- rbind(disp,add.surface,add.buried)

yearcode <- rep(1,dim(disp)[1])
mast <- rep(NA,dim(disp)[1])
for (i in 1:length(yearcode)){
  if(disp$year12[i]==1){yearcode[i]=2}
  if(disp$year13[i]==1){yearcode[i]=3}
  if(disp$year14[i]==1){yearcode[i]=4}
  mast[i] <- mean(mastbo[yearcode[i]],mastwo[yearcode[i]])
}

################################################################

#Probability of being eaten if not dispersed

keep <- which(disp$removed==0)
ndisp <- disp[keep,]
year.ndisp <- yearcode[keep]
mast.ndisp <- mast[keep]

library(MASS)
#treat x yearly
ue <- glm(eaten~wo+shelter+mast.ndisp,data=ndisp,family='binomial')
summary(ue)
#treat
ue <- glm(eaten~wo+shelter,data=ndisp,family='binomial')
summary(ue)
#yearly
ue <- glm(eaten~wo+mast.ndisp,data=ndisp,family='binomial')
summary(ue)
#neither
ue <- glm(eaten~wo,data=ndisp,family='binomial')
summary(ue)

#ue <- glmmPQL(eaten~wo+shelter+mast.ndisp,data=ndisp,family='binomial',
#              random= ~ 1 | year.ndisp)
#summary(ue)

#Test function

ueprob <- function(n, shelter,wo,lambda){
  hold <- -0.179 + 1.4714*wo + 1.609*shelter +1.2051*lambda
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}

ueprob(1,0,0,2)

#######################################################

#Dispersal distance

keep <- which(disp$removed==1)
ydisp <- disp[keep,]
year.ydisp <- yearcode[keep]
mast.ydisp <- mast[keep]

jags.data <- list(ndispacorns=dim(ydisp)[1],shelter=ydisp$shelter,species=ydisp$wo,
                  dist=ydisp$dist,mast=mast.ydisp)
params <- c('disp.mean','disp.shelter'#,'disp.species'
            ,'disp.mast','sh')
modFile <- 'development/model_dist.R'
inits <- function(){list(sh=1)}
library(jagsUI)
dist.model <- jags(data=jags.data,inits=inits,parameters.to.save=params,model.file=modFile,
                       n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

#Test function

dispdist <- function(n, shelter,lambda){
  scale <- exp(2.071-0.115*shelter+0.114*lambda)
  shape <- 1.400
  return(rweibull(n,scale=scale,shape=shape))
}

hist(dispdist(100,0,0.15))
mean(dispdist(100,1,2))

#############################################

#Caching probability

cp <- glm(buried~wo+shelter+mast.ydisp+dist,data=ydisp,family='binomial')
summary(cp)

cp <- glmmPQL(buried~dist,data=ydisp,family='binomial',
              random= ~ 1 | year.ydisp)
summary(cp)

#dist in meters
cacheprob <- function(n, dist){
  hold <- -2.4986 + 0.08858*dist + rnorm(n,0,1.663)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}

hist(cacheprob(100,15))

###############################################

#Probability of being eaten if dispersed

#treat x yearly
de <- glm(eaten~shelter+mast.ydisp+buried,data=ydisp,family='binomial')
summary(de)
#treat
de <- glm(eaten~shelter+buried,data=ydisp,family='binomial')
summary(de)
#yearly
de <- glm(eaten~mast.ydisp+buried,data=ydisp,family='binomial')
summary(de)
#neither
de <- glm(eaten~buried,data=ydisp,family='binomial')
summary(de)

deprob <- function(shelter, buried, lambda){
  hold <- 1.4956 + 0.5817*shelter + 1.1414*lambda - 5.4178*buried
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}

deprob(0,1,2)
