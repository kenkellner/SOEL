####################################
## Estimate Acorn Fate Parameters ##
####################################

#Based on data from:
#Kellner, K. F., N. I. Lichti, and R. K. Swihart (2016). Midstory removal reduces effectiveness 
#of oak (Quercus) acorn dispersal by small mammals in the Central Hardwood Forest region. 
#Forest Ecology and Management 375:182–190.

######################################################################
#Read in data and correct errors/remove outliers
disp = read.csv('data/ibm_dispersal.csv',header=T)
disp <- disp[disp$clear==0,]
disp$dist[is.na(disp$dist)] <- 0
disp$dist[1973] <- 24.5
disp <- disp[!(disp$Treecode==4&disp$year12==1),]
disp <- disp[disp$squirrel==0,]

#Correct data for missing tagged seeds based on estimated detection probabilities (Kellner et al. 2016)
p.surface <- 0.75
p.buried <- 0.4
add.surface.no <- round((1-p.surface)*length(disp$removed[disp$removed==1&disp$buried==0]))
select.surface <- sample(c(1:length(disp$removed[disp$removed==1&disp$buried==0])),add.surface.no)
add.surface <- disp[disp$removed==1&disp$buried==0,][select.surface,]

add.buried.no <- round((1-p.buried)*length(disp$removed[disp$removed==1&disp$buried==1]))
select.buried <- sample(c(1:length(disp$removed[disp$removed==1&disp$buried==1])),add.buried.no)
add.buried <- disp[disp$removed==1&disp$buried==1,][select.buried,]

disp <- rbind(disp,add.surface,add.buried)

#Generate mast availability covariate
yearcode <- rep(1,dim(disp)[1])
mast <- rep(NA,dim(disp)[1])
for (i in 1:length(yearcode)){
  if(disp$year12[i]==1){yearcode[i]=2}
  if(disp$year13[i]==1){yearcode[i]=3}
  if(disp$year14[i]==1){yearcode[i]=4}
  mast[i] <- mean(mastbo[yearcode[i]],mastwo[yearcode[i]])
}

################################################################

#Estimate parameter pUndispEaten
#Probability of being eaten if not dispersed

#Select only acorns that were not dispersed
keep <- which(disp$removed==0)
ndisp <- disp[keep,]
year.ndisp <- yearcode[keep]
mast.ndisp <- mast[keep]

#Fit full model (binomial regression): 
#species + shelterwood treatment (TE) + acorn availability (YE)
library(MASS)
ue <- glm(eaten~wo+shelter+mast.ndisp,data=ndisp,family='binomial')
summary(ue)

#Test model
ueprob <- function(n, shelter,wo,lambda){
  hold <- -0.179 + 1.4714*wo + 1.609*shelter +1.2051*lambda
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}
ueprob(1,0,0,2)

#######################################################

#Estimate dispersal distance dispDist

#Select only acorns that were dispersed/removed
keep <- which(disp$removed==1)
ydisp <- disp[keep,]
year.ydisp <- yearcode[keep]
mast.ydisp <- mast[keep]

#Bundle dispersal distance data and model file for JAGS
jags.data <- list(ndispacorns=dim(ydisp)[1],shelter=ydisp$shelter,
                  dist=ydisp$dist,mast=mast.ydisp)
params <- c('disp.mean','disp.shelter','disp.mast','sh')

###########
#JAGS model file
writeLines("
model {
  
  for (i in 1:ndispacorns){
    log(dm[i]) <- disp.mean 
    + disp.shelter*shelter[i] 
    + disp.mast*mast[i]
    lam[i] <- pow((1/dm[i]),sh)
    dist[i] ~ dweib(sh,lam[i])
  }
  
  #priors
  disp.mean ~ dunif(-10,10)
  disp.shelter ~ dnorm(0,0.01)
  disp.mast ~ dnorm(0,0.01)
  sh ~ dgamma(1.0,1.0E-3)
}
", con='model_acorn_dispersaldist.txt')

modFile <- 'model_acorn_dispersaldist.txt'

###########

inits <- function(){list(sh=1)}

#Fit Weibull regression model in JAGS of dispersal distance
#as a function of shelterwood treatment (TE) and mast availability (YE)
library(jagsUI)
dist.model <- jags(data=jags.data,inits=inits,parameters.to.save=params,model.file=modFile,
                       n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

#Test fitted model
dispdist <- function(n, shelter,lambda){
  scale <- exp(2.071-0.115*shelter+0.114*lambda)
  shape <- 1.400
  return(rweibull(n,scale=scale,shape=shape))
}
hist(dispdist(100,0,0.15))
mean(dispdist(100,1,2))

#############################################

#Estimate probability of caching given dispersal pCache

#Uses same dataset from dispDist (dispersed acorns only)

#Fit full model (binomial regression): 
#species + shelterwood treatment (TE) + random year effect (YE)
cp <- glmmPQL(buried~dist,data=ydisp,family='binomial',
              random= ~ 1 | year.ydisp)
summary(cp)

#Test fitted model
cacheprob <- function(n, dist){
  hold <- -2.4986 + 0.08858*dist + rnorm(n,0,1.663)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}
hist(cacheprob(100,15))

###############################################

#Estimate probability of being eaten given dispersal pDispEaten

#Fit full model (binomial regression): 
#shelterwood treatment (TE) + acorn availability (YE) + cached status
de <- glm(eaten~shelter+mast.ydisp+buried,data=ydisp,family='binomial')
summary(de)

#Test fitted model
deprob <- function(shelter, buried, lambda){
  hold <- 1.4956 + 0.5817*shelter + 1.1414*lambda - 5.4178*buried
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}
deprob(0,1,2)
