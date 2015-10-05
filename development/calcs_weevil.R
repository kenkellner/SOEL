#Weevil Infestation probability Function

weevil = read.csv('data/ibm_weevils.csv', header=TRUE)
treedata = read.csv('data/hee_treedata.csv',header=TRUE)
prod = read.csv('data/ibm_production.csv',header=TRUE)

mastbo <- 1/(colMeans(prod[treedata$species==0,2:10],na.rm=T)/0.34)
mastwo <- 1/(colMeans(prod[treedata$species==1,2:10],na.rm=T)/0.34)

mast = i.edge = i.shelter = rep(0,dim(weevil)[1])
for (i in 1:dim(weevil)[1]){
  i.edge[i] <- treedata$edge[weevil$Treecode[i]]
  i.shelter[i] <- treedata$shelter[weevil$Treecode[i]]
  mast[i] <- mean(mastbo[weevil$Year[i]],mastwo[weevil$Year[i]])
}

#Mixed effects model
#treat x yearly
library(MASS)
wi <- glmmPQL(Infested ~ Species + i.shelter + mast, data=weevil,family='binomial',
              random=~1|Year)
summary(wi)
#treat only
wi <- glm(Infested ~ Species + i.shelter, data=weevil,family='binomial')
summary(wi)
#yearly only
wi <- glmmPQL(Infested ~ Species + mast, data=weevil,family='binomial',
              random=~1|Year)
summary(wi)
#neither
wi <- glm(Infested ~ Species, data=weevil,family='binomial')
summary(wi)

#Test function

weevprob <- function(n, shelter,wo,lambda){
  hold <- -1.08 - 0.8249*wo - 0.44380*shelter + 0.7324*lambda + rnorm(n,0,1.010)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}

hist(weevprob(100,0,0,0.15))