###################################################
## Estimate Acorn Production Parameter meanAcorn ##
###################################################

#Based on data from:
#Kellner, K. F., J. K. Riegel, and R. K. Swihart (2014). Effects of silvicultural disturbance 
#on acorn infestation and removal. New Forests 45:265–281. doi: 10.1007/s11056-014-9409-9

#Read in raw data on tree characteristics and production
tree <- read.csv('data/hee_treedata.csv',header=T)
prod <- read.csv('data/ibm_production.csv',header=T)

com <- cbind(tree,prod)[,c('species','X2006','X2007','X2008','X2009','X2010','X2011',
                          'X2012','X2013','X2014')]

#Separate by species
wo <- com[com$species==1,]
bo <- com[com$species==0,]

#############################################################################
#Acorn production estimates (no covariates)

###White Oak

#All years individually (mast-scenario = "HEE")
1 / (colMeans(wo,na.rm=T)/0.34)
womast = (1 / (colMeans(wo,na.rm=T)/0.34))[2:10]

#Mast years only (mast-scenario = "fixedgood")
hist(unlist(wo[,c(3,6,10)])/0.34)

#meanAcorn = 1/lambda.mast
lambda.mast = 1/mean(unlist(wo[,c(3,6,10)]/0.34),na.rm=TRUE)

#lambda = 0.04475813

hist(rexp(100,lambda.mast))

hist(unlist(wo[,c(3,6,10)]/0.34),freq=FALSE,col='gray',main='Mast Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.mast),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.04476"),cex=2)

#Non-mast year (mast-scenario = "fixedbad")
hist(unlist(wo[,c(2,4,8)])/0.34)
lambda.nonmast = 1/mean(unlist(wo[,c(2,4,8)]/0.34),na.rm=TRUE)

#lambda = 0.11657

hist(unlist(wo[,c(2,4,8)]/0.34),freq=FALSE,col='gray',main='Mast Failure Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.nonmast),col='red',lwd=3)
text(100,0.04,expression(lambda*" = 0.11657"),cex=2)

#Mean across years

hist(unlist(wo[,2:10])/0.34)
lambda.all = 1/mean(unlist(wo[,2:10]/0.34),na.rm=TRUE)

#lambda = 0.08518

hist(unlist(wo[,2:10]/0.34),freq=FALSE,col='gray',main='All Years Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.all),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.08518"),cex=2)

####BO

#All years individually
1 / (colMeans(bo,na.rm=T)/0.34)
bomast = (1 / (colMeans(bo,na.rm=T)/0.34))[2:10]

#Mast years
hist(unlist(bo[,c(3,6,8,10)])/0.34)

lambda.mast = 1/mean(unlist(bo[,c(3,6,8,10)]/0.34),na.rm=TRUE)

#lambda = 0.06030

hist(unlist(bo[,c(3,6,8,10)]/0.34),freq=FALSE,col='gray',main='Mast Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.mast),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.06030"),cex=2)

#Non-mast years
hist(unlist(wo[,c(4,5)])/0.34)
lambda.nonmast = 1/mean(unlist(wo[,c(4,5)]/0.34),na.rm=TRUE)

#lambda = 0.26672

hist(unlist(bo[,c(4,5)]/0.34),freq=FALSE,col='gray',main='Mast Failure Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.nonmast),col='red',lwd=3)
text(100,0.2,expression(lambda*" = 0.26672"),cex=2)

#Mean across years
hist(unlist(bo[,2:10])/0.34)
lambda.all = 1/mean(unlist(bo[,2:10]/0.34),na.rm=TRUE)

#lambda = 0.09033

hist(unlist(bo[,2:10]/0.34),freq=FALSE,col='gray',main='All Years Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.all),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.09033"),cex=2)

#####################################################################################
#Acorn production model (mast-scenario = "random")

#Exponential regression model with random effect of year

#Compile data
mastdata <- cbind(species=com[,1],com[,2:10]/0.34)

prod <- as.matrix(mastdata[,2:10])
prod[which(prod>100,arr.ind=T)]=NA

species <- as.vector(mastdata[,1])

shelter <- matrix(NA,nrow=113,ncol=9)
shelter[,1:3] <- 0
shelter[,4:9] <- treedata$shelter

jags.data <- list(prod=prod,species=species,shelter=shelter,ntrees=dim(prod)[1],
                  nyears=9)

#In final model, production varies only with year
params <- c('grand.mean',#'tree.sd',
            'year.sd'
            #'b.species','b.shelter'
            )

modFile <- 'development/model_acorn_production.R'

#Fit model in JAGS
library(jagsUI)
prod.model <- jags(data=jags.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                   n.chains=3,n.iter=10000,n.burnin=5000,n.thin=2,parallel=TRUE)
save(prod.model,file='output/development/ibm_production_output.Rdata')


#Simulate some mean acorn production values based on model parameters
sim.data <- matrix(NA,nrow=113,ncol=9)
year.eff <- vector(length=9)

for (i in 1:9){
  year.eff[i] <- rnorm(1,0,1.126)
}

for (i in 1:113){
  
  for (j in 1:9){
    
    mn <- 2.001 + year.eff[j]
    
    print(exp(mn))

    fin <- rexp(1,1/exp(mn))

    sim.data[i,j] <- fin
    
  }
}
hist(sim.data)
