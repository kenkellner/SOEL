
tree <- read.csv('data/hee_treedata.csv',header=T)
prod <- read.csv('data/hee_production.csv',header=T)

com <- cbind(tree,prod)[,c('species','X2006','X2007','X2008','X2009','X2010','X2011',
                          'X2012','X2013','X2014')]

wo <- com[com$species==1,]

bo <- com[com$species==0,]

###WO

1 / (colMeans(wo,na.rm=T)/0.34) # yearly

#Mast year
hist(unlist(wo[,c(3,6,10)])/0.34)

lambda.mast = 1/mean(unlist(wo[,c(3,6,10)]/0.34),na.rm=TRUE)

#lambda = 0.04475813

hist(rexp(100,lambda.mast))

hist(unlist(wo[,c(3,6,10)]/0.34),freq=FALSE,col='gray',main='Mast Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.mast),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.04476"),cex=2)

#Non-mast year
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

1 / (colMeans(bo,na.rm=T)/0.34) # yearly

#Mast year
hist(unlist(bo[,c(3,6,8,10)])/0.34)

lambda.mast = 1/mean(unlist(bo[,c(3,6,8,10)]/0.34),na.rm=TRUE)

#lambda = 0.06030

hist(unlist(bo[,c(3,6,8,10)]/0.34),freq=FALSE,col='gray',main='Mast Year Distribution',
     xlab=expression(Acorns~per~m^{2}~canopy),xlim=c(0,200))
lines(0:200,dexp(0:200,lambda.mast),col='red',lwd=3)
text(100,0.02,expression(lambda*" = 0.06030"),cex=2)

#Non-mast year
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

#Deciding when to mast

X = 4

alpha = 8
beta = 1

p <- 1 / (1 + (X/alpha) ^ beta)
p

out <- vector(length=10)
out[1] = 0
start = 0.25
for (i in 2:10){
  out[i] <- rbinom(1,1,start)
  if (out [i] == 0){start = start + 0.25}
  if (out [i] == 1){start = 0.25}
}

