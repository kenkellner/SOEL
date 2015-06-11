
seedling <- read.csv('data/hee_seedlings.csv',header=T)[,1:9]
names(seedling) <- c('unit','plot','quad','class','species','class1','class2','class3','class4')

unitplot <- paste(seedling$unit,seedling$plot,sep="")
treat <- rep('matrix',length(unitplot))

class123 <- rowSums(seedling[,6:8])

seedling <- cbind(seedling,unitplot,treat,class123)
seedling$unitplot <- as.character(seedling$unitplot)
seedling$treat <- as.character(seedling$treat)

clear <- c('3K3','3L3','3M3','3N3','3E6','3F6','3D6','3D7','3E7','3F7',
           '6G2','6H2','6I2','6J2','6J3','6L6','6M6','6N6','6M7','6N7',
           '9A6','9B6','9C6','9D6','9E6','9K5','9L5','9M5','9N5')
  
patch <- c('1J2','1J4','1K4','1H5','1I6','1O4','1P5',
           '7D3','7E3','7J4','7M6','7L6','7J8','7N4','7O4',
           '8G1','8G2','8E3','8F3','8E5','8D7','8J6','8K6','8O4')
  
shelter <- c('3G2','3H2','3H1','3T4','3U4','3S4',
             '6B6','6C6','6D6','6D7','6I5','6H6','6I6','6H7','6I7',
             '9B3','9A4','9B4','9M2','9L3','9M3')


for (i in 1:nrow(seedling)){
  if(seedling$unitplot[i]%in%clear){seedling$treat[i] <- 'clear'}
  if(seedling$unitplot[i]%in%patch){seedling$treat[i] <- 'patch'}
  if(seedling$unitplot[i]%in%shelter){seedling$treat[i] <- 'shelter'}
}

totalplots <- length(unique(seedling$unitplot))

clearplots <- length(unique(seedling$unitplot[seedling$treat=='clear']))
shelterplots <- length(unique(seedling$unitplot[seedling$treat=='shelter']))
patchplots <- length(unique(seedling$unitplot[seedling$treat=='patch']))
matrixplots <- length(unique(seedling$unitplot[seedling$treat=='matrix']))

#Means and SD

collapsed <- as.data.frame(matrix(NA,nrow=length(unique(seedling$unitplot)),ncol=7))
collapsed[,1] <- as.character(unique(seedling$unitplot))
names(collapsed) <- c('unitplot','treat','class1','class2','class3','class123','class4')

for (i in 1:nrow(collapsed)){
  
  collapsed$treat[i] <- seedling[seedling$unitplot==collapsed$unitplot[i],][1,11]
  
  hold <- seedling[seedling$unitplot==collapsed$unitplot[i]&
                     seedling$species%in%c('BLO','NRO','SCO','WHO','CHO'),]
  if(nrow(hold)==0){collapsed[i,3:7] <- 0
  } else {
    collapsed$class1[i] <- sum(hold$class1,na.rm=TRUE)
    collapsed$class2[i] <- sum(hold$class2,na.rm=TRUE)
    collapsed$class3[i] <- sum(hold$class3,na.rm=TRUE)
    collapsed$class123[i] <- sum(collapsed[i,3:5],na.rm=TRUE)
    collapsed$class4[i] <- sum(hold$class4,na.rm=TRUE)
  }
  
}

seedlingsval.out <- forest.sim(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='hee',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")

#Data for figures

means.n <- c(

  mean(collapsed$class123[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass123[10:30,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass4[10:30,1:30])
)

se.n <- c(
  #Using SE for HEE data and SD for model data? not ideal
  sd(collapsed$class123[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='matrix'])),
  #sd(seedlingsval.out$none$seedclass123[10:30,1:30])/sqrt(630),
  sd(seedlingsval.out$none$seedclass123[10:30,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='matrix'])),
  sd(seedlingsval.out$none$seedclass4[10:30,1:30])/sqrt(630)
  
)
par(mfrow=c(2,1),
    mar=c(4.1,5.1,1,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

structure = c(0.7,1.9,0.7,1.9)

barplot(means[1:2],ylim=c(0,4500),col=c('black','darkgrey'),names=c('HEE Data','Model Output'),
        ylab=expression('Seedlings'~(ha^{-1})),main="Seedling Density")
for (i in 1:2){
  segments(x0=structure[i],y0=means[i],x1=structure[i],y1=means[i]+se[i])
  segments(x0=structure[i]-0.1,y0=means[i]+se[i],x1=structure[i]+0.1,y1=means[i]+se[i])
}
barplot(means[3:4],ylim=c(0,30),col=c('black','darkgrey'),names=c('HEE Data','Model Output'),
        ylab=expression('Saplings'~(ha^{-1})),main="Sapling Density")
for (i in 3:4){
  segments(x0=structure[i],y0=means[i],x1=structure[i],y1=means[i]+se[i])
  segments(x0=structure[i]-0.1,y0=means[i]+se[i],x1=structure[i]+0.1,y1=means[i]+se[i])
}

######Clearcut


seedlingsval.clear <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                               harvests=c('clearcut'),seedlings='hee',
                                acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                mastscenario="hee")

means.c <- c(
  
  mean(collapsed$class123[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass4[26,1:30])
)

se.c <- c(
  
  sd(collapsed$class123[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='clear'])),
  #sd(seedlingsval.clear$clearcut$seedclass123[26,1:30])/sqrt(30),
  sd(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='clear'])),
  sd(seedlingsval.clear$clearcut$seedclass4[26,1:30])/sqrt(30)
  
)

comb1 <- c(means.n[1:2],means.c[1:2])
comb1.se <- c(se.n[1:2],se.c[1:2])
comb2 <- c(means.n[3:4],means.c[3:4])

par(mfrow=c(2,1),
    mar=c(4.1,4.1,2,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

structure <- c(2,3,5,6)

plot(structure,comb1,pch=c(21,19,21,19),cex=1,ylim=c(0,5000),xlim=c(1.5,6.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1}),main='Seedling Density')
axis(1,at=c(2.5,5.5),labels=c('Matrix','Clearcut'),tick=FALSE)
box()
legend('topright',pch=c(21,19),cex=1,legend=c('HEE Data','Model'))
for (i in 1:4){
  segments(x0=structure[i],y0=comb1[i]-comb1.se[i],x1=structure[i],y1=comb1[i]+comb1.se[i])
  segments(x0=structure[i]-0.1,y0=comb1[i]+comb1.se[i],x1=structure[i]+0.1,y1=comb1[i]+comb1.se[i])
  segments(x0=structure[i]-0.1,y0=comb1[i]-comb1.se[i],x1=structure[i]+0.1,y1=comb1[i]-comb1.se[i])
}



comb1 <- cbind(means.n[1:2],means.c[1:2])
comb2 <- cbind(means.n[3:4],means.c[3:4])
barplot(comb1,beside=T,names=c('Matrix','Clearcut'),main='Seedling Density',
        ylab=expression('Seedlings'~ha^{-1}),legend.text=c('HEE Data','Model'),
        ylim=c(0,5000))

barplot(comb2,beside=T,names=c('Matrix','Clearcut'),main='Sapling Density',
        ylab=expression('Saplings'~ha^{-1}),legend.text=c('HEE Data','Model'),
        ylim=c(0,500),args.legend=list(x=3,y=450))
