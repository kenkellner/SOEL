#########################################
## Code for Seedling Validation Figure ##
#########################################

#Read in/format raw HEE seedling density data
seedling <- read.csv('data/hee_seedlings.csv',header=T)[,1:9]
names(seedling) <- c('unit','plot','quad','class','species','class1','class2','class3','class4')

#Generate new variable combining unit and plot
unitplot <- paste(seedling$unit,seedling$plot,sep="")
treat <- rep('matrix',length(unitplot))

#Create a new class of all seedlings < 1.4 m height
class123 <- rowSums(seedling[,6:8])

#Bind new dataset together and format
seedling <- cbind(seedling,unitplot,treat,class123)
seedling$unitplot <- as.character(seedling$unitplot)
seedling$treat <- as.character(seedling$treat)

#Assign plots to harvest treatments (based on maps)
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

#Collapse quadrat-level measurements to plot-level measurements
collapsed <- as.data.frame(matrix(NA,nrow=length(unique(seedling$unitplot)),ncol=8))
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

#Read in simulation output if necessary
#load('data/seedlings_val_figure.Rdata')

##############################################################
#Run model(s) for no harvest treatment

seedlingsval.out <- forest.sim(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='hee',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")
seedlingsval.none.simple <- forest.sim(model='ibm', nreps=30, nyears=40, 
                               harvests=c('none'),seedlings='simple',
                               acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                            disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                               mastscenario="hee")
seedlingsval.none.simple6 <- forest.sim(model='ibm', nreps=30, nyears=40, 
                                       harvests=c('none'),seedlings='simple',maxgrowth=0.6,
                                       acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                    disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                       mastscenario="hee")
seedlingsval.none.none <- forest.sim(model='ibm', nreps=30, nyears=40, 
                                        harvests=c('none'),seedlings='none',
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.none.jabowa <- forest.sim(model='jabowa', nreps=30, nyears=40, 
                                     harvests=c('none'),seedlings='none',
                                     acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                  disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                     mastscenario="hee")


#Format output for figures
means.n <- c(

  mean(collapsed$class123[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass123[26,1:30]),
  mean(seedlingsval.none.simple$none$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.simple$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.none$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.jabowa$none$seedclass4[26,1:30])
)

se.n <- c(
  #Using SE for HEE data and SD for model data? not ideal
  sd(collapsed$class123[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='matrix'])),
  sd(seedlingsval.out$none$seedclass123[26,1:30]),
  sd(seedlingsval.none.simple$none$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='matrix'])),
  sd(seedlingsval.out$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.simple$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.none$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.jabowa$none$seedclass4[26,1:30])
)

#Run model comparisons and save
data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30)))
data$dens <- c(seedlingsval.out$none$seedclass123[26,1:30],
               seedlingsval.none.simple$none$seedclass123[26,1:30])
t.test(dens~type,data=data)
seedling.matrix <- c('A','B')

data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30),rep('modelN',30),rep('jabowa',30)))
data$dens <- c(seedlingsval.out$none$seedclass4[26,1:30],
               seedlingsval.none.simple$none$seedclass4[26,1:30],
               seedlingsval.none.none$none$seedclass4[26,1:30],
               seedlingsval.none.jabowa$none$seedclass4[26,1:30])
t1 <- aov(dens~type,data=data)
TukeyHSD(t1)
sapling.matrix <- c('A','B','A','C')

############################################################
#Run model(s) for clearcut treatment and format output

seedlingsval.clear <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                               harvests=c('clearcut'),seedlings='hee',
                                acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                mastscenario="hee")
seedlingsval.clear.simple <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('clearcut'),seedlings='simple',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.clear.simple6 <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                        harvests=c('clearcut'),seedlings='simple',maxgrowth=0.6,
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.clear.none <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                         harvests=c('clearcut'),seedlings='none',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")
seedlingsval.clear.jabowa <- forest.sim(model='jabowa', nreps=30, nyears=30, burnin=20,
                                      harvests=c('clearcut'),seedlings='none',
                                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                   disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                      mastscenario="hee")

means.c <- c(
  
  mean(collapsed$class123[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  mean(seedlingsval.clear.simple$clearcut$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.simple$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.none$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.jabowa$clearcut$seedclass4[26,1:30])
)

se.c <- c(
  
  sd(collapsed$class123[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='clear'])),
  sd(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  sd(seedlingsval.clear.simple$clearcut$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='clear'])),
  sd(seedlingsval.clear$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.simple$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.none$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.jabowa$clearcut$seedclass4[26,1:30])
)

#Run model comparisons and save
data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30)))
data$dens <- c(seedlingsval.clear$clearcut$seedclass123[26,1:30],
               seedlingsval.clear.simple$clearcut$seedclass123[26,1:30])
t.test(dens~type,data=data)
seedling.clear <- c('A','B')

data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30),rep('modelN',30),rep('jabowa',30)))
data$dens <- c(seedlingsval.clear$clearcut$seedclass4[26,1:30],
               seedlingsval.clear.simple$clearcut$seedclass4[26,1:30],
               seedlingsval.clear.none$clearcut$seedclass4[26,1:30],
               seedlingsval.clear.jabowa$clearcut$seedclass4[26,1:30])
t1 <- aov(dens~type,data=data)
TukeyHSD(t1)
sapling.clear <- c('A','B','C','B')

############################################################
#Run model(s) for shelterwood treatment and format output
seedlingsval.shelt <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('shelterwood'),seedlings='hee',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.shelt.simple <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                 harvests=c('shelterwood'),seedlings='simple',
                                 acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                              disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                 mastscenario="hee")
seedlingsval.shelt.simple6 <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                        harvests=c('shelterwood'),seedlings='simple',maxgrowth=0.6,
                                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                     disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                        mastscenario="hee")
seedlingsval.shelt.none <- forest.sim(model='ibm', nreps=30, nyears=30, burnin=20,
                                         harvests=c('shelterwood'),seedlings='none',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")
seedlingsval.shelt.jabowa <- forest.sim(model='jabowa', nreps=30, nyears=30, burnin=20,
                                         harvests=c('shelterwood'),seedlings='simple',
                                         acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                      disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                                         mastscenario="hee")

means.s <- c(
  
  mean(collapsed$class123[collapsed$treat=="shelter"]/16*10000),
  mean(seedlingsval.shelt$shelterwood$seedclass123[26,1:30]),
  mean(seedlingsval.shelt.simple$shelterwood$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="shelter"]/16*10000),
  mean(seedlingsval.shelt$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.simple$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.none$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.jabowa$shelterwood$seedclass4[26,1:30])
)

se.s <- c(
  
  sd(collapsed$class123[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='shelter'])),
  sd(seedlingsval.shelt$shelterwood$seedclass123[26,1:30]),
  sd(seedlingsval.shelt.simple$shelterwood$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='shelter'])),
  sd(seedlingsval.shelt$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.simple$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.none$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.jabowa$shelterwood$seedclass4[26,1:30])
  
)

#Run model comparisons and save
data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30)))
data$dens <- c(seedlingsval.shelt$shelterwood$seedclass123[26,1:30],
               seedlingsval.shelt.simple$shelterwood$seedclass123[26,1:30])
t.test(dens~type,data=data)
seedling.shelt <- c('A','B')

data <- list()
data$type <- as.factor(c(rep('modelH',30),rep('modelS',30),rep('modelN',30),rep('jabowa',30)))
data$dens <- c(seedlingsval.shelt$shelterwood$seedclass4[26,1:30],
               seedlingsval.shelt.simple$shelterwood$seedclass4[26,1:30],
               seedlingsval.shelt.none$shelterwood$seedclass4[26,1:30],
               seedlingsval.shelt.jabowa$shelterwood$seedclass4[26,1:30])
t1 <- aov(dens~type,data=data)
TukeyHSD(t1)
sapling.shelt <- c('A','B','A','C')

#############################

#Save all model output
#save('seedlingsval.out','seedlingsval.none.simple','seedlingsval.none.simple6',
#     'seedlingsval.none.none','seedlingsval.none.jabowa','seedlingsval.clear',
#     'seedlingsval.clear.simple','seedlingsval.clear.simple6','seedlingsval.clear.none',
#     'seedlingsval.clear.jabowa','seedlingsval.shelt','seedlingsval.shelt.simple',
#     'seedlingsval.shelt.simple6','seedlingsval.shelt.none','seedlingsval.shelt.jabowa',
#     file='data/seedlings_val_figure.Rdata')

#############################
#Combine figure output

comb1 <- c(means.n[1:3],means.c[1:3],means.s[1:3])
comb1.se <- c(se.n[1:3],se.c[1:3],se.s[1:3])
comb2 <- c(means.n[4:8],means.c[4:8],means.s[4:8])
comb2.se <- c(se.n[4:8],se.c[4:8],se.s[4:8])


#############################
#Figure code that includes HEE data as a separate point

par(mfrow=c(2,1),
    mar=c(4.1,4.1,2,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

structure <- c(2,3,4,6,7,8,10,11,12)

plot(structure,comb1,pch=rep(c(21,19,19),3),col=rep(c('black','black','gray55'),3),
     cex=1,ylim=c(0,10000),xlim=c(1.5,12.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1}),main='Oak Seedling Density')
axis(1,at=c(3,7,11),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)
box()
legend('topleft',pch=c(21,19,19,17,24),col=c('black','black','gray55'),cex=1,
       legend=c('HEE Sites','Model H','Model S'))
for (i in 1:9){
  segments(x0=structure[i],y0=comb1[i]-comb1.se[i],x1=structure[i],y1=comb1[i]+comb1.se[i])
  segments(x0=structure[i]-0.1,y0=comb1[i]+comb1.se[i],x1=structure[i]+0.1,y1=comb1[i]+comb1.se[i])
  segments(x0=structure[i]-0.1,y0=comb1[i]-comb1.se[i],x1=structure[i]+0.1,y1=comb1[i]-comb1.se[i])
}
abline(v=5)
abline(v=9)

structure <- c(2,3,4,5,6,8,9,10,11,12,14,15,16,17,18)

plot(structure,comb2,pch=rep(c(21,19,19,17,24),3),col=rep(c('black','black','gray55','black','black'),3),
     cex=1,ylim=c(0,950),xlim=c(1.5,18.5),xaxt='n',xlab=''
     ,ylab=expression('Saplings'~ha^{-1}),main='Oak Sapling Density')
axis(1,at=c(4,10,16),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)
box()
#legend('topright',pch=c(21,19),cex=1,legend=c('HEE Data','Model'))
for (i in 1:15){
  segments(x0=structure[i],y0=comb2[i]-comb2.se[i],x1=structure[i],y1=comb2[i]+comb2.se[i])
  segments(x0=structure[i]-0.1,y0=comb2[i]+comb2.se[i],x1=structure[i]+0.1,y1=comb2[i]+comb2.se[i])
  segments(x0=structure[i]-0.1,y0=comb2[i]-comb2.se[i],x1=structure[i]+0.1,y1=comb2[i]-comb2.se[i])
}
legend('topleft',pch=c(17,24),col=c('black','black'),cex=1,
       legend=c('Model N','JABOWA'))
abline(v=7)
abline(v=13)

#################################################
#Alternate figure with HEE data as shaded area

comb1.new <- comb1[c(2:3,5:6,8:9)]
comb1.se.new <- comb1.se[c(2:3,5:6,8:9)]

par(mfrow=c(2,1),
    mar=c(4.1,4.1,2,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

structure <- c(2,3,5,6,8,9)

diff <- c(seedling.matrix,seedling.clear,seedling.shelt)

plot(structure,comb1.new,ylim=c(0,10000),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1}),main='Oak Seedling Density')
axis(1,at=c(2.5,5.5,8.5),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)
legend('topleft',pch=c(21,21,22,22),pt.bg=c('white','black','white','black'),cex=1,
       legend=c('Model H','Model S','Model N','JABOWA'),bty='n')

polygon(x=c(0,0,4,4),y=c(comb1[1]-comb1.se[1]*1.96,comb1[1]+comb1.se[1]*1.96,
                         comb1[1]+comb1.se[1]*1.96,comb1[1]-comb1.se[1]*1.96),col='gray85',border=F)
segments(x0=0,y0=comb1[1],x1=4,y1=comb1[1],lty=2)
polygon(x=c(4,4,7,7),y=c(comb1[4]-comb1.se[4]*1.96,comb1[4]+comb1.se[4]*1.96,
                         comb1[4]+comb1.se[4]*1.96,comb1[4]-comb1.se[4]*1.96),col='gray85',border=F)
segments(x0=4,y0=comb1[4],x1=7,y1=comb1[4],lty=2)
polygon(x=c(7,7,10,10),y=c(comb1[7]-comb1.se[7]*1.96,comb1[7]+comb1.se[7]*1.96,
                         comb1[7]+comb1.se[7]*1.96,comb1[7]-comb1.se[7]*1.96),col='gray85',border=F)
segments(x0=7,y0=comb1[7],x1=10,y1=comb1[7],lty=2)

box()
adjust <- c(600,500,1000,600,600,600)
for (i in 1:3){
  segments(x0=structure[i],y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i],y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]+comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]-comb1.se.new[i])
  text(structure[i],comb1.new[i]+comb1.se.new[i]+adjust[i],diff[i],cex=0.8)
}
points(structure,comb1.new,pch=21,bg=rep(c('white','black'),3),
       cex=1)
abline(v=4)
abline(v=7)

#Bottom figure
comb2.new <- comb2[c(2:5,7:10,12:15)]
comb2.se.new <- comb2.se[c(2:5,7:10,12:15)]

structure <- c(2,3,4,5,7,8,9,10,12,13,14,15)

diff <- c(sapling.matrix,sapling.clear,sapling.shelt)

plot(structure,comb2.new,ylim=c(0,1100),xlim=c(1.5,15.5),xaxt='n',xlab=''
     ,ylab=expression('Saplings'~ha^{-1}),main='Oak Sapling Density',pch=rep(c(21,21,22,22),3),
     cex=1)
axis(1,at=c(3.5,8.5,13.5),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)

polygon(x=c(0,0,6,6),y=c(comb2[1]-comb2.se[1]*1.96,comb2[1]+comb2.se[1]*1.96,
                         comb2[1]+comb2.se[1]*1.96,comb2[1]-comb2.se[1]*1.96),col='gray85',border=F)
segments(x0=0,y0=comb2[1],x1=6,y1=comb2[1],lty=2)
polygon(x=c(6,6,11,11),y=c(comb2[6]-comb2.se[6]*1.96,comb2[6]+comb2.se[6]*1.96,
                         comb2[6]+comb2.se[6]*1.96,comb2[6]-comb2.se[6]*1.96),col='gray85',border=F)
segments(x0=6,y0=comb2[6],x1=11,y1=comb2[6],lty=2)
polygon(x=c(11,11,16,16),y=c(comb2[11]-comb2.se[11]*1.96,comb2[11]+comb2.se[11]*1.96,
                           comb2[11]+comb2.se[11]*1.96,comb2[11]-comb2.se[11]*1.96),col='gray85',border=F)
segments(x0=11,y0=comb2[11],x1=16,y1=comb2[11],lty=2)

adjust <- c(70,70,70,70,70,70,85,70,70,70,70,70)
for (i in 1:12){
  segments(x0=structure[i],y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i],y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]+comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]-comb2.se.new[i])
  text(structure[i],comb2.new[i]+comb2.se.new[i]+adjust[i],diff[i],cex=0.8)
}

points(structure,comb2.new,pch=rep(c(21,21,22,22),3),bg=rep(c('white','black','white','black'),3),
       cex=1)

box()
abline(v=6)
abline(v=11)

#################################################
#Alternate figure without model s and n

#library(extrafont)
#font_install('fontcm')
#loadfonts()
pdf(file="../dissertation/figures/fig5-7.pdf",width=3.9,height=5,family="CM Roman",pointsize=9)

comb1.new <- comb1[c(2,5,8)]
comb1.se.new <- 1.96*comb1.se[c(2,5,8)]

#par(mfrow=c(2,1),
#    mar=c(4.1,4.1,2,0),
#    oma=c(0,0,1,1),
#    mgp=c(2.5,1,0))

par(mar = c(3.5,4.5,1,2) + 0.1)
par(fig=c(0,1,0.45,1),new=FALSE,mgp=c(2.5,1,0))

structure <- c(2.5,5.5,8.5)

diff <- c(seedling.matrix,seedling.clear,seedling.shelt)

plot(structure,comb1.new,ylim=c(0,8000),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1})
     #,main='Oak Seedling Density'
     )
#text(1.7,7700,'(a)',cex=1.5)
#axis(1,at=c(2.5,5.5,8.5),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)


#shadecol <- rgb(red=141,green=213,blue=18, maxColorValue=255)
shadecol <- 'gray85'
polygon(x=c(0,0,4,4),y=c(comb1[1]-comb1.se[1]*1.96,comb1[1]+comb1.se[1]*1.96,
                         comb1[1]+comb1.se[1]*1.96,comb1[1]-comb1.se[1]*1.96),col=shadecol,border=F)
segments(x0=0,y0=comb1[1],x1=4,y1=comb1[1],lty=2)
polygon(x=c(4,4,7,7),y=c(comb1[4]-comb1.se[4]*1.96,comb1[4]+comb1.se[4]*1.96,
                         comb1[4]+comb1.se[4]*1.96,comb1[4]-comb1.se[4]*1.96),col=shadecol,border=F)
segments(x0=4,y0=comb1[4],x1=7,y1=comb1[4],lty=2)
polygon(x=c(7,7,10,10),y=c(comb1[7]-comb1.se[7]*1.96,comb1[7]+comb1.se[7]*1.96,
                           comb1[7]+comb1.se[7]*1.96,comb1[7]-comb1.se[7]*1.96),col=shadecol,border=F)
segments(x0=7,y0=comb1[7],x1=10,y1=comb1[7],lty=2)

box()
adjust <- c(600,500,1000,600,600,600)
for (i in 1:3){
  segments(x0=structure[i],y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i],y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]+comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]-comb1.se.new[i])
  #text(structure[i],comb1.new[i]+comb1.se.new[i]+adjust[i],diff[i],cex=0.8)
}
points(structure,comb1.new,pch=21,bg=rep(c('white','white'),3),
       cex=1)
abline(v=4)
abline(v=7)

legend('topleft',pch=c(21,21,22),pt.bg=c('white','black',shadecol),
       pt.cex=c(1,1,3),
       legend=c('SOEL','JABOWA', 'HEE Data'),bty='n')

#Bottom figure
comb2.new <- comb2[c(2,5,7,10,12,15)]
comb2.se.new <- 1.96*comb2.se[c(2,5,7,10,12,15)]

structure <- c(2,3,5,6,8,9)

diff <- c(sapling.matrix,sapling.clear,sapling.shelt)

par(fig=c(0,1,0,0.55),new=TRUE)

plot(structure,comb2.new,ylim=c(0,1100),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Saplings'~ha^{-1}),
     #main='Oak Sapling Density'
     pch=rep(c(21,21),3),
     cex=1)
#text(1.7,1050,'(b)',cex=1.5)
axis(1,at=c(2.5,5.5,8.5),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)

polygon(x=c(0,0,4,4),y=c(comb2[1]-comb2.se[1]*1.96,comb2[1]+comb2.se[1]*1.96,
                         comb2[1]+comb2.se[1]*1.96,comb2[1]-comb2.se[1]*1.96),col=shadecol,border=F)
segments(x0=0,y0=comb2[1],x1=4,y1=comb2[1],lty=2)
polygon(x=c(4,4,7,7),y=c(comb2[6]-comb2.se[6]*1.96,comb2[6]+comb2.se[6]*1.96,
                           comb2[6]+comb2.se[6]*1.96,comb2[6]-comb2.se[6]*1.96),col=shadecol,border=F)
segments(x0=4,y0=comb2[6],x1=7,y1=comb2[6],lty=2)
polygon(x=c(7,7,10,10),y=c(comb2[11]-comb2.se[11]*1.96,comb2[11]+comb2.se[11]*1.96,
                             comb2[11]+comb2.se[11]*1.96,comb2[11]-comb2.se[11]*1.96),col=shadecol,border=F)
segments(x0=7,y0=comb2[11],x1=10,y1=comb2[11],lty=2)

adjust <- c(70,70,70,70,70,70,85,70,70,70,70,70)
for (i in 1:6){
  segments(x0=structure[i],y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i],y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]+comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]-comb2.se.new[i])
  #text(structure[i],comb2.new[i]+comb2.se.new[i]+adjust[i],diff[i],cex=0.8)
}

points(structure,comb2.new,pch=rep(c(21,21),3),bg=rep(c('white','black','white','black'),3),
       cex=1)

box()
abline(v=4)
abline(v=7)

dev.off()

