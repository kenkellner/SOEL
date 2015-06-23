
##Site quality Example Figure
png(filename='site.png',type='cairo',units='in',
    width=6,height=6.3,pointsize=12,res=96)
par(mfrow=c(2,2),
    mar=c(5.1,4.1,2.6,0),
    oma=c(1,0,1,1))

x <- seq(1980,5500,1)
y <- 4 * (x - 1977)*(5894 - x) / ((5894 - 1977)^2)
plot(x,y,type='l',lwd=2,xlab="Degree-Days", main="Degree-Days",
     ylab="Growth Response")

x <- seq(0,400,1)
y <- (-0.6 + 1 * (2.94 * (1 - 10 ^(-1 * 0.00234 * (x + 117.52)))))/2.19
plot(x,y,type='l',lwd=2,xlab="Soil Nitrogen (kg/ha)", main="Soil Fertility",
     ylab="")

x <- seq(0,0.4,0.01)
y <- 1 - (x / 0.45)^2
plot(x,y,type='l',lwd=2,xlab="Wilt Index", main="Wilt Potential",
     ylab="Growth Response")

x <- seq(0.5,10,0.01)
y <- 1 - (0.933 / x)
plot(x,y,type='l',lwd=2,xlab="Depth to Water Table (m)", main="Soil Saturation",
     ylab="",ylim=c(0,1.01),xlim=c(0,10))
dev.off()

###############################################################################

##k-value Figure

dbh.input <- seq(1,100,1)
#k4 = 0.0000756
k3 = 1/6000
k2 = 1/4000
k1 = 1/2000


approach1 <- (PHI * exp(-1 * k1 * C * dbh.input ^ 2))
approach2 <- (PHI * exp(-1 * k2 * C *dbh.input ^ 2))
approach3 <- (PHI * exp(-1 * k3 * C *dbh.input ^ 2))

plot(dbh.input,approach1,type='l',lwd=2,ylab="% Light Available Under Tree",xlab="Tree DBH (cm)")
lines(dbh.input,approach2,type='l',lty=2,lwd=2)
lines(dbh.input,approach3,type='l',lty=3,lwd=3)
legend('topright',legend=c('1/2000','1/4000','1/6000'),lty=c(1,2,3),lwd=2,
       title=expression('Value of'~italic(k)))

################################################################################

##Light tolerance figure

light <- seq(0,1,0.01)

ylow <- 2.24*(1 - exp(-1.136 * (light-0.08)))

yhigh <- 1 - exp(-4.64*(light-0.05))

#Calculate average values for curve fitting

yint <- rowMeans(cbind(ylow,yhigh))

#Fit to logarithmic response curve

fit = nls(yint ~ a*(1 - exp(b*(light - 0.05))), 
          start = list(a=1,b=-4),control=list(maxiter=10000))

#Equation for intermediate tolerance
yint.fit <- 1.371*(1 - exp(-2.227*(light-0.05)))

#Check on plot - it works!

plot(light,ylow,type='l',ylab=expression('Growth response to light'~italic('fAL')),
     xlab="Light",lwd=2,ylim=c(0,1.6))
lines(light,yint.fit,type='l',lty=2,lwd=2)
lines(light,yhigh,type='l',lty=3,lwd=2)
legend("topleft",legend=c('Low','Intermediate','High'),lty=c(1,2,3),lwd=2,title="Shade Tolerance")

################################################################################

##Density Figure

par(mfrow=c(1,2),
    mar=c(4.1,5.1,1.6,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

yearly <- read.csv('data/densityfigure_yearly_ba.csv',header=F)
#in row 2, 1 = total basal area, 2 = ba-oak, 3 = ba-maple, 4 = ba-pop
yearly <- yearly[,yearly[2,]==1]

values <- c(1,2,3,3.5,4,5)

yearly.mean <- matrix(NA,nrow=161,ncol=6)

for (i in 1:length(values)){
  
  hold <- yearly[,yearly[1,]==values[i]]
  
  for (j in 1:161){
    
    yearly.mean[j,i] = mean(as.numeric(hold[j+2,]))
    
  }
}

plot(yearly.mean[,1],type='l',lty=1,
     ylab=expression('Mean Basal Area'~(m^{2}/ha)),
     xlab="Year",xlim=c(0,185))
lines(yearly.mean[,2],type='l',lty=1)
lines(yearly.mean[,3],type='l',lty=1)
lines(yearly.mean[,4],type='l',lty=1,lwd=3)
lines(yearly.mean[,5],type='l',lty=1)
lines(yearly.mean[,6],type='l',lty=1)
abline(h=mean(as.numeric(yearly[3,])),lty=2)
text(77,73,expression(italic(d)~'= 1'))
text(95,49,expression(italic(d)~'= 2'))
#text(113,33.5,'D=3')
#text(123,29,'D=3.5')

#text(170,yearly.mean[161,1]+1,'d=1')
#text(170,yearly.mean[161,2],'d=2')
text(173,yearly.mean[161,3]+1,expression(italic(d)~'= 3'))
text(176.5,yearly.mean[161,4]+1,expression(bolditalic(d)~bold('= 3.5')),font=2)
text(150,24.5,expression(italic(d)~'= 4'))
text(125,15,expression(italic(d)~'= 5'))
arrows(x0=10,y0=50,x1=10,y1=31)
text(13,53,'Harvest')
text(10,80,'(a)',font=2,cex=1.5)

final <- read.csv('data/densityfigure_final_ba.csv',header=T)

bardata <- as.matrix(final[c(2,3,1),2:7])


barplot(bardata,ylim=c(0,45),ylab=expression('Mean Final Basal Area'~(m^{2}/ha)),
        xlab=expression("Density Parameter Value"~italic(d)),
        names.arg=c('1','2','3',expression(bold('3.5')),'4','5'),
        legend.text=c('Oak','Tulip Poplar','Sugar Maple'))
#abline(h=29.2,lty=2)
#abline(h=27.91,lty=2)
abline(h=mean(as.numeric(yearly[3,])),lty=2)
ht <- 28.464876
segments(x0=3.8,y0=0,x1=3.8,y1=ht,lwd=4)
segments(x0=4.8,y0=0,x1=4.8,y1=ht,lwd=4)
segments(x0=3.8,y0=0,x1=4.8,y1=0,lwd=4)
segments(x0=3.8,y0=ht,x1=4.8,y1=ht,lwd=4)
text(0.5,42.5,'(b)',cex=1.5,font=2)

################################################################################

#Stump sprout figure

dbh <- seq(0.05,0.8,0.01)
dbhcm <- dbh*100
age <- (400 * dbh * 100) / 100
sindex <- 22

p.woak.pred <- (-53.6225 - 1.7003*log(dbh*100) - 0.00534*age*log(dbh*100) + 25.7155*log(sindex) 
                - 0.2913*sindex*log(sindex))
p.woak <- (1 + exp(-1*p.woak.pred))^(-1)

p.boak.pred <- -8.1468 - 0.00055*age*(dbh*100) + 3.1679*log(sindex)
p.boak <- (1 + exp(-1*p.boak.pred))^(-1)

p.maple <- -0.341 * log(dbh) + 0.0877
p.poplar <- 1.1832 - 1.36638 * dbh

for (i in 1:length(p.oak)){if(p.oak[i]>1){p.oak[i] = 1}}
for (i in 1:length(p.maple)){if(p.maple[i]>1){p.maple[i] = 1}}
for (i in 1:length(p.poplar)){if(p.poplar[i]>1){p.poplar[i] = 1}}

plot(dbhcm,p.boak,type='l',lty=1,ylim=c(0,1),xlim=c(0,80),lwd=2,
     ylab="Probability of Stump Sprouting",
     xlab="Tree DBH (cm)")
lines(dbhcm,p.woak,type='l',lty=1,lwd=2,col='darkgray')
lines(dbhcm,p.maple,type='l',lty=2,lwd=2)
lines(dbhcm,p.poplar,type='l',lty=3,lwd=2)
legend('topright',legend=c('Black Oak','White Oak','Sugar Maple','Tulip Poplar'),
       lwd=2,col=c('black','darkgray','black','black'),lty=c(1,1,2,3))

################################################################################

#Harvest Comparison Figure

comp <- read.csv('data/figuredata_compareharvest.csv',header=TRUE)

par(mgp=c(2.5,1,0))

plot(comp$clearcut,type='l',lwd=2,lty=1,xlab="Years",
     ylab=expression('Basal Area'~(m^{2}/ha)),ylim=c(0,42))
lines(comp$shelterwood,type='l',lwd=2,lty=2)
lines(comp$singletree,type='l',lwd=2,lty=3)
legend('topleft',c('Clearcut','Shelterwood','Single-tree'),lwd=2,lty=1:3)

arrows(x0=20,y0=32,x1=10,y1=29,length=0.1)
text(33,32,'Initial Harvest',cex=0.8)

arrows(x0=40,y0=28.5,x1=32,y1=26.5,length=0.1)
text(58,28.5,'2nd Selection Cut',cex=0.8)

arrows(x0=27,y0=21,x1=19,y1=20,length=0.1)
text(40,21,'Shelterwood\n2nd Phase',cex=0.8)

arrows(x0=38,y0=17,x1=28,y1=16,length=0.1)
text(49,17,'3rd Phase',cex=0.8)

#############################################################

#Seedling growth figure

setwd('../seedling-survival')

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$age==1)
ht <- seedling$htgrowth[keep,]

end <- numeric(dim(ht)[1])
for (i in 1:dim(ht)[1]){
  hold <- ht[i,]
  if(length(which(!is.na(hold)&hold!=0))<4){
    firstNA <- min(which(is.na(hold)),4,na.rm=TRUE)
    first0 <- min(which(hold==0),4,na.rm=TRUE)
    end[i] <- min(firstNA,first0) - 1
  } else {end[i]=4}
}
#Only keep seedlings which have at least one recorded growth in height
#(i.e., did not die in period 2)
keep2 <- which(end>0)
age <- seedling$seedling.data$age[keep][keep2]
growth <- seedling$htgrowth[keep,]
growth <- growth[keep2,]

sprout.raw <- seedling$sprout[keep,]

#Keep track of when seedlings became sprouts
for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    start <- min(which(hold==1),na.rm=TRUE)
    sprout.raw[i,start:dim(sprout.raw)[2]] <- 1
  }
}
sprout.raw <- sprout.raw[,c(2,4,6,8)]
is.sprout <- sprout.raw[keep2,]
keep3 <- which(rowSums(is.sprout)<1&age==1)
growth <- growth[keep3,]

setwd('../oak-lifecycle')

y = matrix(NA,nrow=237,ncol=4)
shade = 0
wo = 1
browsed = 0
ymean = rnorm(237,0,1.934)
cutoff <- length(which(growth>30))/472 #- should be based on seedlings in openings
taillambda <- 1/mean(growth[which(growth>30)])

for (i in 1:237){
  for (j in 1:4){
    if(runif(1,0,1)>cutoff&shade<0.2){
      rand <- rnorm(1,0,9.151)
      y[i,j] = max(-100, 4.485 + ymean[i] - 3.014*shade + 1.730*wo 
                   + -4.892*browsed + rand)
    } else {y[i,j] <- min(rexp(1,rate=taillambda),150)}
  }
}

par(mfrow=c(2,1))
hist(y[y>-10],freq=F,xlim=c(-20,120),ylim=c(0,0.065),
     xlab="Yearly Growth (cm)",main="Predicted Seedling Growth",col='gray',breaks=15)
hist(growth[growth>-10],freq=F,xlim=c(-20,120),ylim=c(0,0.065),
     xlab="Yearly Growth (cm)",main="Actual Seedling Growth",col='gray',breaks=15)

