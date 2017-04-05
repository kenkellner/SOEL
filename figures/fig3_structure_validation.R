#################################
## Structure Validation Figure ##
##  Figure 3 in Manuscript     ##
#################################

#Load SOEL output data
load('output/casestudy_validation.Rdata')

#Create tif container
tiff(filename="figures/Fig3.tif",width=5,height=5,units="in",res=300, pointsize=9,
     compression = "lzw",type='cairo')

###################################################################################

par(mar = c(4,4.5,1,2) + 0.1)
par(fig=c(0,0.56,0.6,1),new=FALSE,mgp=c(2.5,1,0))

####################
#Basal area 

## All trees ##
#JABOWA prediction
plot(rowMeans(ba.jabowa$clearcut$ba[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylab=expression('Basal Area'~(m^{2}~ha^{-1})),main="All Trees",
     xaxt='n',xlab="",ylim=c(0,50)
)

#Plot boxes based on actual HEE data
polygon(x=c(80,80,100,100),y=c(21.7,29.9,29.9,21.7),col='gray85',border=F)
box()

#SOEL prediction
lines(rowMeans(ba.hee$clearcut$ba[1:120,]),type='l',lwd=2,lty=2)

#Plot confidence envelope
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$ba[i,]) + 1.96*sd(ba.hee$clearcut$ba[i,])
  bot[i] <- mean(ba.hee$clearcut$ba[i,]) - 1.96*sd(ba.hee$clearcut$ba[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$ba[i,]) + 1.96*sd(ba.jabowa$clearcut$ba[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$ba[i,]) - 1.96*sd(ba.jabowa$clearcut$ba[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

#Legend
legend('bottomleft',legend=c('JABOWA','SOEL','HEE Data'),lty=c(1,2,0),lwd=c(1,1,0),
       bty='n',pch=c(NA,NA,22),col=c('black','black','black'),
       pt.bg = c(NA,NA,'gray85'),pt.cex=c(NA,NA,3))

## Basal area (overstory trees) ##

#JABOWA prediction
par(fig=c(0.44,1,0.6,1),new=TRUE)
plot(rowMeans(ba.jabowa$clearcut$ba.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     main="Overstory Only",ylab="",
     xlab="",ylim=c(0,50),xaxt='n'
)

#Boxes based on actual HEE data
polygon(x=c(80,80,100,100),y=c(15.5,22.5,22.5,15.5),col='gray85',border=F)
box()

#SOEL prediction
lines(rowMeans(ba.hee$clearcut$ba.ovs[1:120,]),type='l',lwd=2,lty=2)

#Confidence envelopes
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.ovs[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$ba.ovs[i,]) + 1.96*sd(ba.hee$clearcut$ba.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$ba.ovs[i,]) - 1.96*sd(ba.hee$clearcut$ba.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$ba.ovs[i,]) + 1.96*sd(ba.jabowa$clearcut$ba.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$ba.ovs[i,]) - 1.96*sd(ba.jabowa$clearcut$ba.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

###########################################################

## Stem Density (all trees) ##

#JABOWA prediction
par(fig=c(0,0.56,0.3,0.7),new=TRUE)
plot(rowMeans(ba.jabowa$clearcut$dens[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(0,4000),
     ylab=expression('Trees'~ha^{-1}),
     xlab="",xaxt='n')

#HEE data
polygon(x=c(80,80,100,100),y=c(923,1527,1527,923),col='gray85',border=F)
box()

#SOEL and JABOWA predictions
lines(rowMeans(ba.jabowa$clearcut$dens[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$dens[1:120,]),type='l',lwd=2,lty=2)

#Confidence envelopes
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$dens[i,]) + 1.96*sd(ba.hee$clearcut$dens[i,])
  bot[i] <- mean(ba.hee$clearcut$dens[i,]) - 1.96*sd(ba.hee$clearcut$dens[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$dens[i,]) + 1.96*sd(ba.jabowa$clearcut$dens[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$dens[i,]) - 1.96*sd(ba.jabowa$clearcut$dens[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

## Overstory Trees ##

#JABOWA prediction
par(fig=c(0.44,1,0.3,0.7),new=TRUE)
plot(rowMeans(ba.jabowa$clearcut$dens.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(50,220),
     xlab="",xaxt='n',
     ylab="")

#HEE data
polygon(x=c(80,80,100,100),y=c(93,150,150,93),col='gray85',border=F)
box()

#SOEL and JABOWA predictions
lines(rowMeans(ba.jabowa$clearcut$dens.ovs[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$dens.ovs[1:120,]),type='l',lwd=2,lty=2)

#Confidence envelopes
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$dens.ovs[i,]) + 1.96*sd(ba.hee$clearcut$dens.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$dens.ovs[i,]) - 1.96*sd(ba.hee$clearcut$dens.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$dens.ovs[i,]) + 1.96*sd(ba.jabowa$clearcut$dens.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$dens.ovs[i,]) - 1.96*sd(ba.jabowa$clearcut$dens.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

################################################################################################
#Quadratic mean diameter

## All trees ##

#JABOWA prediction
par(fig=c(0,0.56,0,0.4),new=TRUE)
plot(rowMeans(ba.jabowa$clearcut$qdbh[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(10,35),
     ylab='QMD (cm)',xlab='Years Since Stand Initiation')

#HEE data
polygon(x=c(80,80,100,100),y=c(16,20.6,20.6,16),col='gray85',border=F)
box()

#SOEL and JABOWA predictions
lines(rowMeans(ba.jabowa$clearcut$qdbh[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$qdbh[1:120,]),type='l',lwd=2,lty=2)

#Confidence envelopes
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$qdbh[i,]) + 1.96*sd(ba.hee$clearcut$qdbh[i,])
  bot[i] <- mean(ba.hee$clearcut$qdbh[i,]) - 1.96*sd(ba.hee$clearcut$qdbh[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$qdbh[i,]) + 1.96*sd(ba.jabowa$clearcut$qdbh[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$qdbh[i,]) - 1.96*sd(ba.jabowa$clearcut$qdbh[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

## Overstory trees ##

#JABOWA prediction
par(fig=c(0.44,1,0,0.4),new=TRUE)
plot(rowMeans(ba.jabowa$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(30,55),
     xlab='Years Since Stand Initiation',ylab="")

#HEE data
polygon(x=c(80,80,100,100),y=c(43.5,45.9,45.9,43.5),col='gray85',border=F)
box()

#SOEL and JABOWA predictions
lines(rowMeans(ba.jabowa$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2,lty=2)

#Confidence envelopes
top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$qdbh.ovs[i,]) + 1.96*sd(ba.hee$clearcut$qdbh.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$qdbh.ovs[i,]) - 1.96*sd(ba.hee$clearcut$qdbh.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$qdbh.ovs[i,]) + 1.96*sd(ba.jabowa$clearcut$qdbh.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$qdbh.ovs[i,]) - 1.96*sd(ba.jabowa$clearcut$qdbh.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

dev.off()
