##########################################
## Code for Structure Validation Figure ##
##########################################

#Run models

ba.jabowa <- forest.sim(model='jabowa', nreps=30, nyears=122, burnin=1, 
                                       harvests=c('clearcut'),seedlings='none',
                                       acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                                    disperse.eaten=0.704,cache.prob=0.288,
                                                    undisperse.eaten=0.538),
                                       mastscenario="hee")

ba.none <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                        harvests=c('clearcut'),seedlings='none',
                        acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                     disperse.eaten=0.704,cache.prob=0.288,
                                     undisperse.eaten=0.538),
                        mastscenario="hee")

ba.simple <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                      harvests=c('clearcut'),seedlings='simple',
                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                   disperse.eaten=0.704,cache.prob=0.288,
                                   undisperse.eaten=0.538),
                      mastscenario="hee")

ba.hee <- forest.sim(model='ibm', nreps=30, nyears=122, burnin=1,
                      harvests=c('clearcut'),seedlings='hee',
                      acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                   disperse.eaten=0.704,cache.prob=0.288,
                                   undisperse.eaten=0.538),
                      mastscenario="hee")

save('ba.jabowa','ba.none','ba.simple','ba.hee',file='data/structure_val_figure.Rdata')
#load('data/structure_val_figure.Rdata')

###################################################
#Begin Figure Code

######Basal Area

par(mfrow=c(3,2),
    mar=c(4.1,4.1,2,0),
    oma=c(0,0,1,1),
    mgp=c(2.5,1,0))

plot(rowMeans(ba.jabowa$clearcut$ba[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylab=expression('Basal Area'~(m^{2}~ha^{-1})),main="All Trees",
     xlab='Years Since Stand Initiation')
polygon(x=c(80,80,100,100),y=c(21.7,29.9,29.9,21.7),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$ba[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$ba[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$ba[i,]) + sd(ba.hee$clearcut$ba[i,])
  bot[i] <- mean(ba.hee$clearcut$ba[i,]) - sd(ba.hee$clearcut$ba[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$ba[i,]) + sd(ba.jabowa$clearcut$ba[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$ba[i,]) - sd(ba.jabowa$clearcut$ba[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')
legend('bottomleft',legend=c('JABOWA','SOEL','HEE Data'),lty=c(1,2,0),lwd=c(2,2,0),
       bty='n',pch=c(NA,NA,22),col=c('black','black','gray85'),
       pt.bg = c(NA,NA,'gray85'),pt.cex=c(NA,NA,3))

plot(rowMeans(ba.jabowa$clearcut$ba.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     main="Overstory Only",ylab="",
     xlab='Years Since Stand Initiation')
polygon(x=c(80,80,100,100),y=c(15.5,22.5,22.5,15.5),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$ba.ovs[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$ba.ovs[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.ovs[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$ba.ovs[i,]) + sd(ba.hee$clearcut$ba.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$ba.ovs[i,]) - sd(ba.hee$clearcut$ba.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$ba.ovs[i,]) + sd(ba.jabowa$clearcut$ba.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$ba.ovs[i,]) - sd(ba.jabowa$clearcut$ba.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

######Density

plot(rowMeans(ba.jabowa$clearcut$dens[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(0,5000),
     ylab=expression('Trees'~ha^{-1}),xlab='Years Since Stand Initiation')
polygon(x=c(80,80,100,100),y=c(923,1527,1527,923),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$dens[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$dens[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$dens[i,]) + sd(ba.hee$clearcut$dens[i,])
  bot[i] <- mean(ba.hee$clearcut$dens[i,]) - sd(ba.hee$clearcut$dens[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$dens[i,]) + sd(ba.jabowa$clearcut$dens[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$dens[i,]) - sd(ba.jabowa$clearcut$dens[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

plot(rowMeans(ba.jabowa$clearcut$dens.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(50,220),
     xlab='Years Since Stand Initiation',ylab="")
polygon(x=c(80,80,100,100),y=c(93,150,150,93),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$dens.ovs[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$dens.ovs[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$dens.ovs[i,]) + sd(ba.hee$clearcut$dens.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$dens.ovs[i,]) - sd(ba.hee$clearcut$dens.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$dens.ovs[i,]) + sd(ba.jabowa$clearcut$dens.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$dens.ovs[i,]) - sd(ba.jabowa$clearcut$dens.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

#######QMD

plot(rowMeans(ba.jabowa$clearcut$qdbh[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(10,35),
     ylab='QMD (cm)',xlab='Years Since Stand Initiation')
polygon(x=c(80,80,100,100),y=c(16,20.6,20.6,16),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$qdbh[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$qdbh[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$qdbh[i,]) + sd(ba.hee$clearcut$qdbh[i,])
  bot[i] <- mean(ba.hee$clearcut$qdbh[i,]) - sd(ba.hee$clearcut$qdbh[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$qdbh[i,]) + sd(ba.jabowa$clearcut$qdbh[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$qdbh[i,]) - sd(ba.jabowa$clearcut$qdbh[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

plot(rowMeans(ba.jabowa$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2,xlim=c(60,120),
     ylim=c(30,55),
     xlab='Years Since Stand Initiation',ylab="")
polygon(x=c(80,80,100,100),y=c(43.5,45.9,45.9,43.5),col='gray85',border=F)
box()
lines(rowMeans(ba.jabowa$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2)
lines(rowMeans(ba.hee$clearcut$qdbh.ovs[1:120,]),type='l',lwd=2,lty=2)

top <- bot <- top.j <- bot.j <- vector(length=length(ba.hee$clearcut$ba.dens[1:120,1]))
for (i in 1:length(ba.hee$clearcut$ba[1:120,1])){
  top[i] <- mean(ba.hee$clearcut$qdbh.ovs[i,]) + sd(ba.hee$clearcut$qdbh.ovs[i,])
  bot[i] <- mean(ba.hee$clearcut$qdbh.ovs[i,]) - sd(ba.hee$clearcut$qdbh.ovs[i,])
  top.j[i] <- mean(ba.jabowa$clearcut$qdbh.ovs[i,]) + sd(ba.jabowa$clearcut$qdbh.ovs[i,])
  bot.j[i] <- mean(ba.jabowa$clearcut$qdbh.ovs[i,]) - sd(ba.jabowa$clearcut$qdbh.ovs[i,])
}
lines(top,lty=1,col='gray45')
lines(bot,lty=1,col='gray45')
lines(top.j,lty=1,col='gray45')
lines(bot.j,lty=1,col='gray45')

