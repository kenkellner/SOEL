#Read in data

clear <- read.csv('data/clearcut.csv',header=T)
shelter <- read.csv('data/shelter.csv',header=T)
control <- read.csv('data/control.csv',header=T)
singletree <- read.csv('data/singletree.csv',header=T)

cols <- c(rgb(red=75,green=142,blue=26, maxColorValue=255),
          rgb(red=241,green=194,blue=50, maxColorValue=255),
          rgb(red=244,green=125,blue=66, maxColorValue=255))

shade.cols <- c(rgb(red=160,green=218,blue=95, maxColorValue=255),
                rgb(red=246,green=175,blue=140, maxColorValue=255))

#Clearcut
clear.oak <- clear[,1:10]
clear.maple <- clear[,11:20]

oak.mean <- rowMeans(clear.oak)
oak.se <- apply(clear.oak,1,sd) / sqrt(10)
oak.upper <- oak.mean + 1.96*oak.se
oak.lower <- oak.mean - 1.96*oak.se

maple.mean <- rowMeans(clear.maple)
maple.se <- apply(clear.maple,1,sd) / sqrt(10)
maple.upper <- maple.mean + 1.96*maple.se
maple.lower <- maple.mean - 1.96*maple.se

plot(1:250,oak.mean,type="l",lwd=4, col=cols[1],xlab="Year",
     ylab="Basal Area (m2/ha)",main="",ylim=c(0,55))
abline(v=50,lty=2,lwd=2)
abline(v=150,lty=2,lwd=2)
abline(v=250,lty=2,lwd=2)
polygon(c(1:250,rev(1:250)),c(oak.upper,rev(oak.lower)),col=shade.cols[1],border=NA)
polygon(c(1:250,rev(1:250)),c(maple.upper,rev(maple.lower)),col=shade.cols[2],border=NA)
lines(1:250,maple.mean,type="l",lwd=4, col=cols[3])
lines(1:250,oak.mean,type="l",lwd=4, col=cols[1])
#lines(1:250,oak.upper,type="l",lwd=1,lty=2,col=cols[1])
#lines(1:250,oak.lower,type="l",lwd=1,lty=2,col=cols[1])
legend(-3,50,c("Oak","Maple","Harvest"),lwd=c(4,4,2),col=c(cols[1],cols[3],"black"),lty=c(1,1,2))

#Control
clear.oak <- control[,1:10]
clear.maple <- control[,11:20]

oak.mean <- rowMeans(clear.oak)
oak.se <- apply(clear.oak,1,sd) / sqrt(10)
oak.upper <- oak.mean + 1.96*oak.se
oak.lower <- oak.mean - 1.96*oak.se

maple.mean <- rowMeans(clear.maple)
maple.se <- apply(clear.maple,1,sd) / sqrt(10)
maple.upper <- maple.mean + 1.96*maple.se
maple.lower <- maple.mean - 1.96*maple.se

plot(1:250,oak.mean,type="l",lwd=4, col=cols[1],xlab="Year",
     ylab="Basal Area (m2/ha)",main="",ylim=c(0,80))
#abline(v=50,lty=2,lwd=2)
#abline(v=150,lty=2,lwd=2)
#abline(v=250,lty=2,lwd=2)
polygon(c(1:250,rev(1:250)),c(oak.upper,rev(oak.lower)),col=shade.cols[1],border=NA)
polygon(c(1:250,rev(1:250)),c(maple.upper,rev(maple.lower)),col=shade.cols[2],border=NA)
lines(1:250,maple.mean,type="l",lwd=4, col=cols[3])
lines(1:250,oak.mean,type="l",lwd=4, col=cols[1])
#lines(1:250,oak.upper,type="l",lwd=1,lty=2,col=cols[1])
#lines(1:250,oak.lower,type="l",lwd=1,lty=2,col=cols[1])
legend(-3,70,c("Oak","Maple"),lwd=c(4,4,2),col=c(cols[1],cols[3],"black"),lty=c(1,1,2))


#Single-tree
clear.oak <- singletree[,1:10]
clear.maple <- singletree[,11:20]

oak.mean <- rowMeans(clear.oak)
oak.se <- apply(clear.oak,1,sd) / sqrt(10)
oak.upper <- oak.mean + 1.96*oak.se
oak.lower <- oak.mean - 1.96*oak.se

maple.mean <- rowMeans(clear.maple)
maple.se <- apply(clear.maple,1,sd) / sqrt(10)
maple.upper <- maple.mean + 1.96*maple.se
maple.lower <- maple.mean - 1.96*maple.se

plot(1:250,oak.mean,type="l",lwd=4, col=cols[1],xlab="Year",
     ylab="Basal Area (m2/ha)",main="",ylim=c(0,32))
abline(v=50,lty=2,lwd=1)
abline(v=75,lty=2,lwd=1)
abline(v=100,lty=2,lwd=1)
abline(v=c(125,175,200,225),lty=2)
abline(v=150,lty=2,lwd=1)
abline(v=250,lty=2,lwd=1)
polygon(c(1:250,rev(1:250)),c(oak.upper,rev(oak.lower)),col=shade.cols[1],border=NA)
polygon(c(1:250,rev(1:250)),c(maple.upper,rev(maple.lower)),col=shade.cols[2],border=NA)
lines(1:250,maple.mean,type="l",lwd=4, col=cols[3])
lines(1:250,oak.mean,type="l",lwd=4, col=cols[1])
#lines(1:250,oak.upper,type="l",lwd=1,lty=2,col=cols[1])
#lines(1:250,oak.lower,type="l",lwd=1,lty=2,col=cols[1])
legend(-3,20,c("Oak","Maple","Harvest"),lwd=c(4,4,1),col=c(cols[1],cols[3],"black"),lty=c(1,1,2))



#Shelter
clear.oak <- shelter[,1:10]
clear.maple <- shelter[,11:20]

oak.mean <- rowMeans(clear.oak)
oak.se <- apply(clear.oak,1,sd) / sqrt(10)
oak.upper <- oak.mean + 1.96*oak.se
oak.lower <- oak.mean - 1.96*oak.se

maple.mean <- rowMeans(clear.maple)
maple.se <- apply(clear.maple,1,sd) / sqrt(10)
maple.upper <- maple.mean + 1.96*maple.se
maple.lower <- maple.mean - 1.96*maple.se

plot(1:250,oak.mean,type="l",lwd=4, col=cols[1],xlab="Year",
     ylab="Basal Area (m2/ha)",main="",ylim=c(0,55))
abline(v=50,lty=2,lwd=2)
abline(v=150,lty=2,lwd=2)
abline(v=250,lty=2,lwd=2)
polygon(c(1:250,rev(1:250)),c(oak.upper,rev(oak.lower)),col=shade.cols[1],border=NA)
polygon(c(1:250,rev(1:250)),c(maple.upper,rev(maple.lower)),col=shade.cols[2],border=NA)
lines(1:250,maple.mean,type="l",lwd=4, col=cols[3])
lines(1:250,oak.mean,type="l",lwd=4, col=cols[1])
#lines(1:250,oak.upper,type="l",lwd=1,lty=2,col=cols[1])
#lines(1:250,oak.lower,type="l",lwd=1,lty=2,col=cols[1])
legend(-3,50,c("Oak","Maple","Harvest"),lwd=c(4,4,2),col=c(cols[1],cols[3],"black"),lty=c(1,1,2))