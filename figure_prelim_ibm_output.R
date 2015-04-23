ba.data <- read.csv("data/oak_ibm_jabowa Basal Area.csv",header=T)

plot(ba.data$x,ba.data$y,type='l',lwd=2,main="100 yr Clearcut Rotation",xlab="Year",
     ylab="Basal Area m2/ha")
abline(h=30,lty=2)
text(500, 42, "Dashed Line = Target Realistic BA")

###########################################################

prop.data <- read.csv("data/oak_ibm_jabowa Prop Oak.csv",header=T)

plot(prop.data$x,prop.data$y,type="l",lwd=2,col='black'
     ,main="100 yr Clearcut Rotation",xlab="Year",ylab="Proportion of Basal Area")

lines(prop.data$x,prop.data$y.1,type="l",lwd=2,col='blue')
lines(prop.data$x,prop.data$y.2,type="l",lwd=2,col='red')

legend(450,0.8,legend=c('Oak','Tolerant','Intolerant'),lwd=2,col=c('black','blue','red'))

ba.data <- read.csv("noharvest Basal Area.csv",header=T)

plot(ba.data$x,ba.data$y,type='l',lwd=2,main="No Harvesting",xlab="Year",
     ylab="Basal Area m2/ha")
abline(h=30,lty=2)
text(500, 42, "Dashed Line = Target Realistic BA")

###########################################################

prop.data <- read.csv("data/noharvest Prop Oak.csv",header=T)

plot(prop.data$x,prop.data$y,type="l",lwd=2,col='black'
     ,main="100 yr Clearcut Rotation",xlab="Year",ylab="Proportion of Basal Area")

lines(prop.data$x,prop.data$y.1,type="l",lwd=2,col='blue')
lines(prop.data$x,prop.data$y.2,type="l",lwd=2,col='red')

legend(450,0.3,legend=c('Oak','Tolerant','Intolerant'),lwd=2,col=c('black','blue','red'))


