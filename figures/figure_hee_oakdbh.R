#############################################################################
##Figure showing oak dbh distribution at HEE sites
#############################################################################
#Read in data
data.bo = as.numeric(na.omit(read.csv('data/hee_oakdbh.csv',header=FALSE)[,1]))*2.54
data.sm = as.numeric(na.omit(read.csv('data/hee_oakdbh.csv',header=FALSE)[,2]))*2.54

#Plot histograms of dbh for oak vs. sugar maple
par(mfrow=c(2,1),mar=c(4,4,3,2))
hist(data.bo, breaks=15,xlim=c(0,40),main="Black Oak", col=rgb(red=75,green=142,blue=26, maxColorValue=255), xlab="DBH")
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
hist(data.sm, breaks=15,xlim=c(0,40), main="Sugar Maple", col=rgb(red=244,green=125,blue=66, maxColorValue=255), xlab="DBH")
abline(v=median(data.sm),lwd=3)

#Black and white version
par(mfrow=c(2,1),mar=c(4,4,3,2))
hist(data.bo, breaks=15,xlim=c(0,100),main="Black Oak", col="gray38", xlab="d.b.h. (cm)", freq=FALSE)
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
hist(data.sm, breaks=15,xlim=c(0,100), main="Sugar Maple", col="gray75", xlab="d.b.h. (cm)", freq=FALSE)
abline(v=median(data.sm),lwd=3)

#Dissertation Version using Computer Modern Font
#install.packages('extrafont')
library(extrafont)
font_install('fontcm')
loadfonts()

b = hist(data.bo,xlim=c(0,100),main="Black Oak", col="gray38", xlab="dbh")
b$density = b$counts/sum(b$counts)

s = hist(data.sm, breaks=15,xlim=c(0,100), main="Sugar Maple", col="gray75", xlab="dbh", freq=FALSE)
s$density = s$counts/sum(s$counts)

#library(extrafont)
#font_install('fontcm')
#loadfonts()
pdf(file="../dissertation/figures/fig1-1.pdf",width=5,height=5,family="CM Roman",pointsize=10)
par(mfrow=c(2,1),mar=c(4,4,3,2))
#hist(b,xlim=c(0,100),main="Black Oak", col="gray38", xlab="dbh", freq=F)
plot(b,xlim=c(0,100),main="Black Oak", col="gray38", xlab="dbh (cm)", freq=F,
     ylab="Proportion of Stems",ylim=c(0,.3))
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
#hist(s, breaks=15,xlim=c(0,100), main="Sugar Maple", col="gray75", xlab="dbh", freq=FALSE)
plot(s,xlim=c(0,100), main="Sugar Maple", col="gray75", xlab="dbh (cm)", freq=FALSE,
     ylab="Proportion of Stems",ylim=c(0,0.3))
abline(v=median(data.sm),lwd=3)
dev.off()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.18/bin/gswin64c.exe")
embed_fonts("../dissertation/figures/fig1-1.pdf")
