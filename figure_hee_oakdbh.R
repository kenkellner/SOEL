#############################################################################
##Figure showing oak dbh distribution at HEE sites
#############################################################################
#Read in data
data.bo = as.numeric(read.csv('data/hee_oakdbh.csv',header=FALSE)[,1])
data.sm = as.numeric(read.csv('data/hee_oakdbh.csv',header=FALSE)[,2])

#Plot histograms of dbh for oak vs. sugar maple
par(mfrow=c(2,1),mar=c(4,4,3,2))
hist(data.bo, breaks=15,xlim=c(0,40),main="Black Oak", col=rgb(red=75,green=142,blue=26, maxColorValue=255), xlab="DBH")
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
hist(data.sm, breaks=15,xlim=c(0,40), main="Sugar Maple", col=rgb(red=244,green=125,blue=66, maxColorValue=255), xlab="DBH")
abline(v=median(data.sm),lwd=3)

#Black and white version
par(mfrow=c(2,1),mar=c(4,4,3,2))
hist(data.bo, breaks=15,xlim=c(0,40),main="Black Oak", col="gray38", xlab="DBH", freq=FALSE)
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
hist(data.sm, breaks=15,xlim=c(0,40), main="Sugar Maple", col="gray75", xlab="DBH", freq=FALSE)
abline(v=median(data.sm),lwd=3)