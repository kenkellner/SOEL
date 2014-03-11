#############################################################################
##Figure showing oak dbh distribution at HEE sites
#############################################################################
#Read in data
data.bo = as.numeric(read.csv('hee_oakdbh.csv',header=FALSE)[,1])
data.sm = as.numeric(read.csv('hee_oakdbh.csv',header=FALSE)[,2])

#Plot histograms of dbh for oak vs. sugar maple
par(mfrow=c(2,1),mar=c(4,4,3,2))
hist(data.bo, breaks=15,xlim=c(0,40),main="Black Oak", col='gray', xlab="DBH")
abline(v=median(data.bo,na.rm=TRUE),lwd=3)
hist(data.sm, breaks=15,xlim=c(0,40), main="Sugar Maple", col='gray28', xlab="DBH")
abline(v=median(data.sm),lwd=3)

