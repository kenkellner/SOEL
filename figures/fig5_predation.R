#########################################
##  Seed Predation and Harvest Figure  ##
##      Figure 5 in Manuscript         ##
#########################################

#Read in SOEL output files
load('output/casestudy_predation.Rdata')

#Get required functions
source('utility_functions.R')

#Generate dataset from SOEL output files
datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,37)
datalist <- add.pctgermmean(datalist,30,37)

structure <- c(1,2,3,4,6,7,8,9)
par(mar = c(5,4.5,1,2) + 0.1)

#Open tiff container
tiff(filename="figures/Fig5.tif",width=5,height=6,units="in",res=300, pointsize=8,
     compression = "lzw",type='cairo')

############################################################

#Plot acorns produced x scenario
par(fig=c(0,0.53,0.43,1),mgp=c(2.5,1,0),new=FALSE)
h = gen.dataset(datalist,'acornsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$acornsum/1000,by=list(h$scenario,h$harvest),FUN=mean)[,3]
sds <- aggregate(x=h$acornsum/1000,by=list(h$scenario,h$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,9.5),ylim=c(.98*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     ylab='TOTAC (thousands)')
abline(v=5)

wd=0.4
for(i in 1:8){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(4)),pch=21)

text(structure,(uplim+15),rep('A',8))
text(9,min(lowlim),'(a)',cex=1.5)

###############################################################

#Plot percent emergence x scenario
par(fig=c(0.47,1,0.43,1),mgp=c(2.5,1,0),new=TRUE)

h = gen.dataset(datalist,'pctgermmean',30)
for (i in 31:37){
  h = rbind(h,gen.dataset(datalist,'pctgermmean',i))
}

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$pctgerm,by=list(h$scenario,h$harvest),FUN=mean)[,3]
sds <- aggregate(x=h$pctgerm,by=list(h$scenario,h$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,9.5),ylim=c(.95*min(lowlim),1.2*max(uplim)),xaxt='n',xlab="",
     ylab="PCTEMR")
abline(v=5)

wd=0.4
for(i in 1:8){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(4)),pch=21)
text(structure,(uplim+0.001),c('A','A','B','BC','A','D','BC','C'))
text(9,min(lowlim),'(b)',cex=1.5)
legend("topleft",legend=c('Control','Treat Effect','Year Effect','Treat + Year'),
       pt.bg=rev(gray.colors(4)),pch=21,bg='white',ncol=2,title='Acorn Predation Scenario')

###########################################################################################

par(fig=c(0,0.53,0,0.57),mgp=c(2.5,1,0),new=TRUE)
h = gen.dataset(datalist,'seedlingsum')
h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$seedlingsum/1000,by=list(h$scenario,h$harvest),FUN=mean)[,3]
sds <- aggregate(x=h$seedlingsum/1000,by=list(h$scenario,h$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,9.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     ylab='TNSEED (thousands)')
abline(v=5)

wd=0.4
for(i in 1:8){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(4)),pch=21)

text(structure,(uplim+0.7),c('AB','AB','C','C','A','B','C','C'))
text(9,min(lowlim),'(c)',cex=1.5)

axis(1,at=c(2.5,7.5),tick=F,
     labels=c('No Harvest','Midstory Removal'))

#############################################################################################

#Plot number of saplings x scenario
par(fig=c(0.47,1,0,0.57),mgp=c(2.5,1,0),new=TRUE)
h = gen.dataset(datalist,'seedorigin',37)
h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$seedorigin,by=list(h$scenario,h$harvest),FUN=mean)[,3]
sds <- aggregate(x=h$seedorigin,by=list(h$scenario,h$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,9.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     ylab='SAPDENS')
abline(v=5)

wd=0.4
for(i in 1:8){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(4)),pch=21)
text(structure,(uplim+8),c('A','A','B','BC','BC','C','D','D'))
text(9,min(lowlim),'(d)',cex=1.5)

axis(1,at=c(2.5,7.5),tick=F,
     labels=c('No Harvest','Midstory Removal'))

mtext(text='Harvest Treatment',side=1,outer=T,line=-2.5)
dev.off()
