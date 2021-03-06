######################################
##  Mast and Harvest Timing Figure  ##
##     Figure 4 in Manuscript       ##
######################################

#Read in SOEL output files
load('output/casestudy_masting.Rdata')

#Get required functions
source('utility_functions.R')

#Bind output files together
datalist = list(pg11=mast.priorgood1.1SD,pg21=mast.priorgood2.1SD,pg12=mast.priorgood1.2SD,
                pg22=mast.priorgood2.2SD,pga=mast.average)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

#Open tiff container
tiff(filename="figures/Fig4.tif",width=5,height=6,units="in",res=300, pointsize=9,
     compression = "lzw",type='cairo')

######################################################################

structure <- c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,17)

#Seedlings x scenario/harvest
par(fig=c(0,1,0.43,1),mgp=c(2.5,1,0),new=FALSE,mar = c(5,4.5,1,2) + 0.1)

s <- gen.dataset(datalist,'seedclass123',30)
s$harvest <- as.factor(s$harvest)
s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

mns <- aggregate(x=s$seedclass123/1000,by=list(s$scenario,s$harvest),FUN=mean)[,3]
sds <- aggregate(x=s$seedclass123/1000,by=list(s$scenario,s$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,17.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     ylab='SEEDDENS (thousands)'
)
abline(v=6)
abline(v=12)

wd=0.4
for(i in 1:15){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(5)),pch=21)

text(structure,(uplim+1),c('A','ABC','C','D','E','AB','BC','C','D','E','A','C','C','D','E'))

text(17,min(lowlim),'(a)',cex=1.5)

############################################################################

par(fig=c(0,1,0,0.57),mgp=c(2.5,1,0),new=TRUE,mar = c(5,4.5,1,2) + 0.1)

s <- gen.dataset(datalist,'seedorigin',37)
s$harvest <- as.factor(s$harvest)
s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

mns <- aggregate(x=s$seedorigin,by=list(s$scenario,s$harvest),FUN=mean)[,3]
sds <- aggregate(x=s$seedorigin,by=list(s$scenario,s$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,17.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',
     ylab='SAPDENS',xlab='Harvest Treatment'
)
abline(v=6)
abline(v=12)

wd=0.4
for(i in 1:15){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=rev(gray.colors(5)),pch=21)

text(structure,(uplim+20), c('A','AB','AB','AB','ABC','BCD','CD','CD','D','D','E','E','E','F','F'))
text(17,min(lowlim),'(b)',cex=1.5)

axis(1,at=c(3,9,15),tick=F,
     labels=c('No Harvest','Midstory Removal','Clearcut'))

legend("topleft",legend=c('Average','1 year +1 SD','2 years +1 SD','1 year +2 SD','2 years +2 SD'),
       pt.bg=rev(gray.colors(5)),pch=21,bg='white',ncol=2,title='Acorn Production Scenario')


dev.off()
