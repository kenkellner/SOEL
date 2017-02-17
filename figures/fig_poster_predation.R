#########################################
##  Seed Predation and Harvest Figure  ##
##      Figure 3 in Manuscript         ##
#########################################

#Read in SOEL output files
load('output/casestudy_predation.Rdata')

#Get required functions
source('utility_functions.R')

#Generate dataset from SOEL output files
datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,38)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,37)
datalist <- add.pctgermmean(datalist,30,37)

structure <- c(1,2,4,5)
par(mar = c(5,4.5,1,2) + 0.1)

cols <- rev(c(rgb(red=244,green=125,blue=66, maxColorValue=255),
              #rgb(red=241,green=194,blue=50, maxColorValue=255),
              rgb(red=141,green=213,blue=18, maxColorValue=255)))

#Open png container
png(filename="figures/fig_poster.png",width=8,height=5,units="in",res=300, pointsize=12,
     type='cairo')

############################################################

par(fig=c(0,0.51,0,1),mgp=c(2.5,1,0),new=FALSE)
h = gen.dataset(datalist,'seedlingsum')
h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$seedlingsum/1000,by=list(h$scenario,h$harvest),FUN=mean)[,3][c(1,2,5,6)]
sds <- aggregate(x=h$seedlingsum/1000,by=list(h$scenario,h$harvest),FUN=sd)[,3][c(1,2,5,6)]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,5.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     ylab=expression("New Seedlings "~ ha^{-1}~"(7 Year Sum) x 1000"),
     main='Seedlings'
     #ylab=expression('TNSEED'%*%1000)
     #ylab='TNSEED (thousands)'
)
abline(v=3)

wd=0.4
for(i in 1:4){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=cols,pch=21)

text(structure,(uplim+0.3),c('A','A','A','B'))

axis(1,at=c(1.5,4.5),tick=F,
     labels=c('No Harvest','Midstory Removal'))

#############################################################################################

#Plot number of saplings x scenario
par(fig=c(0.49,1,0,1),mgp=c(2.5,1,0),new=TRUE)
h = gen.dataset(datalist,'seedorigin',37)
h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

mns <- aggregate(x=h$seedorigin,by=list(h$scenario,h$harvest),FUN=mean)[,3][c(1,2,5,6)]
sds <- aggregate(x=h$seedorigin,by=list(h$scenario,h$harvest),FUN=sd)[,3][c(1,2,5,6)]
uplim <- mns+sds
lowlim <- mns-sds

plot(1,xlim=c(0.5,5.5),ylim=c(.95*min(lowlim),1.05*max(uplim)),xaxt='n',xlab="",
     main='Saplings',
     ylab=expression("Seed-origin Saplings "~ ha^{-1} ~"(Year 7)")
     #ylab='SAPDENS'
)
abline(v=3)

wd=0.4
for(i in 1:4){
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=2,bg=cols,pch=21)
text(structure,(uplim+4),c('A','A','B','B'))

axis(1,at=c(1.5,4.5),tick=F,
     labels=c('No Harvest','Midstory Removal'))

legend('topleft',pch=21,pt.cex=2,pt.bg=cols,legend=c('Scenario C','Scenario H'))

mtext(text='Harvest Treatment',side=1,outer=T,line=-2.5)
dev.off()
