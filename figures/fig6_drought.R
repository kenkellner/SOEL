############################
##    Drought Figure      ##
## Figure 6 in Manuscript ##
############################

#Read in SOEL output files
load('output/casestudy_drought.Rdata')

#Get required functions
source('utility_functions.R')

#Generate dataset from output
datalist = list(dn=drought.average,d00=drought.prob0,d02=drought.prob2,d04=drought.prob4,
                d06=drought.prob6,d08=drought.prob8,d10=drought.prob10)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig6-4.pdf",width=6,height=4,family="CM Roman",pointsize=9)

#Open tiff container
tiff(filename="figures/Fig4.tif",width=5,height=5,units="in",res=300, pointsize=9,
     compression = "lzw",type='cairo')

#cols <- c(rgb(red=141,green=213,blue=18, maxColorValue=255),
#          rgb(red=241,green=194,blue=50, maxColorValue=255),
#          rgb(red=244,green=125,blue=66, maxColorValue=255))

#op <- par()

structure <- c(1,2,3,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20)
######################################################################

#Generate seedling dataset
s <- gen.dataset(datalist,'seedlingsum')
s$harvest <- as.factor(s$harvest)
s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

mns <- aggregate(x=s$seedlingsum/1000,by=list(s$scenario,s$harvest),FUN=mean)[,3]
sds <- aggregate(x=s$seedlingsum/1000,by=list(s$scenario,s$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

par(fig=c(0,1,0.43,1),mgp=c(2.5,1,0),new=FALSE,mar = c(5,4.5,1,2) + 0.1)
plot(1,type='n',xlim=c(0.5,20.5),ylim=c(.9*min(lowlim),1.2*max(uplim)),xaxt='n',xlab="",
     ylab='TNSEED (thousands)'
)
abline(v=7)
abline(v=14)

wd=0.4
for(i in 1:18){  
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=1,bg='black',pch=21)
lines(1:6,mns[1:6])
lines(8:13,mns[7:12])
lines(15:20,mns[13:18])

text(3.5,25,'NH')
text(10.5,25,'MR')
text(17.5,25,'CC')

text(20,max(uplim)*1.15,'(a)',cex=1.5)

##################################################################################

#Generate seedling dataset
s <- gen.dataset(datalist,'seedorigin',37)
s$harvest <- as.factor(s$harvest)
s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

mns <- aggregate(x=s$seedorigin,by=list(s$scenario,s$harvest),FUN=mean)[,3]
sds <- aggregate(x=s$seedorigin,by=list(s$scenario,s$harvest),FUN=sd)[,3]
uplim <- mns+sds
lowlim <- mns-sds

par(fig=c(0,1,0,0.57),mgp=c(2.5,1,0),new=TRUE,mar = c(5,4.5,1,2) + 0.1)
plot(1,type='n',xlim=c(0.5,20.5),ylim=c(.9*min(lowlim),1.2*max(uplim)),xaxt='n',xlab="",
     ylab='SAPDENS'
)
abline(v=7)
abline(v=14)

axis(1,at=structure,tick=T,srt=90,
     labels=FALSE)
text(structure,par("usr")[1],labels=rep(c('0.0','0.2','0.4','0.6','0.8','1.0'),3),
     srt=90,adj=2,xpd=T)

wd=0.4
for(i in 1:18){  
  segments(structure[i],lowlim[i],structure[i],uplim[i])
  segments(structure[i]+wd/2,lowlim[i],structure[i]-wd/2,lowlim[i])
  segments(structure[i]+wd/2,uplim[i],structure[i]-wd/2,uplim[i])
}

points(structure,mns,cex=1,bg='black',pch=21)
lines(1:6,mns[1:6])
lines(8:13,mns[7:12])
lines(15:20,mns[13:18])

text(3.5,3500,'NH')
text(10.5,3500,'MR')
text(17.5,3500,'CC')

text(20,max(uplim)*1.15,'(b)',cex=1.5)

mtext(text='Drought Probability',side=1,outer=T,line=-2.5)

dev.off()
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.18/bin/gswin64c.exe")
#embed_fonts("../dissertation/figures/fig6-4.pdf")