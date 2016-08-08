#########################################
##  Seed Predation and Harvest Figure  ##
##      Figure 3 in Manuscript         ##
#########################################

#Read in SOEL output files
setwd('output/predation')
files <- list.files()
lapply(files,load,.GlobalEnv)
setwd('../..')

#Get required functions
source('utility_functions.R')

#Generate dataset from SOEL output files
datalist <- list(avg=weevil.dispersal.average,trt=weevil.dispersal.treateff,yrly=weevil.dispersal.yearlyeff,
                 treat.yrly=weevil.dispersal.treatyearlyeff)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)
datalist <- add.acornsum(datalist,30,37)

#datalist <- list(avg=weevil.dispersal.average,trt=dispersal.treateff,yrly=dispersal.yearlyeff,
#                 treat.yrly=dispersal.treatyearlyeff)

#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig6-3.pdf",width=5,height=5,family="CM Roman",pointsize=8)

#Open tiff container
tiff(filename="figures/Fig3.tif",width=5,height=5,units="in",res=300, pointsize=8,
     compression = "lzw",type='cairo')

#Plot acorns produced x scenario
par(fig=c(0,0.53,0.43,1),mgp=c(2.5,1,0),new=FALSE)
h = gen.dataset(datalist,'acornsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(acornsum/1000~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(500,875),
             ylab=expression("Acorns "~ ha^{-1}~"(7 Year Sum) x 1000"))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend(0.5,0.04,legend=c("No Harvest","Midstory Removal"),fill=gray.colors(2))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+15),"A")
text(9.5,510,'(a)',cex=1.5)

###########
#Plot percent emergence x scenario
par(fig=c(0.47,1,0.43,1),mgp=c(2.5,1,0),new=TRUE)

h = gen.dataset(datalist,'pctgerm',30)
for (i in 31:37){
  h = rbind(h,gen.dataset(datalist,'pctgerm',i))
}

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(pctgerm~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(0.0,0.07),ylab=expression("Yearly Percent Emergence"))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
legend("topleft",legend=c("No Harvest","Midstory Removal"),fill=gray.colors(2),
       bg='white')
text(c(1,2,3.5,4.5,6,7,8.5,9.5),
     (bx$stats[5,]+c(0.005,0.005,0.005,0.0055,0.0045,0.003,0.003,0.005)),
     c('A','A','A','B','C','C','C','C'))
text(9.5,0.002,'(b)',cex=1.5)

#############
#Plot number of seedlings x scenario
par(fig=c(0,0.53,0,0.57),mgp=c(2.5,1,0),new=TRUE)
h = gen.dataset(datalist,'seedlingsum')

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

bx = boxplot(seedlingsum~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Scenario",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylim=c(10000,25000),ylab=expression("New Seedlings "~ ha^{-1}~"(7 Year Sum)"))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("C",'TE','YE','TE + YE'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),
     bx$stats[5,]+c(1200,1200,1900,1200,1200,1200,1200,1200),
     c('A','A','A','B','C','C','C','C'))
text(9.5,10500,'(c)',cex=1.5)

##########
#Plot number of saplings x scenario
par(fig=c(0.47,1,0,0.57),mgp=c(2.5,1,0),new=TRUE)
h = gen.dataset(datalist,'seedorigin',37)

h$scenario = factor(h$scenario,c('avg','trt','yrly','treat.yrly'))

bx = boxplot(seedorigin~harvest*scenario,data=h,col=gray.colors(2),xaxt='n',xlab="Scenario",
             at=c(1,2,3.5,4.5,6,7,8.5,9.5),ylab=expression("Seed-origin Saplings "~ ha^{-1} ~"(Year 7)"),
             ylim=c(80,250))
axis(1,at=c(1.5,4,6.5,9),tick=F,
     labels=c("C",'TE','YE','TE + YE'))
abline(v=mean(c(2,3.5)))
abline(v=mean(c(4.5,6)))
abline(v=mean(c(7,8.5)))
text(c(1,2,3.5,4.5,6,7,8.5,9.5),(bx$stats[5,]+c(10,10,20,20,20,10,15,10)),
     c('A','B','A','B','C','D','C','D'))
text(9.5,85,'(d)',cex=1.5)

dev.off()
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.18/bin/gswin64c.exe")
#embed_fonts("../dissertation/figures/fig6-3.pdf")

##################################
#Plot only C and TE scenarios for dissertation talk
#Seedlings
op <- par(mar = c(5,4.5,1.5,2) + 0.1)
cols <- c(rgb(red=141,green=213,blue=18, maxColorValue=255),
          rgb(red=244,green=125,blue=66, maxColorValue=255))

h = gen.dataset(datalist,'seedlingsum')
h <- h[h$scenario%in%c('avg','trt'),]

h$scenario = factor(h$scenario,c('avg','trt'))

bx = boxplot(seedlingsum~scenario*harvest,data=h,col=cols,xaxt='n',xlab="Harvest Applied",
             at=c(1,2,3.5,4.5),ylim=c(9000,17000),ylab=expression("Stems "~ ha^{-1}~"(7 Year Sum)"),
             main="Seedlings")
axis(1,at=c(1.5,4),tick=F,
     labels=c("None",'Shelterwood'))
abline(v=mean(c(2,3.5)))

text(c(1,2,3.5,4.5),
     bx$stats[5,]+c(500,500,500,500),
     c('A','A','A','B'))
legend('topleft',legend=c("Model C","Model H"),fill=cols)

##########
#Saplings
h = gen.dataset(datalist,'seedorigin',37)
h <- h[h$scenario%in%c('avg','trt'),]

h$scenario = factor(h$scenario,c('avg','trt'))

bx = boxplot(seedorigin~scenario*harvest,data=h,col=cols,xaxt='n',xlab="Harvest Applied",
             at=c(1,2,3.5,4.5),ylab=expression("Stems "~ ha^{-1} ~"(Year 7)"),
             ylim=c(80,200),main="Saplings")
axis(1,at=c(1.5,4),tick=F,
     labels=c("None",'Shelterwood'))
abline(v=mean(c(2,3.5)))
legend('topleft',legend=c("Model C","Model H"),fill=cols
       )
text(c(1,2,3.5,4.5),(bx$stats[5,]+c(10,10,10,10)),
     c('A','A','B','B'))