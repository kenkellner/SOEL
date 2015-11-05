#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig6-2.pdf",width=5.5,height=4,family="CM Roman",pointsize=9)

par(fig=c(0,0.53,0,1),mgp=c(2.5,1,0),new=FALSE)
datalist = list(pg11=mast.priorgood1.1SD,pg21=mast.priorgood2.1SD,pg12=mast.priorgood1.2SD,
                pg22=mast.priorgood2.2SD,pga=mast.average)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

s <- gen.dataset(datalist,'seedclass123',30)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(seedclass123~harvest*scenario,data=s,col=gray.colors(3),
             xlab="",
             at=c(1,2,3,4.5,5.5,6.5,8,9,10,11.5,12.5,13.5,15,16,17),
             ylab=expression("Seedlings "~ ha^{-1}~"at harvest"),
             xaxt='n',
             ylim=c(8000,34000))
axis(1,at=c(2,5.5,9,12.5,16),tick=F,
     labels=c("AVG","1YR1SD",'2YR1SD','1YR2SD','2YR2SD'),
     las=2)
text(c(2,5.5,9,12.5,16),
     bx$stats[5,c(2,5,8,11,14)]+3000,
     c('A','B','C','D','E'))

legend("topleft",legend=c('No Harvest','Midstory Removal','Clearcut'),fill=gray.colors(3))
text(16.5,9000,'(a)',cex=1.5)

#############
par(fig=c(0.47,1,0,1),mgp=c(2.5,1,0),new=TRUE)
s <- gen.dataset(datalist,'seedclass4',37)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('pga','pg11','pg21','pg12','pg22'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(seedclass4~harvest*scenario,data=s,col=gray.colors(3),
             xlab="",
             at=c(1,2,3,4.5,5.5,6.5,8,9,10,11.5,12.5,13.5,15,16,17),
             ylab=expression("Saplings "~ ha^{-1}~"(year 7)"),
             xaxt='n')
axis(1,at=c(2,5.5,9,12.5,16),tick=F,
     labels=c("AVG","1YR1SD",'2YR1SD','1YR2SD','2YR2SD'),
     las=2)

text(c(1,2,3,4.5,5.5,6.5,8,9,10,11.5,12.5,13.5,15,16,17),
     bx$stats[5,]+c(32,15,15,15,15,15,15,15,15,12,15,15,15,22,15),
      c('A','B','D','A','BC','DE','A','C','E','A','BC','E','A','C','F'),
     cex=0.8)
text(16.5,90,'(b)',cex=1.5)

dev.off()
