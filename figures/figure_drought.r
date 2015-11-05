#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig6-4.pdf",width=6,height=4,family="CM Roman",pointsize=9)

par(fig=c(0,0.53,0,1),mgp=c(2.5,1,0),new=FALSE)

datalist = list(dn=drought.average,d00=drought.prob0,d02=drought.prob2,d04=drought.prob4,
                d06=drought.prob6,d08=drought.prob8,d10=drought.prob10)
datalist <- add.newseedlings(datalist,30,37)
datalist <- add.seedorigin(datalist)

s <- gen.dataset(datalist,'seedlingsum')
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10','dn'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(seedlingsum~harvest*scenario,data=s,col=gray.colors(3),
             xlab="Drought Probability",
             at=c(1:18,20,21,22),
             ylab=expression("New Seedlings "~ ha^{-1}~"(7 Year Sum)"),
             xaxt='n')
axis(1,at=c(2,5,8,11,14,17,21),tick=T,
     labels=c("0","0.2",'0.4','0.6','0.8','1.0','Avg'))
abline(v=19)
#legend(12,32000,legend=c('No Harvest','Shelterwood','Clearcut'),fill=gray.colors(3))
text(1.5,1500,'(a)',cex=1.5)

#############
par(fig=c(0.47,1,0,1),mgp=c(2.5,1,0),new=TRUE)
s <- gen.dataset(datalist,'seedorigin',37)
s$harvest <- as.factor(s$harvest)

s$scenario = factor(s$scenario,c('d00','d02','d04','d06','d08','d10','dn'))
s$harvest = factor(s$harvest,c('none','shelterwood','clearcut'))

op <- par(mar = c(5,4.5,1,2) + 0.1)
bx = boxplot(seedorigin~harvest*scenario,data=s,col=gray.colors(3),
             xlab="Drought Probability",
             at=c(1:18,20,21,22),
             ylab=expression("Seed-origin Saplings "~ ha^{-1} ~"(Year 7)"),
             xaxt='n')
axis(1,at=c(2,5,8,11,14,17,21),tick=T,
     labels=c("0","0.2",'0.4','0.6','0.8','1.0','Avg'))
abline(v=19)
legend("topright",legend=c('No Harvest','Midstory Removal','Clearcut'),fill=gray.colors(3),
       bg='white')
text(1.5,100,'(b)',cex=1.5)

dev.off()