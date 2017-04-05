#############################
## Figures for Appendix S1 ##
#############################

#Set ghostscript location
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.19/bin/gswin64c.exe")
library(extrafont)

#Load required model output
load('output/S1data.Rdata')

#################################################

## Figure 2: Site Quality and Light in JABOWA ##
pdf(file="appendices/appendix1/figures/fig2.pdf",width=5,height=5,family="Helvetica",pointsize=10)

par(mfrow=c(3,2),
    mar=c(4.1,4.1,2.6,0),
    oma=c(1,0,1,1))

#Degree days
x <- seq(1980,5500,1)
y <- 4 * (x - 1977)*(5894 - x) / ((5894 - 1977)^2)
plot(x,y,type='l',lwd=2,xlab="Degree-Days", main="Degree-Days",
     ylab="Growth Response")

#Nitrogen
x <- seq(0,400,1)
y <- (-0.6 + 1 * (2.94 * (1 - 10 ^(-1 * 0.00234 * (x + 117.52)))))/2.19
plot(x,y,type='l',lwd=2,xlab="Soil Nitrogen (kg/ha)", main="Soil Fertility",
     ylab="")

#Wilt index
x <- seq(0,0.4,0.01)
y <- 1 - (x / 0.45)^2
plot(x,y,type='l',lwd=2,xlab="Wilt Index", main="Wilt Potential",
     ylab="Growth Response")

#Soil Saturation
x <- seq(0.5,10,0.01)
y <- 1 - (0.933 / x)
plot(x,y,type='l',lwd=2,xlab="Depth to Water Table (m)", main="Soil Saturation",
     ylab="",ylim=c(0,1.01),xlim=c(0,10))

#Light
light <- seq(0,1,0.01)

#Equation for intermediate tolerance
yint.fit <- 1.371*(1 - exp(-2.227*(light-0.05)))

plot(light,yint.fit,type='l',ylab=expression('Growth Response'),
     xlab="Light",lwd=2,ylim=c(0,1.45),main='Light')

dev.off()
embed_fonts("appendices/appendix1/figures/fig2.pdf")

###############################################################################

## Figure 4: Stem density control parameter d ##

pdf(file="appendices/appendix1/figures/fig4.pdf",width=5,height=5.8,family="Helvetica",pointsize=10)

par(mar = c(4,4.5,0.5,1) + 0.1)
par(fig=c(0,1,0.43,1),new=FALSE,mgp=c(2.5,1,0))

#Basal area over time for different values of d
plot(yearly.basal.area[,1],type='l',lty=1,
     ylab=expression('Mean Basal Area'~(m^{2}/ha)),
     xlab="Year",xlim=c(0,185))
lines(yearly.basal.area[,2],type='l',lty=1)
lines(yearly.basal.area[,3],type='l',lty=1)
lines(yearly.basal.area[,4],type='l',lty=1,lwd=3)
lines(yearly.basal.area[,5],type='l',lty=1)
lines(yearly.basal.area[,6],type='l',lty=1)

#Labels
abline(h=mean(as.numeric(yearly.basal.area[1,])),lty=2)
text(77,73,expression(italic(d)~'= 1'),cex=0.8)
text(95,49,expression(italic(d)~'= 2'),cex=0.8)
text(173,yearly.basal.area[161,3]+1,expression(italic(d)~'= 3'),cex=0.8)
text(176.5,yearly.basal.area[161,4]+1,expression(bolditalic(d)~bold('= 3.5')),font=2,cex=0.8)
text(150,24.5,expression(italic(d)~'= 4'),cex=0.8)
text(125,15,expression(italic(d)~'= 5'),cex=0.8)
arrows(x0=10,y0=50,x1=10,y1=31)
text(13,53,'Harvest')
text(10,80,'(a)',font=1,cex=1.5)

#####

#Final basal area of different species for different values of d
bardata <- as.matrix(final.basal.area[c(2,3,1),2:7])

par(fig=c(0,1,0,0.45),new=TRUE)
barplot(bardata,ylim=c(0,45),ylab=expression('Mean Final Basal Area'~(m^{2}/ha)),
        xlab=expression("Density Parameter Value"~italic(d)),
        names.arg=c('1','2','3',expression(bold('3.5')),'4','5'),
        legend.text=c('Oak','Tulip Poplar','Sugar Maple'),
        args.legend=list(x='topright',bty='n'))
box()

#Highlight 3.5
abline(h=mean(as.numeric(yearly[3,])),lty=2)
ht <- 28.464876
segments(x0=3.8,y0=0,x1=3.8,y1=ht,lwd=4)
segments(x0=4.8,y0=0,x1=4.8,y1=ht,lwd=4)
segments(x0=3.8,y0=0,x1=4.8,y1=0,lwd=4)
segments(x0=3.8,y0=ht,x1=4.8,y1=ht,lwd=4)
text(0.5,41.5,'(b)',cex=1.5,font=1)

dev.off()
embed_fonts("appendices/appendix1/figures/fig4.pdf")

################################################################################

## Figure 5: Stump sprout probability ##

#See Appendix S1 for equation sources
dbh <- seq(0.05,0.8,0.01)
dbhcm <- dbh*100
age <- (400 * dbh * 100) / 100
sindex <- 22

#White oak
p.woak.pred <- (-53.6225 - 1.7003*log(dbh*100) - 0.00534*age*log(dbh*100) + 25.7155*log(sindex) 
                - 0.2913*sindex*log(sindex))
p.woak <- (1 + exp(-1*p.woak.pred))^(-1)

#Black oak
p.boak.pred <- -8.1468 - 0.00055*age*(dbh*100) + 3.1679*log(sindex)
p.boak <- (1 + exp(-1*p.boak.pred))^(-1)

#Maple
p.maple <- -0.341 * log(dbh) + 0.0877

#Poplar
p.poplar <- 1.1832 - 1.36638 * dbh

#Constrain probabilities to be <= 1
for (i in 1:length(p.oak)){if(p.oak[i]>1){p.oak[i] = 1}}
for (i in 1:length(p.maple)){if(p.maple[i]>1){p.maple[i] = 1}}
for (i in 1:length(p.poplar)){if(p.poplar[i]>1){p.poplar[i] = 1}}

pdf(file="appendices/appendix1/figures/fig5.pdf",width=5,height=3.9,family="Helvetica",pointsize=10)

par(mfrow=c(1,1))
par(mgp=c(3,1,0),mar = c(5,4.5,2,2) + 0.1)
plot(dbhcm,p.boak,type='l',lty=1,ylim=c(0,1),xlim=c(0,80),lwd=2,
     ylab="Probability of Stump Sprouting",
     xlab="Tree dbh (cm)")
lines(dbhcm,p.woak,type='l',lty=1,lwd=2,col='darkgray')
lines(dbhcm,p.maple,type='l',lty=2,lwd=2)
lines(dbhcm,p.poplar,type='l',lty=3,lwd=2)
legend('topright',legend=c('Black Oak','White Oak','Sugar Maple','Tulip Poplar'),
       lwd=2,col=c('black','darkgray','black','black'),lty=c(1,1,2,3))
dev.off()
embed_fonts("appendices/appendix1/figures/fig5.pdf")

################################################################################

## Figure 6: Basal area over time for different harvest treatments ##

pdf(file="appendices/appendix1/figures/fig6.pdf",width=5,height=3.9,family="Helvetica",pointsize=9)

par(mgp=c(3,1,0),mar = c(5,4.5,2,2) + 0.1)

plot(harvest.basal.area$clearcut,type='l',lwd=2,lty=1,xlab="Years",
     ylab=expression('Basal Area'~(m^{2}/ha)),ylim=c(0,42))
lines(harvest.basal.area$shelterwood,type='l',lwd=2,lty=2)
lines(harvest.basal.area$singletree,type='l',lwd=2,lty=3)
legend('topleft',c('Clearcut','Shelterwood','Single-tree'),lwd=2,lty=1:3)

arrows(x0=20,y0=32,x1=10,y1=29,length=0.1)
text(34.5,32,'Initial Harvest',cex=0.8)

arrows(x0=40,y0=28.5,x1=32,y1=26.5,length=0.1)
text(58,28.5,'2nd Selection Cut',cex=0.8)

arrows(x0=27,y0=21,x1=19,y1=20,length=0.1)
text(40,21,'Shelterwood\n2nd Phase',cex=0.8)

arrows(x0=38,y0=17,x1=28,y1=16,length=0.1)
text(49,17,'3rd Phase',cex=0.8)
dev.off()
embed_fonts("appendices/appendix1/figures/fig6.pdf")
