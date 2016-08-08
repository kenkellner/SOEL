#########################################
## Code for Seedling Validation Figure ##
#########################################

#Read in/format raw HEE seedling density data
seedling <- read.csv('data/hee_seedlings.csv',header=T)[,1:9]
names(seedling) <- c('unit','plot','quad','class','species','class1','class2','class3','class4')

#Generate new variable combining unit and plot
unitplot <- paste(seedling$unit,seedling$plot,sep="")
treat <- rep('matrix',length(unitplot))

#Create a new class of all seedlings < 1.4 m height
class123 <- rowSums(seedling[,6:8])

#Bind new dataset together and format
seedling <- cbind(seedling,unitplot,treat,class123)
seedling$unitplot <- as.character(seedling$unitplot)
seedling$treat <- as.character(seedling$treat)

#Assign plots to harvest treatments (based on maps)
clear <- c('3K3','3L3','3M3','3N3','3E6','3F6','3D6','3D7','3E7','3F7',
           '6G2','6H2','6I2','6J2','6J3','6L6','6M6','6N6','6M7','6N7',
           '9A6','9B6','9C6','9D6','9E6','9K5','9L5','9M5','9N5')

patch <- c('1J2','1J4','1K4','1H5','1I6','1O4','1P5',
           '7D3','7E3','7J4','7M6','7L6','7J8','7N4','7O4',
           '8G1','8G2','8E3','8F3','8E5','8D7','8J6','8K6','8O4')

shelter <- c('3G2','3H2','3H1','3T4','3U4','3S4',
             '6B6','6C6','6D6','6D7','6I5','6H6','6I6','6H7','6I7',
             '9B3','9A4','9B4','9M2','9L3','9M3')

for (i in 1:nrow(seedling)){
  if(seedling$unitplot[i]%in%clear){seedling$treat[i] <- 'clear'}
  if(seedling$unitplot[i]%in%patch){seedling$treat[i] <- 'patch'}
  if(seedling$unitplot[i]%in%shelter){seedling$treat[i] <- 'shelter'}
}

#Collapse quadrat-level measurements to plot-level measurements
collapsed <- as.data.frame(matrix(NA,nrow=length(unique(seedling$unitplot)),ncol=8))
collapsed[,1] <- as.character(unique(seedling$unitplot))
names(collapsed) <- c('unitplot','treat','class1','class2','class3','class123','class4')

for (i in 1:nrow(collapsed)){
  
  collapsed$treat[i] <- seedling[seedling$unitplot==collapsed$unitplot[i],][1,11]
  
  hold <- seedling[seedling$unitplot==collapsed$unitplot[i]&
                     seedling$species%in%c('BLO','NRO','SCO','WHO','CHO'),]
  if(nrow(hold)==0){collapsed[i,3:7] <- 0
  } else {
    collapsed$class1[i] <- sum(hold$class1,na.rm=TRUE)
    collapsed$class2[i] <- sum(hold$class2,na.rm=TRUE)
    collapsed$class3[i] <- sum(hold$class3,na.rm=TRUE)
    collapsed$class123[i] <- sum(collapsed[i,3:5],na.rm=TRUE)
    collapsed$class4[i] <- sum(hold$class4,na.rm=TRUE)
  }
  
}

#Read in simulation output if necessary
load('output/seedlings_val_figure.Rdata')

###########################################################################

#Summarize output (means and standard deviations for seedlings/saplings)

#Control
means.n <- c(
  
  mean(collapsed$class123[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass123[26,1:30]),
  mean(seedlingsval.none.simple$none$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="matrix"]/16*10000),
  mean(seedlingsval.out$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.simple$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.none$none$seedclass4[26,1:30]),
  mean(seedlingsval.none.jabowa$none$seedclass4[26,1:30])
)

se.n <- c(
  #Using SE for HEE data and SD for model data? not ideal
  sd(collapsed$class123[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='matrix'])),
  sd(seedlingsval.out$none$seedclass123[26,1:30]),
  sd(seedlingsval.none.simple$none$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='matrix'])),
  sd(seedlingsval.out$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.simple$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.none$none$seedclass4[26,1:30]),
  sd(seedlingsval.none.jabowa$none$seedclass4[26,1:30])
)

#Shelterwood
means.s <- c(
  
  mean(collapsed$class123[collapsed$treat=="shelter"]/16*10000),
  mean(seedlingsval.shelt$shelterwood$seedclass123[26,1:30]),
  mean(seedlingsval.shelt.simple$shelterwood$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="shelter"]/16*10000),
  mean(seedlingsval.shelt$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.simple$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.none$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.shelt.jabowa$shelterwood$seedclass4[26,1:30])
)

se.s <- c(
  
  sd(collapsed$class123[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='shelter'])),
  sd(seedlingsval.shelt$shelterwood$seedclass123[26,1:30]),
  sd(seedlingsval.shelt.simple$shelterwood$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='shelter'])),
  sd(seedlingsval.shelt$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.simple$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.none$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.shelt.jabowa$shelterwood$seedclass4[26,1:30])
  
)

#Clearcut
means.c <- c(
  
  mean(collapsed$class123[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  mean(seedlingsval.clear.simple$clearcut$seedclass123[26,1:30]),
  
  mean(collapsed$class4[collapsed$treat=="clear"]/16*10000),
  mean(seedlingsval.clear$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.simple$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.none$clearcut$seedclass4[26,1:30]),
  mean(seedlingsval.clear.jabowa$clearcut$seedclass4[26,1:30])
)

se.c <- c(
  
  sd(collapsed$class123[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='clear'])),
  sd(seedlingsval.clear$clearcut$seedclass123[26,1:30]),
  sd(seedlingsval.clear.simple$clearcut$seedclass123[26,1:30]),
  
  sd(collapsed$class4[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='clear'])),
  sd(seedlingsval.clear$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.simple$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.none$clearcut$seedclass4[26,1:30]),
  sd(seedlingsval.clear.jabowa$clearcut$seedclass4[26,1:30])
)

#Combine output summaries
comb1 <- c(means.n[1:3],means.c[1:3],means.s[1:3])
comb1.se <- c(se.n[1:3],se.c[1:3],se.s[1:3])
comb2 <- c(means.n[4:8],means.c[4:8],means.s[4:8])
comb2.se <- c(se.n[4:8],se.c[4:8],se.s[4:8])

############################################################################
#Generate figure

#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig5-7.pdf",width=5,height=5,family="CM Roman",pointsize=10)
pdf(file="appendices/figures/fig7.pdf",width=5,height=5,family="Helvetica",pointsize=10)

#Exclude model S and N from figure
comb1.new <- comb1[c(2,5,8)]
comb1.se.new <- 1.96*comb1.se[c(2,5,8)]

par(mar = c(3.5,4.5,1,2) + 0.1)
par(fig=c(0,1,0.45,1),new=FALSE,mgp=c(2.5,1,0))

structure <- c(2.5,5.5,8.5)

#Plot means for seedling figure
plot(structure,comb1.new,ylim=c(0,8000),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1}))

#Draw shaded boxes representing actual HEE data
shadecol <- 'gray85'
#shadecol <- rgb(red=141,green=213,blue=18, maxColorValue=255)

polygon(x=c(0,0,4,4),y=c(comb1[1]-comb1.se[1]*1.96,comb1[1]+comb1.se[1]*1.96,
                         comb1[1]+comb1.se[1]*1.96,comb1[1]-comb1.se[1]*1.96),col=shadecol,border=F)
segments(x0=0,y0=comb1[1],x1=4,y1=comb1[1],lty=2)
polygon(x=c(4,4,7,7),y=c(comb1[4]-comb1.se[4]*1.96,comb1[4]+comb1.se[4]*1.96,
                         comb1[4]+comb1.se[4]*1.96,comb1[4]-comb1.se[4]*1.96),col=shadecol,border=F)
segments(x0=4,y0=comb1[4],x1=7,y1=comb1[4],lty=2)
polygon(x=c(7,7,10,10),y=c(comb1[7]-comb1.se[7]*1.96,comb1[7]+comb1.se[7]*1.96,
                           comb1[7]+comb1.se[7]*1.96,comb1[7]-comb1.se[7]*1.96),col=shadecol,border=F)
segments(x0=7,y0=comb1[7],x1=10,y1=comb1[7],lty=2)

box()

#Plot 95% confidence intervals
for (i in 1:3){
  segments(x0=structure[i],y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i],y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]+comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]+comb1.se.new[i])
  segments(x0=structure[i]-0.06,y0=comb1.new[i]-comb1.se.new[i],
           x1=structure[i]+0.06,y1=comb1.new[i]-comb1.se.new[i])
}

#Plot points on top of intervals
points(structure,comb1.new,pch=21,bg=rep(c('white','white'),3),
       cex=1)
abline(v=4)
abline(v=7)

legend('topleft',pch=c(21,21,22),pt.bg=c('white','black',shadecol),
       pt.cex=c(1,1,3),
       legend=c('SOEL','JABOWA', 'HEE Data'),bty='n')

#Bottom figure
comb2.new <- comb2[c(2,5,7,10,12,15)]
comb2.se.new <- 1.96*comb2.se[c(2,5,7,10,12,15)]

structure <- c(2,3,5,6,8,9)

par(fig=c(0,1,0,0.55),new=TRUE)

#Plot means
plot(structure,comb2.new,ylim=c(0,1100),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Saplings'~ha^{-1}),
     pch=rep(c(21,21),3),
     cex=1)
axis(1,at=c(2.5,5.5,8.5),labels=c('Matrix','Clearcut','Shelterwood'),tick=FALSE)

#Plot shaded boxes
polygon(x=c(0,0,4,4),y=c(comb2[1]-comb2.se[1]*1.96,comb2[1]+comb2.se[1]*1.96,
                         comb2[1]+comb2.se[1]*1.96,comb2[1]-comb2.se[1]*1.96),col=shadecol,border=F)
segments(x0=0,y0=comb2[1],x1=4,y1=comb2[1],lty=2)
polygon(x=c(4,4,7,7),y=c(comb2[6]-comb2.se[6]*1.96,comb2[6]+comb2.se[6]*1.96,
                         comb2[6]+comb2.se[6]*1.96,comb2[6]-comb2.se[6]*1.96),col=shadecol,border=F)
segments(x0=4,y0=comb2[6],x1=7,y1=comb2[6],lty=2)
polygon(x=c(7,7,10,10),y=c(comb2[11]-comb2.se[11]*1.96,comb2[11]+comb2.se[11]*1.96,
                           comb2[11]+comb2.se[11]*1.96,comb2[11]-comb2.se[11]*1.96),col=shadecol,border=F)
segments(x0=7,y0=comb2[11],x1=10,y1=comb2[11],lty=2)

#Plot confidence intervals
for (i in 1:6){
  segments(x0=structure[i],y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i],y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]+comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]+comb2.se.new[i])
  segments(x0=structure[i]-0.1,y0=comb2.new[i]-comb2.se.new[i],
           x1=structure[i]+0.1,y1=comb2.new[i]-comb2.se.new[i])
}

#Plot points on top of intervals
points(structure,comb2.new,pch=rep(c(21,21),3),bg=rep(c('white','black','white','black'),3),
       cex=1)

box()
abline(v=4)
abline(v=7)

dev.off()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.19/bin/gswin64c.exe")
#embed_fonts("../dissertation/figures/fig5-7.pdf")
embed_fonts("appendices/figures/fig7.pdf")

