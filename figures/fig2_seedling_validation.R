################################
## Seedling Validation Figure ##
##  Figure 2 in Manuscript    ##
################################

#Read in/format raw HEE seedling density data
seedling <- read.csv('data/ibm_seedlings.csv',header=T)[,1:9]
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
load('output/casestudy_validation.Rdata')

###########################################################################

#Summarize output (means and standard deviations for seedlings/saplings)

#Empirical data from HEE
hee.seedling.means <- c(
  mean(collapsed$class123[collapsed$treat=="matrix"]/16*10000),
  mean(collapsed$class123[collapsed$treat=="shelter"]/16*10000),
  mean(collapsed$class123[collapsed$treat=="clear"]/16*10000)
)

hee.seedling.se <- c(
  sd(collapsed$class123[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='matrix'])),
  sd(collapsed$class123[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='shelter'])),
  sd(collapsed$class123[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class123[collapsed$treat=='clear']))
)

hee.sapling.means <- c(
  mean(collapsed$class4[collapsed$treat=="matrix"]/16*10000),
  mean(collapsed$class4[collapsed$treat=="shelter"]/16*10000),
  mean(collapsed$class4[collapsed$treat=="clear"]/16*10000)
)

hee.sapling.se <- c(
  sd(collapsed$class4[collapsed$treat=="matrix"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='matrix'])),
  sd(collapsed$class4[collapsed$treat=="shelter"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='shelter'])),
  sd(collapsed$class4[collapsed$treat=="clear"]/16*10000)/sqrt(
    length(collapsed$class4[collapsed$treat=='clear']))
)

############################
#SOEL output data

soel.seedling.means <- c(
  mean(seedlingsval.soel$none$seedclass123[35,1:30]),
  mean(seedlingsval.soel$shelterwood$seedclass123[35,1:30]),
  mean(seedlingsval.soel$clearcut$seedclass123[35,1:30])
)

soel.seedling.sd <- c(
  sd(seedlingsval.soel$none$seedclass123[35,1:30]),
  sd(seedlingsval.soel$shelterwood$seedclass123[35,1:30]),
  sd(seedlingsval.soel$clearcut$seedclass123[35,1:30])
) 

soel.sapling.means <- c(
  mean(seedlingsval.soel$none$seedclass4[35,1:30]),
  mean(seedlingsval.soel$shelterwood$seedclass4[35,1:30]),
  mean(seedlingsval.soel$clearcut$seedclass4[35,1:30])
)

soel.sapling.sd <- c(
  sd(seedlingsval.soel$none$seedclass4[35,1:30]),
  sd(seedlingsval.soel$shelterwood$seedclass4[35,1:30]),
  sd(seedlingsval.soel$clearcut$seedclass4[35,1:30])
  
)

##############################
#JABOWA output data

jabowa.sapling.means <- c(
  mean(seedlingsval.jabowa$none$seedclass4[26,1:30]),
  mean(seedlingsval.jabowa$shelterwood$seedclass4[26,1:30]),
  mean(seedlingsval.jabowa$clearcut$seedclass4[26,1:30])
)

jabowa.sapling.sd <- c(
  sd(seedlingsval.jabowa$none$seedclass4[26,1:30]),
  sd(seedlingsval.jabowa$shelterwood$seedclass4[26,1:30]),
  sd(seedlingsval.jabowa$clearcut$seedclass4[26,1:30])
)


############################################################################
#Generate figure

tiff(filename="figures/Fig2.tif",width=5,height=5,units="in",res=300, pointsize=9,
     compression = "lzw",type='cairo')

par(mar = c(3.5,4.5,1,2) + 0.1)
par(fig=c(0,1,0.45,1),new=FALSE,mgp=c(2.5,1,0))

structure <- c(2.5,5.5,8.5)

#Plot means for seedling figure
plot(structure,soel.seedling.means,ylim=c(0,9000),xlim=c(1.5,9.5),xaxt='n',xlab=''
     ,ylab=expression('Seedlings'~ha^{-1}))

#Draw shaded boxes representing actual HEE data
shadecol <- 'gray85'

st <- c(0,4,7,10)
for (i in 1:3){
  polygon(x=c(rep(st[i],2),rep(st[i+1],2)),
          y=c(hee.seedling.means[i]-hee.seedling.se[i]*1.96,hee.seedling.means[i]+hee.seedling.se[i]*1.96,
              hee.seedling.means[i]+hee.seedling.se[i]*1.96,hee.seedling.means[i]-hee.seedling.se[i]*1.96),
          col=shadecol,border=F)
  segments(x0=st[i],y0=hee.seedling.means[i],x1=st[i+1],y1=hee.seedling.means[i],lty=2)
}

box()
abline(v=4)
abline(v=7)

#Plot 95% confidence intervals
for (i in 1:3){
  segments(x0=structure[i],y0=soel.seedling.means[i]-1.96*soel.seedling.sd[i],
           x1=structure[i],y1=soel.seedling.means[i]+1.96*soel.seedling.sd[i])
  segments(x0=structure[i]-0.06,y0=soel.seedling.means[i]+1.96*soel.seedling.sd[i],
           x1=structure[i]+0.06,y1=soel.seedling.means[i]+1.96*soel.seedling.sd[i])
  segments(x0=structure[i]-0.06,y0=soel.seedling.means[i]-1.96*soel.seedling.sd[i],
           x1=structure[i]+0.06,y1=soel.seedling.means[i]-1.96*soel.seedling.sd[i])
}

#Plot points on top of intervals
points(structure,soel.seedling.means,pch=21,bg=rep(c('white','white'),3),
       cex=1)

legend(7,9500,pch=c(21,21,22),pt.bg=c('white','black',shadecol),
       pt.cex=c(1,1,3),
       legend=c('SOEL','JABOWA', 'HEE Data'),bty='n')
########################################################
#Bottom (sapling) figure

par(fig=c(0,1,0,0.55),new=TRUE)

structure <- c(2,3,5,6,8,9)

#Combine SOEl/JABOWA vectors
comb <- c(rbind(soel.sapling.means,jabowa.sapling.means))
comb.se <- c(rbind(soel.sapling.sd,jabowa.sapling.sd))

#Plot means
plot(structure,comb,ylim=c(0,1100),xlim=c(1.5,9.5),xaxt='n',xlab='',
     ylab=expression('Saplings'~ha^{-1}),pch=rep(c(21,21),3),cex=1)
axis(1,at=c(2.5,5.5,8.5),labels=c('No Harvest','Midstory Removal','Clearcut'),tick=FALSE)

#Plot shaded boxes
st <- c(0,4,7,10)
for (i in 1:3){
  polygon(x=c(rep(st[i],2),rep(st[i+1],2)),
          y=c(hee.sapling.means[i]-hee.sapling.se[i]*1.96,hee.sapling.means[i]+hee.sapling.se[i]*1.96,
              hee.sapling.means[i]+hee.sapling.se[i]*1.96,hee.sapling.means[i]-hee.sapling.se[i]*1.96),
          col=shadecol,border=F)
  segments(x0=st[i],y0=hee.sapling.means[i],x1=st[i+1],y1=hee.sapling.means[i],lty=2)
}

box()
abline(v=4)
abline(v=7)

#Plot 95% confidence intervals
for (i in 1:6){
  segments(x0=structure[i],y0=comb[i]-1.96*comb.se[i],
           x1=structure[i],y1=comb[i]+1.96*comb.se[i])
  segments(x0=structure[i]-0.06,y0=comb[i]+1.96*comb.se[i],
           x1=structure[i]+0.06,y1=comb[i]+1.96*comb.se[i])
  segments(x0=structure[i]-0.06,y0=comb[i]-1.96*comb.se[i],
           x1=structure[i]+0.06,y1=comb[i]-1.96*comb.se[i])
}

#Plot points on top of intervals
points(structure,comb,pch=rep(c(21,21),3),bg=rep(c('white','black','white','black'),3),
       cex=1)


dev.off()
