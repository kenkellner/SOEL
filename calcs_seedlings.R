
#Seedling Growth

y = matrix(NA,nrow=237,ncol=4)
#ht = 10
shade = 0
wo = 1
browsed = 0

ymean = rnorm(237,0,1.934)

treat2 <- treat[keep][keep2][keep3]

cutoff <- length(which(growth>30))/472 #- should be based on seedlings in openings

taillambda <- 1/mean(growth[which(growth>30)])


for (i in 1:237){
  for (j in 1:4){
    if(runif(1,0,1)>cutoff&shade<0.2){
      rand <- rnorm(1,0,9.151)
      y[i,j] = max(-100, 4.485 + ymean[i] - 3.014*shade + 1.730*wo 
                + -4.892*browsed + rand)
    } else {y[i,j] <- rexp(1,rate=taillambda)}
  }
}

par(mfrow=c(2,1))
hist(y,freq=F,xlim=c(-10,120))
hist(growth[growth>-10],freq=F)

ysave = y #for plotting later




#############################################

#Seedling Survival

age <- 1
shade <- 0.8
wo <- 1
height <- 150

surv.pred <- -0.587 + 0.086*wo + 0.370*shade + 0.589*age
p.surv <- (1 + exp(-1*surv.pred))^(-1)

p.surv


####################################

#Another approach

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

nsamples <- change <- vector(length=dim(seedling$height)[1])
for (i in 1:length(nsamples)){
  nsamples[i] <- length(na.omit(seedling$height[i,]))
  if(length(na.omit(seedling$height[i,]))>0){
    change[i] <- max(seedling$height[i,],na.rm=T) - seedling$height[i,1]
  } else {change[i] <- NA}
}

treat <- vector(length=dim(seedling$height)[1])

for (i in 1:length(treat)){
  
  o <- seedling$site.data$opening[seedling$plot.data$siteid[seedling$seedling.data$plotid[i]]]
  e <- seedling$site.data$edge[seedling$plot.data$siteid[seedling$seedling.data$plotid[i]]]
  m <- seedling$site.data$matrix[seedling$plot.data$siteid[seedling$seedling.data$plotid[i]]]
  s <- seedling$site.data$shelter[seedling$plot.data$siteid[seedling$seedling.data$plotid[i]]]
  
  if(o==1){treat[i] <- 'opening'}
  if(e==1){treat[i] <- 'edge'}
  if(m==1){treat[i] <- 'matrix'}
  if(s==1){treat[i] <- 'shelter'}
  
}

age <- seedling$seedling.data$age

rate <- change / (max(1,(nsamples - 1)))

out <- data.frame(change,nsamples,rate,age,treat)

val = (rate[age==1 & treat == "opening" & rate > 0])

par(mfrow=c(2,1))

hist(y,freq=F)

hist(val,freq=F)
x = seq(0,40,1)
lam = 1/mean(val,na.rm=T)

px <- dexp(x,rate=lam)

lines(x,px,type='l')
