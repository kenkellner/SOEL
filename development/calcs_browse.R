source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')
exclude <- 1 - seedling$plot.data$herbivory

#Plots to keep (deer not excluded, not in shelterwoods)
plots <- c(1:54)
keep.plots <- which(plots<49&exclude==0)
keep.plots <- keep.plots[-2]

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid%in%keep.plots)

#Get survival data as baseline for when browsing could occur
surv <- seedling$surv.sprout[keep,]
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
  if(0%in%surv[i,]){nsamples[i] <- nsamples[i]-1}
}

#Seedling-level covariates
nseedlings <- dim(surv)[1]
seedling.covs <- seedling$seedling.data[keep,]
seed.sitecode <- seedling.covs$siteid

seed.plotcode.raw <- seedling.covs$plotid
seed.plotcode <- rep(1,length(which(seed.plotcode.raw==unique(seed.plotcode.raw)[1])))
for (i in 2:24){
  add <- rep(i,length(which(seed.plotcode.raw==unique(seed.plotcode.raw)[i])))
  seed.plotcode <- c(seed.plotcode,add)
}




species <- seedling.covs$species
age <- seedling.covs$species

#Format root collar diameter data
rcd.raw <- as.matrix(cbind(seedling$rcd[,1],seedling$rcd[,1],seedling$rcd[,2],seedling$rcd[,2],seedling$rcd[,3],
                           seedling$rcd[,3],seedling$rcd[,4],seedling$rcd[,4]))
rcd.raw <- rcd.raw[keep,]
rcd.raw[is.na(rcd.raw[,1])&age==1,1] <- mean(rcd.raw[age==1,1],na.rm=TRUE)
rcd.raw[is.na(rcd.raw[,1])&age==0,1] <- mean(rcd.raw[age==0,1],na.rm=TRUE)

for (i in 1:dim(surv)[1]){
  for (j in 2:8){
    if(!is.na(surv[i,j])){
      if(is.na(rcd.raw[i,(j-1)])){
        rcd.raw[i,(j-1)] <- rcd.raw[i,(j-2)]  
      }}}}
rcd.raw[,8] <- rcd.raw[,7]
rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

#Format height data
ht.raw <- as.matrix(cbind(seedling$height[,1],seedling$height[,1],seedling$height[,2],seedling$height[,2],seedling$height[,3],
                          seedling$height[,3],seedling$height[,4],seedling$height[,4]))
ht.raw <- ht.raw[keep,]
ht.raw[is.na(ht.raw[,1])&age==1,1] <- mean(ht.raw[age==1,1],na.rm=TRUE)
ht.raw[is.na(ht.raw[,1])&age==0,1] <- mean(ht.raw[age==0,1],na.rm=TRUE)

for (i in 1:dim(surv)[1]){
  for (j in 2:8){
    if(!is.na(surv[i,j])){
      if(is.na(ht.raw[i,(j-1)])){
        ht.raw[i,(j-1)] <- ht.raw[i,(j-2)]  
      }}}}
ht.raw[,8] <- rcd.raw[,7]


#Browse - simplify to presence/absence for now
browse <- seedling$browsedeer[keep,]

browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],
                          browse[,5]+browse[,6],browse[,7]+browse[,8]))
browse[which(browse>1,arr.ind=TRUE)] = 1

br <- c(as.vector(browse[,1]),as.vector(browse[,2]),as.vector(browse[,3]),as.vector(browse[,4]))
sp <- c(species,species,species,species)
ht <- as.matrix(cbind(ht.raw[,2],ht.raw[,4],
                     ht.raw[,6],ht.raw[,8]))
ht <- c(as.vector(ht[,1]),as.vector(ht[,2]),as.vector(ht[,3]),as.vector(ht[,4]))
yr <- c(rep(1,580),rep(2,580),rep(3,580),rep(4,580))
yr <- as.factor(yr)
ind <- c(c(1:580),c(1:580),c(1:580),c(1:580))
ind <- as.factor(ind)
browsedata <- data.frame(br,sp,ht,yr,ind)

library(MASS)

browse.mod <- glmmPQL(br~ht+sp,data=browsedata,family='binomial',random=~1|yr)

