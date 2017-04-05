#####################################################
## Estimate Seedling Herbivory Probability pBrowse ##
#####################################################

#Based on data from:
#Kellner, K. F. and R. K. Swihart (2016). Herbivory on planted oak 
#seedlings across a habitat edge created by timber harvest. 
#Plant Ecology. doi: 10.1007/s11258-016-0678-6

#Initial formatting on raw data
source('../seedling-survival/format_data.R')
seedling <- format.seedling('data/ibm_seedling.csv')

#Plots to keep (deer not excluded, not in shelterwoods)
plots <- c(1:54)
exclude <- 1 - seedling$plot.data$herbivory
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

#Format covariate data
species <- seedling.covs$species

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
ht.raw[,8] <- ht.raw[,7]

#Browse - simplify to presence/absence for now
browse <- seedling$browsedeer[keep,]

browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],
                          browse[,5]+browse[,6],browse[,7]+browse[,8]))
browse[which(browse>1,arr.ind=TRUE)] = 1

#Bundle data into new dataframe for analysis
br <- c(as.vector(browse[,1]),as.vector(browse[,2]),as.vector(browse[,3]),as.vector(browse[,4]))
sp <- c(species,species,species,species)
ht <- as.matrix(cbind(ht.raw[,2],ht.raw[,4],
                     ht.raw[,6],ht.raw[,8]))
ht <- c(as.vector(ht[,1]),as.vector(ht[,2]),as.vector(ht[,3]),as.vector(ht[,4]))
ht2 <- ht*ht
yr <- c(rep(1,580),rep(2,580),rep(3,580),rep(4,580))
yr <- as.factor(yr)
browsedata <- data.frame(br,sp,ht,ht2,yr)

#Fit browse model (logistic regression)
#Full model: height, height centered squared, oak species, and random effect of year
library(MASS)
browse.mod <- glmmPQL(br~ht+ht2+sp,data=browsedata,family='binomial',random=~1|yr)

