####################################################################
##Draft Oak Regen Individual-based Simulation (Not a matrix model)
####################################################################

#General flow:
#1. Add mature trees and saplings that grew into new size category - counts and size vectors
#2. Generate # of acorns produced + germinated
#3. Add new seedlings - counts and size vectors
#4. Generate growth for all three classes
#5. Generate survival for all three classes - cut count + size vectors down to only living individuals
#6. Pull out vectors of individuals that moved up a size class (seedlings+saplings)

#Initial values
#Number of mature trees (>10 cm dbh (=12.19m height))
mature = 50
mature.size = runif(50,10,65)

#Number of saplings (>1.4m in height, <12.19 m)
sapling = 100
sapling.size = runif(100,1.41,12.16)

#Number of seedlings (<1.4 m in height)
seedling = 500
seedling.size = runif(500,0.05,1.4)

barplot(c(seedling,sapling,mature),names=c('seedling','sapling','mature'))

nyears = 100


#Function to generate a set of Parameters

gen.params = function(){

#Production
output = data.frame(
prodmean = 1510,
prodvar = 0, #for now
#Acorn survival
acorn.weevil.prob = 0.50, #guess
acorn.removal.prob = sample(c(0.38,0.34,1.00,0.386,1.00),1,replace=TRUE),
acorn.cache.prob = 0.2, #guess
acorn.surv.prob = 0.5, #guess
#Germination
surface.germ.prob = 0.2, #guess
buried.germ.prob = 0.7, #guess
#Growth (dbh in cm)
mature.growth.mean = 0.449,
mature.growth.var = 0.2,
#Growth (height in m)
sapling.growth.mean = 0.58,
sapling.growth.var = 0.065,
seedling.growth.mean = 0.06#0.0356, #control
seedling.growth.var = 0,#0.0487,
#Survival
mat.surv.prob = 0.99, #guess
sap.surv.prob = 0.95, #guess
seed.surv.prob = 0.45 #guess
)
#Conversion
#at ~10 cm dbh, red oak was 12.19 m tall (Harrison et al. 1986)
return(output)
}

options(warn=-1)
#Begin simulation
for (i in 1:nyears){
  print(i)
  params = gen.params()
  attach(params)
  #Beginning of the cycle
  #Add individuals that grew a size class (mature + sapling only)
  if(i>1){
  mature = mature + new.mature
  mature.size = c(mature.size, new.mature.size)
  sapling = sapling + new.sapling
  sapling.size = c(sapling.size, new.sapling.size)}
  
  print('test')
  
  #Generate acorns produced + new seedlings
  mature.nacorns = as.integer(rnorm(mature,prodmean,prodvar))
  totacorns = sum(mature.nacorns)
  acorn.nonweevil = sum(rbinom(totacorns,1,(1-acorn.weevil.prob)))
  acorn.left = sum(rbinom(acorn.nonweevil,1,(1-acorn.removal.prob)))
  acorn.rem = acorn.nonweevil-acorn.left
  acorn.cache = sum(rbinom(acorn.rem,1,acorn.cache.prob))
  acorn.surv = sum(rbinom(acorn.cache,1,acorn.surv.prob))
  print(i)
  
  acorn.germ = as.integer(surface.germ.prob*acorn.left)+as.integer(buried.germ.prob*acorn.surv)
  new.seedling.size = rep(0.01,acorn.germ)
  seedling = seedling + acorn.germ
  seedling.size = c(seedling.size,new.seedling.size)
  
  barplot(c(seedling,sapling,mature),names=c('seedling','sapling','mature'))
  
  #Generate growth for all classes
  mature.dbh.growth = rnorm(mature,mature.growth.mean,mature.growth.var)
  mature.size = mature.size+mature.dbh.growth
  sapling.growth = rnorm(sapling,sapling.growth.mean,sapling.growth.var)
  sapling.size = sapling.size+sapling.growth
  seedling.growth = rnorm(seedling,seedling.growth.mean,seedling.growth.var)
  seedling.growth[which(seedling.growth<0)]=0
  seedling.size = seedling.size+seedling.growth
  
  #Generate survival for all classes
  mature.surv = rbinom(mature,1,mat.surv.prob)
  mature.size = mature.size[mature.surv==1]
  mature = length(mature.size)
  sapling.surv = rbinom(sapling,1,sap.surv.prob)
  sapling.size = sapling.size[sapling.surv==1]
  sapling = length(sapling.size)
  seedling.surv = rbinom(seedling,1,seed.surv.prob)
  seedling.size = seedling.size[seedling.surv==1]
  seedling = length(seedling.size)
  
  #Separate out growth into new class
  new.sapling.size = seedling.size[which(seedling.size>=1.4)]
  new.sapling = length(new.sapling.size)
  seedling.size = seedling.size[which(seedling.size<1.4)]
  seedling = length(seedling.size)
  
  new.mature.size.raw = sapling.size[which(sapling.size>=12.19)]
  new.mature.size = rep(10,length(new.mature.size.raw)) #Conversion (rough)
  new.mature = length(new.mature.size)
  sapling.size = sapling.size[which(sapling.size<12.19)]
  sapling = length(sapling.size)
  detach(params)
  
}