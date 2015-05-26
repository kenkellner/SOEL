
#Reps

reps <- 1

#Size of simulated area (m)

xcorewidth <- 100
ycorewidth <- 100
buffer <- 25

#Burn-in before harvest (years)

burnin <- 10

#Runtime (total)

years <- 150

#Harvest types

harvests <- c('none','clearcut', 'shelterwood', 'single-tree')
  
#Site quality

manual.site.quality <- list(whiteoak=0.75,blackoak=0.75,maple=0.75,poplar=0.75) #Null otherwise

site.chars <- list(degdays = 4840, wtdist = 9.3, availableN = 325, wilt = 0.10) #If NULL

#Model type

model <- 'jabowa' #or
model <- 'ibm'

#Tuning parameter values

extinct <- 0.00025
density-dep <- 3.5

#Way seedlings are handled

seedlings <- 'none'
seedlings <- 'litdata'
seedlings <- 'hee'

maxgrowth <- 0.9 #if litdata

#Regen parameters - not going to manipulate just yet









