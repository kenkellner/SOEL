#Removal function

rem = read.csv('data/ibm_removal.csv',header=T)

library(dplyr)
disp = read.csv('data/ibm_dispersal.csv',header=T)
disp <- disp[disp$clear==0,]
disp$dist[is.na(disp$dist)] <- 0
disp$dist[1973] <- 24.5
disp <- rbind(filter(disp,Treecode!=4),
                  filter(disp,Treecode==4,year12==0))
disp <- disp[disp$squirrel==0,]

mast = rep(NA,dim(rem)[1])
for (i in 1:length(mast)){
  mast[i] <- mean(mastwo[rem$yearcode[i]],mastbo[rem$yearcode[i]])
}

library(MASS)

#Treat x yearly
rm <- glmmPQL(rem~species+shelter+weev,data=rem,family='binomial',random= ~ 1 | yearcode)
summary(rm)

#treat
rm <- glm(rem~species+shelter+weev,data=rem,family='binomial')
summary(rm)
#yearly
rm <- glmmPQL(rem~species+weev,data=rem,family='binomial',random= ~ 1 | yearcode)
summary(rm)
#neither
rm <- glm(rem~species+weev,data=rem,family='binomial')
summary(rm)

#Dispersal trees only

yearcode <- rep(1,dim(disp)[1])
mast <- rep(NA,dim(disp)[1])
for (i in 1:length(yearcode)){
  if(disp$year12[i]==1){yearcode[i]=2}
  if(disp$year13[i]==1){yearcode[i]=3}
  if(disp$year14[i]==1){yearcode[i]=4}
  mast[i] <- mean(mastbo[yearcode[i]],mastwo[yearcode[i]])
}

rm <- glmmPQL(removed~wo+shelter+mast,data=disp,family='binomial',random= ~ 1 | yearcode)
summary(rm)

#Test function

remprob <- function(n, shelter,wo,weev){
  hold <- 0.519 - 0.162*wo + 0.129*shelter - 2.00*weev + rnorm(n,0,0.6322)
  out <- (1 +exp(-1*hold))^(-1)
  return(out)
}

#remprob <- function(n, shelter,wo,weev){
#  hold <- 0.8614 - 1.21*wo + 0.4137*shelter + rnorm(n,0,1.129)
#  out <- (1 +exp(-1*hold))^(-1)
#  return(out)
#}

hist(remprob(100,1,1,1))

