
##Site quality Example Figure

par(mfrow=c(2,2),
    mar=c(5.1,4.1,2.6,0),
    oma=c(1,0,1,1))

x <- seq(1980,5500,1)
y <- 4 * (x - 1977)*(5894 - x) / ((5894 - 1977)^2)
plot(x,y,type='l',lwd=2,xlab="Degree-Days", main="Degree-Days",
     ylab="")

x <- seq(0,400,1)
y <- (-0.6 + 1 * (2.94 * (1 - 10 ^(-1 * 0.00234 * (x + 117.52)))))/2.19
plot(x,y,type='l',lwd=2,xlab="Soil Nitrogen (kg/ha)", main="Soil Fertility",
     ylab="Growth Response")

x <- seq(0,0.4,0.01)
y <- 1 - (x / 0.45)^2
plot(x,y,type='l',lwd=2,xlab="Wilt Index", main="Wilt Potential",
     ylab="Growth Response")

x <- seq(0.5,10,0.01)
y <- 1 - (0.933 / x)
plot(x,y,type='l',lwd=2,xlab="Depth to Water Table (m)", main="Soil Saturation",
     ylab="",ylim=c(0,1.01),xlim=c(0,10))

###############################################################################

##k-value Figure

dbh.input <- seq(1,100,1)
#k4 = 0.0000756
k3 = 1/6000
k2 = 1/4000
k1 = 1/2000


approach1 <- (PHI * exp(-1 * k1 * C * dbh.input ^ 2))
approach2 <- (PHI * exp(-1 * k2 * C *dbh.input ^ 2))
approach3 <- (PHI * exp(-1 * k3 * C *dbh.input ^ 2))

plot(dbh.input,approach1,type='l',lwd=2,ylab="% Light Available Under Tree",xlab="Tree DBH (cm)")
lines(dbh.input,approach2,type='l',lty=2,lwd=2)
lines(dbh.input,approach3,type='l',lty=3,lwd=3)
legend('topright',legend=c('1/2000','1/4000','1/6000'),lty=c(1,2,3),lwd=2,
       title=expression('Value of'~italic(k)))

################################################################################

##Light tolerance figure

light <- seq(0,1,0.01)

ylow <- 2.24*(1 - exp(-1.136 * (light-0.08)))

yhigh <- 1 - exp(-4.64*(light-0.05))

#Calculate average values for curve fitting

yint <- rowMeans(cbind(ylow,yhigh))

#Fit to logarithmic response curve

fit = nls(yint ~ a*(1 - exp(b*(light - 0.05))), 
          start = list(a=1,b=-4),control=list(maxiter=10000))

#Equation for intermediate tolerance
yint.fit <- 1.371*(1 - exp(-2.227*(light-0.05)))

#Check on plot - it works!

plot(light,ylow,type='l',ylab=expression('Growth response to light'~italic('fAL')),
     xlab="Light",lwd=2,ylim=c(0,1.6))
lines(light,yint.fit,type='l',lty=2,lwd=2)
lines(light,yhigh,type='l',lty=3,lwd=2)
legend("topleft",legend=c('Low','Intermediate','High'),lty=c(1,2,3),lwd=2,title="Shade Tolerance")

