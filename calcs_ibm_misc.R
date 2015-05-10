
##Calculating the intermediate light response curve

#Existing high/low curves
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

##########Stump sprouting

##yellow poplar
#Probability
x = c(0.1524,0.6604, 0.80)
y = c(0.95, 0.40, 0)

plot(x,y,xlim=c(0.10,1.00),ylim=c(0,1.10))
abline(lm(y~x))
abline(v=0.10)

############Light Transmittence

#check if my way of calculating is same as Botkin
C = 1.75

k = 0.0000756

PHI = 1

dbh.input1 = 80

dbh.input2 = 35

SLA1 <- C * dbh.input1 ^ 2

SLA2 <- C * dbh.input2 ^ 2

cdensity1 <- 1 - (PHI * exp(-1 * k * SLA1))

cdensity2 <- 1 - (PHI * exp(-1 * k * SLA2))

light.method1 = cdensity1 + (1 - cdensity1) * cdensity2

light.method2 <- 1 - (PHI * exp(-1 * k * (SLA1 + SLA2)))

#Approaches are identical
light.method1
light.method2

#############################################

#H-Dbh relationship

#for oaks
dbhinput = seq(0,1,0.001)
#dbhinput = 0.41
Hmax = 3800
Dmax = 100
b2 = 2*(Hmax - 137) / Dmax
b3 = (Hmax - 137) / Dmax ^ 2

height = (137 + b2 * (dbhinput) - b3 * ((dbhinput) ^ 2))
plot(dbhinput, height)

summary(lm(dbhinput ~ height))

#equation: dbh in cm = -1.88 + 0.01372*height in cm

#137-167 cm height = 0 - 0.41 cm dbh
#or 0 - 0.0041 m dbh

###################
#for maples
dbhinput = seq(0,1,0.001)
#dbhinput = 0.41
Hmax = 3350
Dmax = 100
b2 = 2*(Hmax - 137) / Dmax
b3 = (Hmax - 137) / Dmax ^ 2

height = (137 + b2 * (dbhinput) - b3 * ((dbhinput) ^ 2))
plot(dbhinput, height)

summary(lm(dbhinput ~ height))

#equation: dbh in cm = -2.144 + 0.01564*height in cm

#137-167 cm height = 0 - 0.468 cm dbh
#or 0 - 0.00468 m dbh


###################
#for poplars
dbhinput = seq(0,1,0.001)
#dbhinput = 0.41
Hmax = 4000
Dmax = 100
b2 = 2*(Hmax - 137) / Dmax
b3 = (Hmax - 137) / Dmax ^ 2

height = (137 + b2 * (dbhinput) - b3 * ((dbhinput) ^ 2))
plot(dbhinput, height)

summary(lm(dbhinput ~ height))

#equation: dbh in cm = -1.783 + 0.01301*height in cm

#137-167 cm height = 0 - 0.39 cm dbh
#or 0 - 0.0039 m dbh





