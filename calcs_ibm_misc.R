S = 60
D = 0.19

h2 = 80

alpha = h2*(1 + 0.0026*exp(0.04002*S))
beta = 14.601/h2

H1 = (4.5 + alpha*(1 - exp(-beta*(D*39.371)))) * .3084

D1 = ((log(1 - (((H*3.28084)-4.5)/alpha))) / -beta) * 0.0254



25.655

S - exp(22.0217*(1/80-1/50))

h2 = 1.80408*S^(.932)*(1-exp(-.0240308*80))^(2.555*S^(-.280445))

H = 26.49
plot(D1,H)

test = NLSstAsymptotic(sortedXyData(wo$dbh,wo$ht))

x = seq(1,30,0.1)
y = 0.3555*log(x) - 0.7051


#################################

H = 6.157
k = 1.12
A = 80
S = 60 * 0.3048
Hs = 1.80408*S^(0.932097)*(1 - exp(-0.0240308*A))^(2.55529*S^(-0.280445))
Ds = 6.40146*S^(0.631893)*(1 - exp(-0.0227614*A))^(1.21892)

J = log((k*Hs - Hs)/(k*Hs - 1.37)) / Ds
G = k*Hs - 1.37


D = log(1 - ((H - 1.37)/G)) / J

H = seq(1.5,37,0.1)

plot(D,H)

test = seq(0,10,0.01)
test2 = rexp()

##########################################

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







