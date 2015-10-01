

#OAKS

#overstory mean = 45.75
mean(c(44.7,46.3,46.6,44.8,46.4,43.9,48.2,45.6,45.3))

#Overstory density = 89
oak.ov = c(120,81,63,123,92,92,48,77,105)
mean(oak.ov)

#total mean dbh = 17.97
mean(c(20.6,17.7,15.6,19,16,17.7,18.8,17,19.4))

#total mean density = 183.89
oak.tot = c(235,205,166,204,192,219,85,163,186)
mean(oak.tot)

#sapling density = 94.89
mean(oak.tot-oak.ov)

#non-overstory mean dbh
#89*45.75 + 95*x = 184*17.97
((184*17.97) - (89*45.75)) /95


#TOLERANTS

mean(c(40.9,39.7,40,40.3,43.2,39.2,40.7,40.7,42.7))

maple.ov = c(7,12,14,5,8,7,22,14,7)
mean(maple.ov)

maple.tot = c(297,607,556,583,736,469,529,784,394)
mean(maple.tot - maple.ov)

mean(c(10.3,10.8,10.7,8.6,9.6,9.4,13.3,9.2,10.9))

#INTOLERANTS

mean(c(44,48,45.8,45.7,45.1,46.8,46.5,44.5,39.2)) 

mean(c(8,15,18,8,5,11,13,5,4))

mean(c(12.7,20.8,14.8,12.5,13.4,17.6,20.1,12.3,9.9))

mean(c(161,206,354,125,208,141,88,51,132))

###################################################

dbh.input <- seq(1,80,1)
dbh.input = 56.7
C = 1.75
exponent = 2.5
k = 0.0000756
PHI = 1

SLA <- C * dbh.input^exponent

light = 1 - (PHI * exp(-1 * k * SLA))











