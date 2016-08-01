#############################################################
## Parameters for Initializing Realistic Simulated Forests ##
#############################################################

#Based on data from:
#Saunders, M., and J. Arseneault (2013). Pre-treatment analysis of woody vegetation composition 
#and structure on the hardwood ecosystem experiment research units. 
#In The Hardwood Ecosystem Experiment: a framework for studying responses to forest management. 
#Gen. Tech. Rep. NRS-P-108. (R. K. Swihart, M. R. Saunders, R. A. Kalb, G. S. Haulton and C. H. 
#Michler, Editors). USDA Forest Service Northern Research Station, Newtown Square, PA, pp. 96–125.

#OAKS

#Unit-level values averaged

#overstory basal area mean = 45.75
mean(c(44.7,46.3,46.6,44.8,46.4,43.9,48.2,45.6,45.3))

#Overstory stem density = 89
mean(c(120,81,63,123,92,92,48,77,105))

#total mean dbh = 17.97
mean(c(20.6,17.7,15.6,19,16,17.7,18.8,17,19.4))

#All trees mean density = 183.89
mean(c(235,205,166,204,192,219,85,163,186))

#Sapling densities calculated as all trees - overstory trees


#TOLERANTS (= maples in simulation)

#Overstory mean dbh
mean(c(40.9,39.7,40,40.3,43.2,39.2,40.7,40.7,42.7))

#Overstory stem density
mean(c(7,12,14,5,8,7,22,14,7))

#All trees mean dbh
mean(c(10.3,10.8,10.7,8.6,9.6,9.4,13.3,9.2,10.9))

#All trees stem density
mean(c(297,607,556,583,736,469,529,784,394))


#INTOLERANTS (= tulip poplars in simulation)

#Overstory dbh
mean(c(44,48,45.8,45.7,45.1,46.8,46.5,44.5,39.2)) 

#Overstory stem density
mean(c(8,15,18,8,5,11,13,5,4))

#All trees mean dbh
mean(c(12.7,20.8,14.8,12.5,13.4,17.6,20.1,12.3,9.9))

#All trees density
mean(c(161,206,354,125,208,141,88,51,132))
