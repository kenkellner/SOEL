#####################################################
## Estimate Seedling Herbivory Probability pBrowse ##
#####################################################

#Based on data from:
#Kellner, K. F. and R. K. Swihart (2016). Herbivory on planted oak 
#seedlings across a habitat edge created by timber harvest. 
#Plant Ecology. doi: 10.1007/s11258-016-0678-6

#Read in raw data
ibm.browse <- read.csv('data/ibm_browse.csv',header=T)

#Fit browse model (logistic regression)
#Full model: height, height centered squared, oak species, and random effect of year
library(MASS)
browse.mod <- glmmPQL(br~ht+ht2+sp,data=ibm.browse,family='binomial',random=~1|yr)
