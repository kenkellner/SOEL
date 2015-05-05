
#Seedling Growth

ht = 10
shade = 0.8
wo = 0
browsed = 0

for (i in 1:100){
y[i] = max(0,3.498 + rnorm(1,0,0.833) + 0.078*ht + -3.884*shade + 1.272*wo 
           + -4.698*browsed + rnorm(1,0,9.92))
}

#############################################

#Seedling Survival



