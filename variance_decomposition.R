
#Load library that handles correlated latin hypercube
library(pse)

#specify input covariate names and sampling distribution
covs = c('a','b','c')
q = 'qunif'

#Specify correlation matrix for input covariates (only a and b are correlated)
corm = matrix(data=c(1,0.6,0,0.6,1,0,0,0,1),nrow=3)

#Specify characteristics of input covariate sampling distributions
qarg<- list(a=list(min=0,max=1),b=list(min=0,max=1),c=list(min=0,max=1))

#generate latin hypercube
test <- LHS(model=NULL,factors=covs,N=100,q=q,q.arg=qarg,opts=list(COR=corm))

#examine generated set of parameter values
head(test$data)

#Specify regression coefficients to generate "output" data
sensA <- 0.4
sensB <- 0.3
sensC <- 0.7
y.sim <- 5 + sensA*test$data$a + sensB*test$data$b + sensC*test$data$c + rnorm(100,0,0.5)


#Function to decompose explained variance (and sensitivities) to uncorrelated and correlated portions
#based on Xu and Gertner 2008
varPart <- function(response, inputs){
  
  #Create empty output object and add names
  out <- as.data.frame(matrix(data=NA,nrow=(dim(inputs)[2]+1),ncol=7))
  names(out) <- c('parameter','parV','parUV','parCV','S','Su','Sc')
  out[,1] <- c(colnames(inputs),'sum')
  
  #Iterate over i input covariates and calculate variances
  for (i in 1:(dim(inputs)[2])){
    
    #Assume partial variance = correlated component of partial variance + uncorrelated component
    
    ######calculate partial variance for covariate i (eq 11)
    
    #regression of output on only covariate i (eq 10)
    pv = lm(response~inputs[,i])
    
    temp.sum <- 0
    for(j in 1:length(response)){
      temp.sum <- temp.sum + (pv$fitted.values[j]-mean(response))^2
    }
    parV <- 1/(length(response)-1)*temp.sum
    
    #Place in output object
    out[i,2] <- parV
    
    #####Calculate uncorrelated partial variance for covariate i
    
    #Create sub-dataset
    focal <- inputs[,i]
    minusi <- inputs[,-i]
    comb <- cbind(focal,minusi)
    
    #regress covariate i on all other covariates (eq 14)
    rp <- lm(focal~.,comb)
    
    #regress output on residuals from above (eq 13)
    upv <- lm(response ~ rp$residuals)
    
    #Calculate uncorrelated partial variance (eq 16)
    temp.sum <- 0
    for(j in 1:length(response)){
      temp.sum <- temp.sum + (upv$fitted.values[j]-mean(response))^2
    }
    parUV <- 1/(length(response)-1)*temp.sum
    
    #Save in output object
    out[i,3] <- parUV
    
    ######Calculate correlated partial variance for covariate i
    
    parCv <- parV - parUV #(eq 9)
    
    #Save in output object
    out[i,4] <- parCv
    
    #Calculate sensitivities for each partial variance based on ratio w/total variance
    v <- var(response)
    
    #save in output object
    out[i,5] <- parV/v
    out[i,6] <- parUV/v
    out[i,7] <- parCv/v
  
    
  }
  
  #calculate column means and return output
  out[(dim(inputs)[2]+1),2:7] <- colSums(out[1:(dim(inputs)[2]),2:7])
  return(out)
}

#test function on simulated data

varPart(y.sim,test$data)

#note that parameters a and b (correlated) have much larger correlated partial variance components (parCV)
#and correspondingly large correlated contribution to sensitivity (Sc) relative to parameter c (uncorrelated)

#Also note that it is possible for the correlated partial variance component to be negative; 

#Now examine some SORIM output

load('sens_test.Rdata')

#Grab set of input parameter values (from no harvest scenario)
#and standardize
inp.covs <- as.data.frame(scale(sens.test$none[,1:10]))

#Grab output metric (density of saplings)
out.vals <- sens.test$none$seedclass4

varPart(out.vals,inp.covs)

#Interesting observations - acorn production (lamAcorn) has highest "sensitivity" but most is 
#in correlated portion. weibull shape parameter has a large uncorrelated portion.

