
#function to combine scenario datasets and re-sort by treatment
treat.reorg <- function(datalist){
  
  out <- list()
  
  for (i in 1:(length(datalist[[1]])-1)){
    
    hold <- list()
    for (j in 1:length(datalist)){
      hold[[j]] <- datalist[[j]][[i]]
      
    }
    names(hold) <- names(datalist)
    out[[i]] <- hold
    
    
  }
  names(out) <- names(datalist[[1]])[1:length(names(datalist[[1]]))-1]
  
  return(out)
}

#Function to summarize data from a particular year and metric
gen.dataset <- function(datalist,metric,year,cont=FALSE,vals=NULL){
  
  temp <- treat.reorg(datalist)
  
  out <- as.data.frame(matrix(data=NA,
                        nrow=length(temp)*length(temp[[1]])*dim(temp[[1]][[1]][[1]])[2]
                        ,ncol=4))
                        
  names(out) <- c(metric,'harvest','scenario','rep')
  
  nreps <- dim(temp[[1]][[1]][[1]])[2]
  
  out[,4] <- rep(c(1:nreps),length(temp)*length(temp[[1]]))
  
  index=1
  
  for (i in 1:length(temp)){
    
    for (j in 1:length(temp[[1]])){
      
      dat.add <- eval(parse(text=paste('temp[[i]][[j]]$',metric,'[',year,',]',sep="")))
      temp.harv <- rep(names(temp)[i],length(dat.add))
      if(!cont){temp.treat <- rep(names(temp[[1]])[j],length(dat.add))}
      if(cont){temp.treat <- as.numeric(rep(vals[j],length(dat.add)))}
      out[index:(index+length(dat.add)-1),1:3] <- cbind(dat.add,temp.harv,temp.treat)
      index = index + length(dat.add)
    }
  }
  out[,1] <- as.numeric(out[,1])
  if(cont){out[,3] <- as.numeric(out[,3])}
  return(out)
}

#Function to generate quick summary figures
gen.figures <- function(datalist,metric,year,ylim,cont=FALSE,vals=NULL,singleplot=FALSE,specify.main=NULL){
  
  dat <- gen.dataset(datalist,metric,year,cont,vals)
  
  if(singleplot){
    if(length(unique(dat$harvest))==2){par(mfrow=c(2,1))}
    if(length(unique(dat$harvest))==4){par(mfrow=c(2,2))}
  }
  
  cols <- c('red','blue','green','orange')
  
  for (i in 1:length(unique(dat$harvest))){
    temp <- dat[dat$harvest == unique(dat$harvest)[i],]
    
    means <- sds <- vector(length = length(unique(temp$scenario)))
    
    for (j in 1:length(unique(temp$scenario))){
      
      hold <- as.numeric(temp[temp$scenario==unique(temp$scenario)[j],1])
      means[j] <- mean(hold,na.rm=T)
      sds[j] <- sd(hold,na.rm=T)
    }
    
    if(!cont){
      
    brp <- barplot(means,ylab=metric,xlab="",names=unique(temp$scenario),
                    main=unique(dat$harvest)[i],col=cols[i],
                    ylim=ylim,las=3)
      for (k in 1:length(means)){
          segments(brp[k],means[k]-sds[k],brp[k],means[k]+sds[k])
      }
    }
    
    if(cont){
      
      if(i==1){
      
      plot(vals,means,ylab=metric,xlab="Parameter Value",ylim=ylim,col=cols[i],
           main=specify.main,type='o',lwd=3)
      for (k in 1:length(means)){
        segments(vals[k],means[k]-sds[k],vals[k],means[k]+sds[k])
      }
      legend('bottomleft',legend=c('none','clearcut','shelterwood','singletree'),
             col=cols,lty=1)
      }
      if(i>1){
        lines(vals,means,col=cols[i],
             type='o',lwd=3)
        for (k in 1:length(means)){
          segments(vals[k],means[k]-sds[k],vals[k],means[k]+sds[k])
      }}
      
    }
  }
  if(singleplot){par(mfrow=c(1,1))}
}

analyze.ibm <- function(datalist,harvest,metric,year,cont=FALSE,vals=NULL){
  
  require(pgirmess)
  
  dat <- gen.dataset(datalist,metric,year,cont,vals)
  dat <- dat[dat$harvest == harvest,]
  
  if(!cont){
    dat[,3] <- as.factor(dat[,3])
    a <- aov(dat[,1] ~ dat[,3])
    a.mc <- TukeyHSD(a)
  
    b <- kruskal.test(dat[,1] ~ dat[,3])
    b.mc <- kruskalmc(dat[,1], dat[,3])
    
    out <- list(anova=a,anova.mc=a.mc,kruskal=b,kruskal.mc=b.mc)
  }
  
  if(cont){
    dat[,3] ~ as.numeric(dat[,3])
    a <- lm(dat[,1] ~ dat[,3])
    
    out <- a
  }
  
  return(out)
}

#Function to decompose explained variance (and sensitivities) to uncorrelated and correlated portions
#based on Xu and Gertner 2008
varPart <- function(response, inputs, digits=3){
  
  #Create empty output object and add names
  out <- as.data.frame(matrix(data=NA,nrow=(dim(inputs)[2]+1),ncol=9))
  names(out) <- c('parameter','parV','parUV','parCV','S','Su','Sc','%U','cor')
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
    out[i,8] <- out[i,6]/out[i,5]
    out[i,9] <- cor(response,inputs[,i])
    
  }
  
  #calculate column means and return output
  out[(dim(inputs)[2]+1),2:8] <- colSums(out[1:(dim(inputs)[2]),2:8])
  
  for(i in 2:8){
    out[,i] <- round(out[,i],digits=digits)
  }
  
  return(out)
}
