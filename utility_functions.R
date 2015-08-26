
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
gen.dataset <- function(datalist,metric,year){
  
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
      temp.treat <- rep(names(temp[[1]])[j],length(dat.add))
      out[index:(index+length(dat.add)-1),1:3] <- cbind(dat.add,temp.harv,temp.treat)
      index = index + length(dat.add)
    }
  }
  out[,1] <- as.numeric(out[,1])
  return(out)
}

gen.figures <- function(datalist,metric,year,maxy,singleplot=FALSE){
  
  dat <- gen.dataset(datalist,metric,year)
  
  if(singleplot){
    if(length(unique(dat$harvest))==2){par(mfrow=c(2,1))}
    if(length(unique(dat$harvest))==4){par(mfrow=c(2,2))}
  }
  
  cols <- c('red','blue','green','yellow')
  
  for (i in 1:length(unique(dat$harvest))){
    temp <- dat[dat$harvest == unique(dat$harvest)[i],]
    
    means <- sds <- vector(length = length(unique(temp$scenario)))
    
    for (j in 1:length(unique(temp$scenario))){
      
      hold <- as.numeric(temp[temp$scenario==unique(temp$scenario)[j],1])
      means[j] <- mean(hold)
      sds[j] <- sd(hold)
    }
      
    brp <- barplot(means,ylab=metric,xlab="",names=unique(temp$scenario),
                    main=unique(dat$harvest)[i],col=cols[i],
                    ylim=c(0,maxy),las=3)
      for (k in 1:length(means)){
          segments(brp[k],means[k]-sds[k],brp[k],means[k]+sds[k])
      }
  }
  if(singleplot){par(mfrow=c(1,1))}
}

analyze.ibm <- function(datalist,harvest,metric,year,cont=FALSE){
  
  require(pgirmess)
  
  dat <- gen.dataset(datalist,metric,year)
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