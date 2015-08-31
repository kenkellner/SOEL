

require(parallel)


sensitivity.test <- function(nreps, burnin, length, harvests, force.processors=NULL, ram.max){

  #Path to NetLogo installation based on OS
  if(Sys.info()[['sysname']] == "Windows"){
    nl.path <- "C:/Program Files (x86)/Netlogo 5.2.0"}
  if(Sys.info()[['sysname']] == "Linux"){
    nl.path <- paste('/home/',Sys.info()[['user']],'/programs/netlogo-5.2.0',sep="")}
  
  #Model start time
  start.time <- Sys.time()
  
  #Get path to appropriate model
  model.path <- paste(getwd(),"/oak_ibm.nlogo",sep="")

  #Get NetLogo Path
  if(Sys.info()[['sysname']] == "Windows"){
    nl.path <- "C:/Program Files (x86)/Netlogo 5.2.0"}
  if(Sys.info()[['sysname']] == "Linux"){
    nl.path <- paste('/home/',Sys.info()[['user']],'/programs/netlogo-5.2.0',sep="")}
  
  #Set list of resporters to save
  reporters <- c("ticks","basal-area","basal-area-ovs",
                 "ba-oak","ba-oak-ovs","ba-map","ba-map-ovs","ba-pop","ba-pop-ovs",
                 "dens","dens-ovs",
                 "dens-oak","dens-oak-ovs","dens-map","dens-map-ovs","dens-pop","dens-pop-ovs",
                 "qdbh","qdbh-ovs",
                 "qdbh-oak","qdbh-oak-ovs","qdbh-map","qdbh-map-ovs","qdbh-pop","qdbh-pop-ovs",
                 "prop-oak","prop-tol","prop-intol",
                 "total-acorns","acorns-pertree","total-seedlings","new-seedlings",
                 "pct-germ","pct-bo-germ","pct-wo-germ",
                 "regen-dens","regen-stump-dens",
                 "seedlings-class1","seedlings-class2","seedlings-class3",
                 "seedlings-class123","seedlings-class4",
                 "seedbo-class123","seedwo-class123","seedbo-class4","seedwo-class4")
  rep.names <- c("ticks","ba","ba.ovs",
                 "ba.oak","ba.oak.ovs","ba.map","ba.map.ovs","ba.pop","ba.pop.ovs",
                 "dens","dens.ovs",
                 "dens.oak","dens.oak.ovs","dens.map","dens.map.ovs","dens.pop","dens.pop.ovs",
                 "qdbh","qdbh.ovs",
                 "qdbh.oak","qdbh.oak.ovs","qdbh.map","qdbh.map.ovs","qdbh.pop","qdbh.pop.ovs",
                 "prop.oak","prop.tol","prop.intol",
                 "totacorns","acornspertree","totseedlings",
                 "newseedlings","pctgerm","pctbogerm","pctwogerm",
                 "regen",'regenstump',
                 "seedclass1","seedclass2","seedclass3","seedclass123","seedclass4",
                 "seedboclass123","seedwoclass123","seedboclass4","seedwoclass4")
  
  #Generate parameter set
  params <- c('prob.browse','prob.weevil','prob.drought','mast.val','disperse.prob',
              'weibSc','weibSh','disp.eaten.prob','cache.prob','undisp.eaten.prob')
  param.set <- array(data=NA,dim=c(nreps,length(params),4))
  for (i in 1:4){
    
    param.set[,1,i] <- runif(nreps,0,1)
    param.set[,2,i] <- runif(nreps,0.0198,0.9545)
    param.set[,3,i] <- runif(nreps,0,1)
    param.set[,4,i] <- runif(nreps,0.0367,2.975)
    param.set[,5,i] <- runif(nreps,0.401,0.910)
    param.set[,6,i] <- runif(nreps,4.216,11.962)
    param.set[,7,i] <- runif(nreps,1.141,1.888)
    param.set[,8,i] <- runif(nreps,0.610,0.976)
    param.set[,9,i] <- runif(nreps,0.222,1)
    param.set[,10,i] <- runif(nreps,0,1)
    
    #for (j in 1:length(params)){
    #  param.set[,j,i] <- runif(nreps,0,1)
    #}
  }
  
  #Internal function to setup NetLogo in each parallel subprocess
  initNL <- function(dummy, gui, nl.path, model.path) {
    
    #Initialize JVM with plenty of RAM
    options( java.parameters = paste("-Xmx",ram.max,"m",sep="") )
    library(rJava)
    .jinit()
    library(RNetLogo)
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(model.path)
  }
  
  #Java garbage cleanup function
  jgc <- function()
  {
    .jcall("java/lang/System", method = "gc")
  }    
  
  #Setup each parallel process
  if(!is.null(force.processors)){processors <- force.processors
  } else {processors <- detectCores()}
  
  runNL <- function(i,harvest,harvestcount) {
    
    #Garbage collection in R and Java
    gc()
    jgc()
    
    prob.browsed=param.set[i,1,harvestcount]
    prob.weevil=param.set[i,2,harvestcount]
    prob.drought=param.set[i,3,harvestcount]
    mast.val=param.set[i,4,harvestcount]
    
    #Select harvest type, seedling type, etc.
    NLCommand(paste('set x-core',140))
    NLCommand(paste('set y-core',140))
    NLCommand(paste('set buffer',20))
    NLCommand(paste('set harvest-type',harvest))
    NLCommand(paste('set burnin',burnin))
    NLCommand(paste('set light-extinct',0.00025))
    NLCommand(paste('set HEE-mean TRUE'))
    
    #Setup site quality
    #if(!is.null(manual.site.quality)){
      NLCommand("set manual-site-qual TRUE")
      NLCommand(paste('set sqwoak',0.75))
      NLCommand(paste('set sqboak',0.75))
      NLCommand(paste('set sqmap',0.75))
      NLCommand(paste('set sqpop',0.75))
    #} else {
    #  NLCommand('set manual-site-qual FALSE')
    #  NLCommand(paste('set DegDays'),site.chars$degdays)
    #  NLCommand(paste('set wt-dist'),site.chars$wtdist)
    #  NLCommand(paste('set available-N'),site.chars$availableN)
    #  NLCommand(paste('set wilt'),site.chars$wilt)
    #}
    
    #Variables specific to IBM variant
    #if(model == 'ibm'){
      NLCommand(paste('set density-dep',3.5))
      seedlings = 'hee'
      NLCommand(paste('set seedlings \"',seedlings,'\"',sep=""))
      #if(sprouting == TRUE){
        NLCommand('set sprouting TRUE')
      #} else {NLCommand('set sprouting FALSE')}
      #if(seedlings != "none"){
        mast.scenario = "custom"
        weevil.scenario = "custom"
        #dispersal.scenario = "fixedaverage"
        dispersal.scenario = "custom"
        dispersal.distrib = "weibull"
        NLCommand(paste('set mast-scenario ','\"',mast.scenario,'\"',sep=""))
        NLCommand(paste('set mast-val',mast.val))
        NLCommand(paste('set weevil-scenario ','\"',weevil.scenario,'\"',sep=""))
        NLCommand(paste('set wo-weevil-prob',prob.weevil))
        NLCommand(paste('set bo-weevil-prob',prob.weevil))
        NLCommand(paste('set dispersal-scenario ','\"',dispersal.scenario,'\"',sep=""))
        NLCommand(paste('set dispersal-distrib ','\"',dispersal.distrib,'\"',sep=""))
        NLCommand(paste('set disperse-prob',param.set[i,5,harvestcount]))
        NLCommand(paste('set weibSc',param.set[i,6,harvestcount]))
        NLCommand(paste('set weibSh',param.set[i,7,harvestcount]))
        NLCommand(paste('set disperse-eaten-prob',param.set[i,8,harvestcount]))
        NLCommand(paste('set cache-prob',param.set[i,9,harvestcount]))
        NLCommand(paste('set undisp-eaten-prob',param.set[i,10,harvestcount]))
        #}
      #}
      #if(seedlings == "hee"){
        NLCommand(paste('set prob-browsed',prob.browsed))
        seed.scenario = "randomdrought"
        NLCommand(paste('set seed-scenario ','\"',seed.scenario,'\"',sep=""))
        browse.scenario = "custom"
        NLCommand(paste('set browse-scenario ','\"',browse.scenario,'\"',sep=""))
        NLCommand(paste('set drought-prob',prob.drought))
      #}
      #if(seedlings == "simple"){NLCommand(paste('set seedling-growth',maxgrowth))}
    #}
    
    #Setup NetLogo model
    NLCommand("setup")
    
    #Run and save output
    temp <- NLDoReport(length, "go", reporters, as.data.frame=TRUE, df.col.names=rep.names)
    
    #Return output
    #if(model == 'ibm' & seedlings !='none'){
      return(temp[,2:46])
    #} else {return(temp[,2:31])}
    
    gc()
    
  }
  
  out <- sapply(harvests,function(x) NULL)
  for (i in 1:length(harvests)){
    cl <- makeCluster(processors)
    on.exit(stopCluster(cl))
    clusterExport(cl = cl, ls(), envir = environment())
    invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                        nl.path=nl.path, model.path=model.path))
    on.exit(closeAllConnections())
    
    sim <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest= paste('\"',harvests[i],'\"',sep=""),
                        harvestcount=i
    )
    
    out[[i]] <- as.data.frame(matrix(data=NA,nrow=nreps,ncol=dim(param.set)[2]+length(rep.names)-1))
    names(out[[i]]) <- c('prob.browse','prob.weevil','prob.drought','mast.val','disperse.prob',
                         'weibSc','weibSh','disp.eaten.prob','cache.prob','undisp.eaten.prob',
                         rep.names[-1])
    out[[i]][,1:dim(param.set)[2]] <- param.set[,,i]
    for (j in 1:(length(rep.names)-1)){
      out[[i]][,j+dim(param.set)[2]] <- sapply(sim, function(x) x[[j]])[length,]
    }
    stopNL <- function(i){NLQuit()}
    invisible(parLapply(cl, 1:processors, stopNL))
    stopCluster(cl)
    closeAllConnections()
    Sys.sleep(30)
    gc()
    print(paste("Completed", harvests[i]))
  }
  
  #End time
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  
  out$runtime.minutes <- time
  
  #Stop NetLogo instances and cluster
  #stopNL <- function(i){NLQuit()}
  #invisible(parLapply(cl, 1:processors, stopNL))
  #stopCluster(cl)
  
  return(out)
  
  
  
}

