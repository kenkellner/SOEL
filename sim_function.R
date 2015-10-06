###########################################################
## Function to control forest IBM simulations in NetLogo ##
###########################################################

#Dependencies
#Java (needed for NetLogo anyway)
#install.packages('RNetLogo')
require(parallel)

#May need to manually set system paths to java install, e.g. below:
#system('export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre')
#system('export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/client')
#system('sudo R CMD javareconf')
#https://stackoverflow.com/questions/12872699/error-unable-to-load-installed-packages-just-now

#Netlogo model files should be in current working directory

#Path to NetLogo installation based on OS
if(Sys.info()[['sysname']] == "Windows"){
  nl.path <- "C:/Program Files (x86)/Netlogo 5.2.0"}
if(Sys.info()[['sysname']] == "Linux"){
  nl.path <- paste('/home/',Sys.info()[['user']],'/programs/netlogo-5.2.0',sep="")}

forest.sim <- function(model = 'ibm', #Model type (ibm or jabowa)
                       #Size of simulation in m (ibm) or cells (jabowa, e.g. 10,10,2)
                       xcorewidth = 100, ycorewidth = 100, buffer = 20, 
                       #Harvest types to include in run; default is all
                       harvests = c('none','clearcut', 'shelterwood', 'singletree'),
                       #Number of reps per harvest type
                       nreps = 1, 
                       #Burn-in ticks and total length of each rep
                       burnin = 10, nyears = 150,
                       #Manual control of site qualities for each species
                       manual.site.quality = list(whiteoak=0.75,blackoak=0.75,maple=0.75,poplar=0.75),
                       #If above is NULL, use values  below to calculate site qualities by species
                       site.chars = list(degdays = 4840, wtdist = 9.3, availableN = 325, wilt = 0.10),
                       #Tuning parameters; density.dep is used only for ibm, not jabowa
                       extinct = 0.00025, density.dep = 3.5,
                       #Toggle stump sprouting
                       sprouting = TRUE,
                       #Seedling handling method; 'none' for no seedlings, 
                       #'simple' for simple growth based on literature,
                       #'hee' for growth based on HEE data
                       seedlings = 'hee',
                       #Seedling growth/survival scenario; options are
                       #'fixedaverage' or 'randomdrought'
                       #'#Only works when seedlings = 'hee'
                       seedling.scenario = 'fixedaverage',
                       prob.drought = 0.2,
                       #Browse scenario (also only works when seedlings = 'hee')
                       #'fixedaverage', 'hee' for yearly variation, or 'custom' to set manually
                       browse.scenario = 'fixedaverage',
                       prob.browsed = 0,
                       #Masting cycle control; 'fixedaverage', 'fixedgood', fixedbad' for constant values
                       #'hee' for cycling through each year of HEE data repeatedly
                       #'random' for randomly selecting a year of HEE data
                       #'priorgood' or 'priorbad' for two good/bad mast years just before harvest
                       mast.scenario = 'fixedaverage',
                       #Weevil scenario, 'fixedaverage', 'random', 'random-match', 
                       #'hee', 'treat-diff', or 'custom'
                       weevil.scenario = 'fixedaverage',
                       prob.weevil = 0.3,
                       #Dispersal scenario, 'fixedaverage', 'custom', 'treat-diff', 'yearly-diff',
                       #'treat-yearly-diff', or 'random'
                       dispersal.scenario = 'fixedaverage',
                       #Acorn transition probabilities (list) to use if above is set to custom
                       acorn = list(disperse=0.41,weibSc=7.578,
                                    disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                       #Maximum yearly seedling growth if 'simple' is selected
                       maxgrowth = 0.9,
                       #Absolute path to NetLogo directory
                       netlogo.path = nl.path,
                       #Force a certain number of cores to be used (otherwise all cores are used)
                       force.processors = NULL,
                       #RAM per processor maximum in MB
                       ram.max = 5000,
                       sensitivity = FALSE,
                       #Arguments for sensitivity test
                       covs = NULL, q = NULL, qarg = NULL, corm = NULL
                       ){
  
  #Model start time
  start.time <- Sys.time()
  
  #Get path to appropriate model
  if(model == 'ibm'){model.path <- paste(getwd(),"/oak_ibm.nlogo",sep="")
  } else {model.path <- paste(getwd(),"/oak_ibm_jabowa.nlogo",sep="")}
  
  #Set list of resporters to save
  if(model == 'ibm' & seedlings != 'none'){
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
  } else {
    reporters <- c("ticks","basal-area","basal-area-ovs",
                   "ba-oak","ba-oak-ovs","ba-map","ba-map-ovs","ba-pop","ba-pop-ovs",
                   "dens","dens-ovs",
                   "dens-oak","dens-oak-ovs","dens-map","dens-map-ovs","dens-pop","dens-pop-ovs",
                   "qdbh","qdbh-ovs",
                   "qdbh-oak","qdbh-oak-ovs","qdbh-map","qdbh-map-ovs","qdbh-pop","qdbh-pop-ovs",
                   "prop-oak","prop-tol","prop-intol",
                   "seedlings-class4","seedbo-class4","seedwo-class4")
    rep.names <- c("ticks","ba","ba.ovs",
                   "ba.oak","ba.oak.ovs","ba.map","ba.map.ovs","ba.pop","ba.pop.ovs",
                   "dens","dens.ovs",
                   "dens.oak","dens.oak.ovs","dens.map","dens.map.ovs","dens.pop","dens.pop.ovs",
                   "qdbh","qdbh.ovs",
                   "qdbh.oak","qdbh.oak.ovs","qdbh.map","qdbh.map.ovs","qdbh.pop","qdbh.pop.ovs",
                   "prop.oak","prop.tol","prop.intol",
                   "seedclass4","seedboclass4","seedwoclass4")
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
  
  #Set number of processors to use
  if(!is.null(force.processors)){processors <- force.processors
  } else {processors <- detectCores()}
  
  #Generate Latin hypercube if this is a sensitivity analysis
  if(sensitivity){
    require(pse)
    lhc <- LHS(model=NULL,factors=covs,N=nreps,q=q,q.arg=qarg,opts=list(COR=corm))$data
  }
  
  #Internal function to set variables and run NetLogo in each process
  runNL <- function(i,harvest) {
    
    #Garbage collection in R and Java
    gc()
    jgc()
    
    #Set variables if this is a sensitivity analysis
    if(sensitivity){
      mast.scenario <- "custom"
      weevil.scenario <- "custom"
      dispersal.scenario <- "custom"
      seedling.scenario <- "custom"
      browse.scenario <- "custom"
      
      mean.growth <- lhc$meanGr
      mean.survival <- lhc$meanSurv
      prob.browsed <- lhc$pBrowse[i]
      prob.weevil <- lhc$pWeevil[i]
      prob.drought <- 0
      mast.val <- lhc$lamAcorn[i]
      
      acorn <- list(disperse=lhc$pDispersal[i],weibSc=lhc$weibSc[i],
                    disperse.eaten=lhc$pDispEaten[i],
                    cache.prob=lhc$pCache[i],undisperse.eaten=lhc$pUndispEaten[i])
    }
    
    #Select harvest type, seedling type, etc.
    NLCommand(paste('set x-core',xcorewidth))
    NLCommand(paste('set y-core',ycorewidth))
    NLCommand(paste('set buffer',buffer))
    NLCommand(paste('set harvest-type',harvest))
    NLCommand(paste('set burnin',burnin))
    NLCommand(paste('set light-extinct',extinct))
    NLCommand(paste('set HEE-mean TRUE'))
    
    #Setup site quality
    if(!is.null(manual.site.quality)){
      NLCommand("set manual-site-qual TRUE")
      NLCommand(paste('set sqwoak',manual.site.quality$whiteoak))
      NLCommand(paste('set sqboak',manual.site.quality$blackoak))
      NLCommand(paste('set sqmap',manual.site.quality$maple))
      NLCommand(paste('set sqpop',manual.site.quality$poplar))
    } else {
      NLCommand('set manual-site-qual FALSE')
      NLCommand(paste('set DegDays'),site.chars$degdays)
      NLCommand(paste('set wt-dist'),site.chars$wtdist)
      NLCommand(paste('set available-N'),site.chars$availableN)
      NLCommand(paste('set wilt'),site.chars$wilt)
    }
    
    #Variables specific to IBM variant
    if(model == 'ibm'){
      NLCommand(paste('set density-dep',density.dep))
      NLCommand(paste('set seedlings \"',seedlings,'\"',sep=""))
      if(sprouting == TRUE){NLCommand('set sprouting TRUE')
      } else {NLCommand('set sprouting FALSE')}
      if(seedlings != "none"){
        NLCommand(paste('set mast-scenario ','\"',mast.scenario,'\"',sep=""))
        NLCommand(paste('set weevil-scenario ','\"',weevil.scenario,'\"',sep=""))
        NLCommand(paste('set wo-weevil-prob',prob.weevil))
        NLCommand(paste('set bo-weevil-prob',prob.weevil))
        NLCommand(paste('set dispersal-scenario ','\"',dispersal.scenario,'\"',sep=""))
        if(dispersal.scenario == "custom"){
          NLCommand(paste('set disperse-prob',acorn$disperse))
          NLCommand(paste('set weibSc',acorn$weibSc))
          NLCommand(paste('set disperse-eaten-prob',acorn$disperse.eaten))
          NLCommand(paste('set cache-prob',acorn$cache.prob))
          NLCommand(paste('set undisp-eaten-prob',acorn$undisperse.eaten))
        }
      }
      if(seedlings == "hee"){
        NLCommand(paste('set prob-browsed',prob.browsed))
        NLCommand(paste('set seedling-scenario ','\"',seedling.scenario,'\"',sep=""))
        NLCommand(paste('set browse-scenario ','\"',browse.scenario,'\"',sep=""))
        NLCommand(paste('set drought-prob',prob.drought))
        if(seedling.scenario = "custom"){
          NLCommand(paste('set mean-growth',mean.growth))
          NLCommand(paste('set mean-survival',mean.survival))
        }
        }
      if(seedlings == "simple"){NLCommand(paste('set seedling-growth',maxgrowth))}
    }
    
    #Setup NetLogo model
    NLCommand("setup")
    
    #Run and save output
    temp <- NLDoReport(nyears, "go", reporters, as.data.frame=TRUE, df.col.names=rep.names)
    
    #Return output
    if(model == 'ibm' & seedlings !='none'){return(temp[,2:46])
    } else {return(temp[,2:31])}
    
    gc()
      
  }
  
  #Generate and format output
  out <- sapply(harvests,function(x) NULL)
  for (i in 1:length(harvests)){
    cl <- makeCluster(processors)
    on.exit(stopCluster(cl))
    clusterExport(cl = cl, ls(), envir = environment())
    invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                        nl.path=netlogo.path, model.path=model.path))
    on.exit(closeAllConnections())
    sim <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest= paste('\"',harvests[i],'\"',sep=""))
    out[[i]] <- sapply(rep.names[-1],function(x) NULL)
    for (j in 1:(length(rep.names)-1)){
      out[[i]][[j]] <- sapply(sim, function(x) x[[j]])
    }
    stopNL <- function(i){NLQuit()}
    invisible(parLapply(cl, 1:processors, stopNL))
    stopCluster(cl)
    closeAllConnections()
    Sys.sleep(10)
    gc()
    print(paste("Completed", harvests[i]))
  }
  
  #End time
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  
  out$runtime.minutes <- time

  if(sensitivity){return(list(lhc=lhc,out=out))
  } else{return(out)}
 
}

