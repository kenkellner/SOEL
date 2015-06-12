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
                       #Masting cycle control; 'fixedaverage', 'fixedgood', fixedbad' for constant values
                       #'hee' for cycling through each year of HEE data repeatedly
                       #'random' for randomly selecting a year of HEE data
                       #'priorgood' or 'priorbad' for two good/bad mast years just before harvest
                       mastscenario = 'fixedaverage',
                       #Acorn transition probabilities (list)
                       acorn = list(weevil=0.31,disperse=0.41,disperse.dist=5.185,
                                    disperse.eaten=0.704,cache.prob=0.288,undisperse.eaten=0.538),
                       #Browse probability if 'hee' is selected
                       browse = 0,
                       #Maximum yearly seedling growth if 'simple' is selected
                       maxgrowth = 0.9,
                       #Absolute path to NetLogo directory
                       netlogo.path = nl.path,
                       #Force a certain number of cores to be used (otherwise all cores are used)
                       force.processors = NULL
                       ){
  
  #Get path to appropriate model
  if(model == 'ibm'){model.path <- paste(getwd(),"/oak_ibm.nlogo",sep="")
  } else {model.path <- paste(getwd(),"/oak_ibm_jabowa.nlogo",sep="")}
  
  #Set list of resporters to save
  if(model == 'ibm' & seedlings != 'none'){
    reporters <- c("ticks","basal-area","prop-oak","prop-tol","prop-intol",
                   "total-acorns","acorns-pertree","total-seedlings","new-seedlings","pct-germ",
                   "regen-dens","regen-stump-dens",
                   "seedlings-class1","seedlings-class2","seedlings-class3",
                   "seedlings-class123","seedlings-class4")
    rep.names <- c("tick","BA","oak","tol","intol","totacorns","acornspertree","totseedlings",
                   "newseedlings","pctgerm","regen",'regenstump',
                   "seedclass1","seedclass2","seedclass3","seedclass123","seedclass4")
  } else {
    reporters <- c("ticks","basal-area","prop-oak","prop-tol","prop-intol",
                   "regen-dens","regen-stump-dens","seedlings-class4")
    rep.names <- c("tick","BA","oak","tol","intol","regen","regenstump","seedclass4")
  }
  
  #Internal function to setup NetLogo in each parallel subprocess
  initNL <- function(dummy, gui, nl.path, model.path) {
    library(RNetLogo)
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(model.path)
  }
  
  #Setup each parallel process
  if(!is.null(force.processors)){processors <- force.processors
  } else {processors <- detectCores()}
  cl <- makeCluster(processors)
  clusterExport(cl = cl, ls(), envir = environment())
  invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                      nl.path=netlogo.path, model.path=model.path))
  
  #Internal function to set variables and run NetLogo in each process
  runNL <- function(i,harvest) {
    #Select harvest type, seedling type, etc.
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
        NLCommand(paste('set mast-scenario ','\"',mastscenario,'\"',sep=""))
        NLCommand(paste('set weevil-probability',acorn$weevil))
        NLCommand(paste('set disperse-prob',acorn$disperse))
        NLCommand(paste('set disperse-dist',acorn$disperse.eaten))
        NLCommand(paste('set disperse-eaten-prob',acorn$cache.prob))
        NLCommand(paste('set cache-prob',acorn$weevil))
        NLCommand(paste('set undisp-eaten-prob',acorn$undisperse.eaten))
      }
      if(seedlings == "hee"){NLCommand(paste('set prob-browsed',browse))}
      if(seedlings == "simple"){NLCommand(paste('set seedling-growth',maxgrowth))}
    }
    
    #Setup NetLogo model
    NLCommand("setup")
    
    #Run and save output
    temp <- NLDoReport(nyears, "go", reporters, as.data.frame=TRUE, df.col.names=rep.names)
    
    #Return output
    if(model == 'ibm' & seedlings !='none'){return(temp[,2:17])
    } else {return(temp[,2:8])}
      
  }
  
  #Generate and format output
  out <- sapply(harvests,function(x) NULL)
  for (i in 1:length(harvests)){
    sim <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest= paste('\"',harvests[i],'\"',sep=""))
    out[[i]] <- sapply(rep.names[-1],function(x) NULL)
    for (j in 1:(length(rep.names)-1)){
      out[[i]][[j]] <- sapply(sim, function(x) x[[j]])
    }
  }
  
  #Stop NetLogo instances and cluster
  stopNL <- function(i){NLQuit()}
  invisible(parLapply(cl, 1:processors, stopNL))
  stopCluster(cl)
  
  return(out)
 
}

