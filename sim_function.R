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
                       harvests = c('none','clearcut', 'shelterwood', 'single-tree'),
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
                       #Seedling handling method; 'none' for no seedlings, 
                       #'simple' for simple growth based on literature,
                       #'hee' for growth based on HEE data
                       seedlings = 'hee', 
                       #Maximum yearly seedling growth if 'simple' is selected
                       maxgrowth = 0.9,
                       #More regen parameters here later
                       #Absolute path to NetLogo directory
                       nl.path = "/home/kkellner/programs/netlogo-5.2.0"
                       ){
  
  #Get path to appropriate model
  if(model == 'ibm'){model.path <- paste(getwd(),"/oak_ibm.nlogo",sep="")
  } else {model.path <- paste(getwd(),"/oak_ibm_jabowa.nlogo",sep="")}
  
  #Internal function to setup NetLogo in each parallel subprocess
  initNL <- function(dummy, gui, nl.path, model.path) {
    library(RNetLogo)
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(model.path)
  }
  
  #Setup each parallel process
  processors <- detectCores()
  cl <- makeCluster(processors)
  invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                      nl.path=nl.path, model.path=model.path))
  
  #Internal function to set variables and run NetLogo in each process
  runNL <- function(i,harvest) {
    #Select harvest type, seedling type, etc.
    NLCommand(paste('set harvest-type',harvest))
    NLCommand(paste('set burnin',burnin))
    NLCommand(paste('set extinct',extinct))
    
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
    if(model = 'ibm'){
      NLCommand(paste('set density-dep',density-dep))
      NLCommand(paste('set seedlings',seedlings))
      if(seedlings == "simple"){NLCommand(paste('set seedling-growth',maxgrowth))}
    }
    
    #Setup NetLogo model
    NLCommand("setup")
    
    #Run and save output
    temp <- NLDoReport(nyears, "go", c("ticks","basal-area","prop-oak","prop-tol","prop-intol"), 
                       as.data.frame=TRUE,df.col.names=c("tick","BA","oak","tol","intol"))
    
    #Return output
    return(temp[,2:5])  
  }
  
  
  
  
  
  
  
}







