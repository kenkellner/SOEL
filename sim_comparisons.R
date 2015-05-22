system('export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre')
system('export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/client')

library(RNetLogo)
nl.path <- "/home/kkellner/programs/netlogo-5.2.0"

nreps <- 10
nyears <- 150

BAclearcut.jabowa <- matrix(NA,nrow=nyears,ncol=(nreps+1))
BAclearcut.jabowa <- as.data.frame(BAclearcut.jabowa)
names(BAclearcut.jabowa) <- c('ticks',1:10)

jabowa1 <- 'jabowa1'
NLStart(nl.path, gui=F, nl.obj=jabowa1)
model.path <- "/home/kkellner/analysis/oak-lifecycle/oak_ibm_jabowa.nlogo"
NLLoadModel(model.path, nl.obj=jabowa1)

for (i in 1:nreps){

  NLCommand("setup", nl.obj=jabowa1)
  temp <- NLDoReport(150, "go", c("ticks","basal-area"), 
                         as.data.frame=TRUE,df.col.names=c("tick","BA"), 
                         nl.obj=jabowa1)
  if(i == 1){BAclearcut.jabowa[,1] = temp$tick}
  BAclearcut.jabowa[,(i+1)] = temp$BA
}
NLQuit(jabowa1)

plot(BAclearcut.jabowa$ticks,rowMeans(BAclearcut.jabowa[,2:11]),type='l')

#Parallel version
#http://cran.r-project.org/web/packages/RNetLogo/vignettes/parallelProcessing.pdf

val.sim <- function(nreps,nyears,model.path,seedlings=NULL,heegrowth=FALSE){


  #Setup function
  # the initialization function
  initNL <- function(dummy, gui, nl.path, model.path) {
    library(RNetLogo)
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(model.path)
  }

  require(parallel)
  processors <- detectCores()
  cl <- makeCluster(processors)

  invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                    nl.path=nl.path, model.path=model.path))

  #The setup/go function
  runNL <- function(i,harvest,nyears,seedlings=NULL,heegrowth=NULL) {
    NLCommand(paste('set harvest-type',harvest))
    NLCommand("set manual-site-qual TRUE")
    NLCommand("set sqwoak 0.75")
    NLCommand("set sqboak 0.75")
    NLCommand("set sqpop 0.75")
    NLCommand("set sqmap 0.75")
    if(!is.null(seedlings)){
      if(seedlings){
        #Make adjustments here later
        NLCommand("set oak-seedlings TRUE")
        if(heegrowth){
          NLCommand("set hee-seedlings TRUE")         
        } else {
          NLCommand("set hee-seedlings FALSE")
          NLCommand("set seedling-growth 0.60")
        }
      } else{
        NLCommand("set oak-seedlings FALSE")        
      }      
    }
    NLCommand("setup")
    temp <- NLDoReport(nyears, "go", c("ticks","basal-area","prop-oak","prop-tol","prop-intol"), 
                     as.data.frame=TRUE,df.col.names=c("tick","BA","oak","tol","intol"))
    return(temp[,2:5])  
  }

  par.clear <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest='\"clearcut\"',
                            nyears=nyears,seedlings=seedlings,heegrowth=heegrowth)
  par.none <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest='\"none\"',
                           nyears=nyears,seedlings=seedlings,heegrowth=heegrowth)
  par.single <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest='\"single-tree\"',
                             nyears=nyears,seedlings=seedlings,heegrowth=heegrowth)
  par.shelter <- clusterApply(cl=cl,x=1:nreps,fun=runNL,harvest='\"shelterwood\"',
                              nyears=nyears,seedlings=seedlings,heegrowth=heegrowth)
  
  clear = list(BA=sapply(par.clear, function(x) x[[1]]),oak=sapply(par.clear, function(x) x[[2]]),
               tol=sapply(par.clear, function(x) x[[3]]),intol=sapply(par.clear, function(x) x[[4]]))
  
  none = list(BA=sapply(par.none, function(x) x[[1]]),oak=sapply(par.none, function(x) x[[2]]),
               tol=sapply(par.none, function(x) x[[3]]),intol=sapply(par.none, function(x) x[[4]]))
  
  single = list(BA=sapply(par.single, function(x) x[[1]]),oak=sapply(par.single, function(x) x[[2]]),
               tol=sapply(par.single, function(x) x[[3]]),intol=sapply(par.single, function(x) x[[4]]))
  
  shelter = list(BA=sapply(par.shelter, function(x) x[[1]]),oak=sapply(par.shelter, function(x) x[[2]]),
               tol=sapply(par.shelter, function(x) x[[3]]),intol=sapply(par.shelter, function(x) x[[4]]))
  
  out = list(clear=clear,none=none,single=single,shelter=shelter)

  #Stop netlogo instances and cluster
  stopNL <- function(i){NLQuit()}
  invisible(parLapply(cl, 1:processors, stopNL))
  stopCluster(cl)
  
  return(out)
}

model.path <- "/home/kkellner/analysis/oak-lifecycle/oak_ibm_jabowa.nlogo"

jabowa.out <- val.sim(4,150,model.path,seedlings=NULL,heegrowth=NULL)

model.path <- "/home/kkellner/analysis/oak-lifecycle/oak_ibm.nlogo"
#No seedlings
noseedlings.out <- val.sim(4,150,model.path,seedlings=FALSE,heegrowth=NULL)
#Seedlings, HEE growth data
seedlings.out <- val.sim(4,150,model.path,seedlings=TRUE,heegrowth=TRUE)
#Seedlings, literature growth data
seedlings.out <- val.sim(4,150,model.path,seedlings=TRUE,heegrowth=FALSE)






