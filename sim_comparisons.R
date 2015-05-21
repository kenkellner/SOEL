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

#Setup function
# the initialization function
initNL <- function(dummy, gui, nl.path, model.path) {
  library(RNetLogo)
  NLStart(nl.path, gui=gui)
  NLLoadModel(model.path)
}

library(parallel)
processors <- detectCores()
cl <- makeCluster(processors)

invisible(parLapply(cl, 1:processors, initNL, gui=FALSE,
                    nl.path=nl.path, model.path=model.path))

#The setup/go function
runNL <- function(i) {
  NLCommand("setup")
  temp <- NLDoReport(150, "go", c("ticks","basal-area","prop-oak","prop-tol","prop-intol"), 
                     as.data.frame=TRUE,df.col.names=c("tick","BA","oak","tol","intol"))
  
  NLQuit()
  return(temp[,2:5])  
}

par <- clusterApply(cl=cl,x=1:processors,fun=runNL)

