
#rJava error: 
#https://stackoverflow.com/questions/23469061/why-does-rjava-not-work-on-ubuntu-14-04-using-openjdk-7
#might need to restart rstudio-server
system('export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre')
system('export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/client')

library(RNetLogo)
#Path to NetLogo installation based on OS
if(Sys.info()[['sysname']] == "Windows"){nl.path <- "C:/Program Files (x86)/Netlogo 5.2.0"}
if(Sys.info()[['sysname']] == "Linux"){nl.path <- paste('/home/',Sys.info()[['user']],'/programs/netlogo-5.2.0',sep="")}

#Example proof-of-concept
nlheadless1 <- "nlheadless1"
NLStart(nl.path, gui=F, nl.obj=nlheadless1)
model.path <- "/models/Sample Models/Earth Science/Fire.nlogo"
NLLoadModel(paste(nl.path,model.path,sep=""), nl.obj=nlheadless1)
NLCommand("setup", nl.obj=nlheadless1)
burned1 <- NLDoReport(20, "go", c("ticks","burned-trees"), 
                      as.data.frame=TRUE,df.col.names=c("tick","burned"), 
                      nl.obj=nlheadless1)
print(burned1)
NLQuit(nlheadless1)

#Simple JABOWA example
jabowa1 <- 'jabowa1'
NLStart(nl.path, gui=F, nl.obj=jabowa1)
model.path <- "/home/kkellner/analysis/oak-lifecycle/oak_ibm_jabowa.nlogo"
NLLoadModel(model.path, nl.obj=jabowa1)
NLCommand("setup", nl.obj=jabowa1)
baclearcut <- NLDoReport(50, "go", c("ticks","basal-area"), 
                      as.data.frame=TRUE,df.col.names=c("tick","BA"), 
                      nl.obj=jabowa1)
plot(baclearcut$tick,baclearcut$BA,type='l')

NLQuit(jabowa1)
