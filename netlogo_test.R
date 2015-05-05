
#rJava error: 
#https://stackoverflow.com/questions/23469061/why-does-rjava-not-work-on-ubuntu-14-04-using-openjdk-7
#might need to restart rstudio-server
system('export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre')
system('export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64:$JAVA_HOME/jre/lib/amd64/client')

library(RNetLogo)
nl.path <- "/home/kkellner/programs/netlogo-5.2.0"

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
