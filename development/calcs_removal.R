
acorneh <- read.csv('../oak-mast/data/hee_acorn_eh.csv',header=T)
acorncovs <- read.csv('../oak-mast/data/hee_acorn_covs.csv',header=T)
treedata <- read.csv('data/hee_treedata.csv')
codes <- read.csv('data/treecodes.csv',header=T)
add <- read.csv('data/remadd.csv')

weev <- rep(0,length=dim(acorneh)[1])
for (i in 1:length(weev)){
  if(sum(acorneh[i,10:17],na.rm=T)>0){
    weev[i] <- 1
  }
}

new = data.frame(acorncovs$treecode,acorncovs$yearcode,acorncovs$species,
            acorncovs$open,acorncovs$deer,acorncovs$rem,weev)
names(new)<-c('treecode','yearcode','species','open','deer','rem','weev')
new = new[new$open==1|new$deer==1,c(1:3,6,7)]

newcode <- rep(NA,dim(add)[1])
add$tree = as.character(add$tree)
for (i in 1:length(newcode)){
  newcode[i] <- codes$Treecode[which(add$tree[i]==codes$Tree)]
}

add = data.frame(add,newcode)
add = add[add$exclosure%in%c('Open','Deer'),]

new2 = data.frame(treecode=add$newcode,yearcode=add$year,species=add$species,
                  rem=add$removed,weev=add$weevil)

fulldata = rbind(new,new2)

n.edge = n.shelter = rep(0,dim(fulldata)[1])
for (i in 1:dim(fulldata)[1]){
  if(fulldata$yearcode[i]>3){
    n.edge[i] <- treedata$edge[fulldata$treecode[i]]
    n.shelter[i] <- treedata$shelter[fulldata$treecode[i]]
  }
}

full <- data.frame(fulldata,edge=n.edge,shelter=n.shelter)

final <- full[full$edge==0,]

mast = rep(NA,dim(final)[1])
for (i in 1:length(mast)){
  mast[i] <- mean(womast[final$yearcode[i]],bomast[final$yearcode[i]])
}

rm <- glm(rem~species+shelter+weev,data=final,family='binomial')
summary(rm)

rm <- glmmPQL(rem~species+shelter+weev,data=final,family='binomial',random= ~ 1 | yearcode)
summary(rm)


