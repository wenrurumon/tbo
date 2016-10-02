
rm(list=ls())
load("C:/Users/zhu2/Documents/Lung_CT_CHINA/rlt2.rda")
library(graphics)

#########################################
# Macro
#########################################

plotg <- function(g,col=grey(0:64/64)){graphics::image(t(g),col=col)}
g.grad <- function(g,block=1){
  out <- matrix(0,nrow(g),ncol(g))
  for(i in (block+1):(ncol(g)-block)){
    for(j in (block+1):(ncol(g)-block)){
      out[i,j] <- max(abs(g[i,j]-g[-block:block+i,-block:block+j]))
    }
  }
  return(out)
}
g.2group <- function(g,grp=2,value=T){
  g2 <- kmeans(as.vector(g),range(as.vector(g)))$cluster
  if(value){
    g2 <- matrix(ifelse(g2==grp,as.vector(g),0),nrow(g),ncol(g))
  }else{
    g2 <- matrix(ifelse(g2==grp,1,0),nrow(g),ncol(g))
  }
  g2
}

edge.clean <- function(g){
  for(i in 1:ncol(g)){
    j = which(g[,i])[1]
    if(is.na(j)){next}
    while(g[j,i]){
      g[j,i] <- FALSE
      j=j+1
    }
  }
  for(i in ncol(g):1){
    j = which(g[,i])[1]
    if(is.na(j)){next}
    while(g[j,i]){
      g[j,i] <- FALSE
      j=j+1
    }
  }
  g
}
edge.clean2 <- function(g,times=1){
  for(i in 1:times){
    g <- edge.clean(g)
  }
  g
}

g.process <- function(g){
  g[g==-2000] <- 0
  g.grad(g)
}
g.align <- function(source,target){
  temp <- niftyreg.linear(source=source,target=target)
  temp <- niftyreg.nonlinear(source=temp$img,target=target)
  temp
}

#########################################
# Test
#########################################

par(mfrow=c(2,2))

g <- rlt2[[1]][,,100];plotg(g)
g2 <- g.2group(g,value=T); plotg(g2)
g3 <- g.grad(g.2group(g2,value=F)); plotg(g3)
g4 <- g3[rowSums(g3)>0,colSums(g3)>0];plotg(g4)

#

START <- c(lapply(which(g4[,1]!=0),function(x){c(x,1)}),
           lapply(which(g4[1,]!=0),function(x){c(1,x)}),
           lapply(which(g4[,ncol(g4)]!=0),function(x){c(x,ncol(g4))}),
           lapply(which(g4[nrow(g4),]!=0),function(x){c(nrow(g4),x)}))

go <- function(ij,g=g4){
  mi=nrow(g)
  mj=ncol(g)
  ijs <- rep(list(ij),4)
  ijs[[1]][1] <- ijs[[1]][1]+1
  ijs[[2]][1] <- ijs[[2]][1]-1
  ijs[[3]][2] <- ijs[[3]][2]+1
  ijs[[4]][2] <- ijs[[4]][2]-1
  ijs <- ijs[sapply(ijs,function(x){x[1]<=mi & x[1]>0 & x[2] <= mj & x[2] > 0})]
  ijs[sapply(ijs,function(ij){g[ij[1],ij[2]]==1})]
}

i <- 0
found <- c(START)
foundi <- do.call(c,lapply(found,go))
while(sum(!foundi%in%found)>0){
  found <- unique(c(found,foundi))
  foundi <- do.call(c,lapply(found,go))
}
for(ij in found){g4[ij[1],ij[2]]<-0}

plotg(g.grad(g2))
g5 <- g.grad(g2)
for(ij in found){g5[rowSums(g3)>0,colSums(g3)>0][ij[1],ij[2]]<-0}
plotg(g5)

#########################################
# Process
#########################################

raw <- rlt2

#First Stage Feature
out <- lapply(raw,function(x){
  array(0,dim=dim(x))
})
for(j in 1:length(out)){
  print(j)
  for(i in 1:dim(out[[j]])[3]){
    print(i)
    out[[j]][,,i] <- g.process(rlt2[[j]][,,i])
  }
}

g.target <- out[[1]]
system.time(g2 <- g.align(source=out[[2]],target=g.target))
system.time(g3 <- g.align(source=out[[3]],target=g.target))

par(mfrow=c(3,4))
for(i in 1:4){plotg(g.target[,,i*2+120])}
for(i in 1:4){plotg(g2$image[,,i*2+120])}
for(i in 1:4){plotg(g3$image[,,i*2+120])}
par(mfrow=c(2,2));for(i in 1:4){plotg(raw[[3]][,,100+i*2])}
