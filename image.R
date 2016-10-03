
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

g.align <- function(source,target){
  temp <- niftyreg.linear(source=source,target=target)
  temp <- niftyreg.nonlinear(source=temp$img,target=target)
  temp
}

g.process <- function(g){
  # plotg(g)
  g <- g.2group(g,value=T)
  g2 <- g.2group(g,value=F)
  colsel <- range(which(colMeans(g2)>0.5))
  rowsel <- range(which(rowMeans(g2)>0.3))
  # return(c(rowsel,colsel))
  for(i in 1:512){
    g[i,1:max(which(g2[i,1:colsel[1]]==0))] <- 0
    g[i,(colsel[2]-1+min(which(g2[i,colsel[2]:512]==0))):512] <- 0
  }
  # plotg(g)
  # plotg(g[rowsel[1]:rowsel[2],colsel[1]:colsel[2]])
  return(list(graph=g,window=c(rowsel,colsel)))
}

gs.process <- function(out){
  print(1)
  # out <- rlt2[[1]]
  system.time(rlt.gprocess <- lapply(1:dim(out)[[3]],function(i){g.process(rlt2[[1]][,,i])}))
  sel <- sapply(rlt.gprocess,function(x){x$window})
  sel <- c(apply(sel,1,min)[c(1,3)],apply(sel,1,max)[c(2,4)])
  out2 <- array(0,dim=c(sel[3]-sel[1]+1,sel[4]-sel[2]+1,dim(out)[[3]]))
  for(i in 1:dim(out)[3]){out2[,,i] <- rlt.gprocess[[i]]$graph[sel[1]:sel[3],sel[2]:sel[4]]}
  return(out2)
}

#########################################
# Test
#########################################

#########################################
# Process
#########################################

system.time(test <- lapply(rlt2,gs.process))
