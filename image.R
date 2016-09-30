
rm(list=ls())
load("C:/Users/zhu2/Documents/Lung_CT_CHINA/rlt2.rda")
library(graphics)

#########################################
# Macro
#########################################

plotg <- function(g,col=grey(0:64/64)){graphics::image(g,col=col)}
g.grad <- function(g,block=1){
  out <- matrix(0,nrow(g),ncol(g))
  for(i in (block+1):(ncol(g)-block)){
    for(j in (block+1):(ncol(g)-block)){
      out[i,j] <- max(abs(g[i,j]-g[-block:block+i,-block:block+j]))
    }
  }
  return(out)
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
