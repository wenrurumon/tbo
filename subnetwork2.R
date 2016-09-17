
#Always input a netowrk matrix and return a vector of cluster code


#####################################
# Cluster Macro
#####################################

library(data.table)
library(igraph)

rc <- function(x,thres=0){
  x[x<thres]<-0
  x.g <- graph_from_adjacency_matrix(x>0,mode='undirected')
  x.gs <- components(x.g)
  return(x.gs$membership)
}
fc <- function(x){
  w<-as.vector(t(x))[t(x)>0]
  x <- graph_from_adjacency_matrix(x>0,mode='undirected')
  fc <- membership(fastgreedy.community(x,weight=w))
  fc[] <- match(fc,unique(fc))
  fc
}

#####################################
# Supprting Macro
#####################################

subnetwork <- function(x,x.clust){
  lapply(unique(x.clust),function(i){
    x[x.clust%in%i,x.clust%in%i,drop=F]
  })
}
subrun <- function(x.sub,x.run,q=0){
  x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
    subnetwork(x,qfc(x,q))
  }))
  x.sub <- c(x.run_sub,x.sub[!x.run])
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
  names(x.clust) <- do.call(c,lapply(x.sub,colnames))
  return(list(subnets=x.sub,cluster=x.clust))
}
matrank <- function(x,score=T){
  while(sum(colSums(x>0)<=1)>0){
    x <- x[colSums(x>0)>1,colSums(x>0)>1,drop=F]
    if(ncol(x)==0){break}
  }
  if(score){
    x <- sum(x>0)/ncol(x)
    if(is.na(x)){return(0)}
  }
  return(x)
}
