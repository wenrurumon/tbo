
################################################
# Macro
################################################

rm(list=ls())

library(kernlab)
library(igraph)
library(KRLS)
library(slam)
library(corrplot)
library(MCL)
library(sparcl)

#greedy optimization
fcclust <- function(x,w=FALSE){
  x.g <- graph_from_adjacency_matrix(x>0,mode='undirected')
  if(w){w<-as.vector(t(x))[t(x)>0]}else{w<-NULL}
  fc <- fastgreedy.community(x.g,weight=w)
  subnets <- lapply(unique(membership(fc)),function(i){x[which(membership(fc)==i),which(membership(fc)==i),drop=F]})
  rlt <- list(subnets=subnets,cluster=membership(fc))
  return(rlt)
}
#apply fcclust on multi subnetworks
apply_fcclust <- function(x.gs,w=TRUE){
  rlt <- lapply(x.gs,function(x){
    if(length(x)==1){
      cluster=1; names(cluster)=colnames(x)
      return(cluster=cluster)
    } else {
      return(fcclust(x,w)$cluster)
    }
  })
  if (length(rlt)>1){
    for(i in 2:length(rlt)){
      rlt[[i]] <- max(rlt[[i-1]])+rlt[[i]]
    }
  }
  rlt <- do.call(c,rlt)
  cluster=rlt[order(as.numeric(names(rlt)))]
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  return(list(subnets=subnets,cluster=cluster)) 
}
#Run fcclust in loop till converge
fcclusts <- function(x,l=0,thres=0,w=TRUE){
  rlt <- cutnet(x,thres=0,w=w)
  x.len <- length(rlt[[1]])
  print(paste('Initialization pruning: length =',x.len))
  if (l==0) {l<-Inf}
  li <- 0
  while(li<l){
    li <- li+1
    rlt <- apply_fcclust(rlt[[1]])
    if(length(rlt[[1]])==x.len){
      print(paste('Prune end at loop ',li-1,',length = ',x.len))
      break
    }else{
      x.len <- length(rlt[[1]])
      print(paste('#loops = ',li,',length = ',x.len))
    }
  }
  return(rlt)
}
#Spectral Clustering
spclust <- function(x,centers){
  rlt <- specc(x,centers)
  cluster <- as.vector(rlt)
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  rlt <- list(subnets=subnets,cluster=as.vector(rlt),centers=centers(rlt))
  return(rlt)
}
#Kmeans
kmclust <- function(x,centers,iter.max=100,nstart=100){
  rlt <- kmeans(x,centers,iter.max=iter.max,nstart=nstart)
  cluster <- rlt$cluster
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  rlt <- list(subnets=subnets,cluster=rlt$cluster,centers=rlt$centers)
  return(rlt)
}
#Kmeans with layers
dkmclust <- function(x,centers,iter.max=100,nstart=100,layers=3){
  x.g <- graph_from_adjacency_matrix(x>0)
  x.d <- degree(x.g,mode='all')
  D <- diag(1/x.d)
  S <- similarity.jaccard(x.g)
  X <- D %*% S
  for(l in 1:layers) {
    K <- kmclust(x=X,centers=dim(X)[1]/2,iter.max=iter.max,nstart=nstart)
    X <- K$centers
  }
  rlt <- kmeans(t(X), centers=centers, iter.max=iter.max, nstart=nstart)
  cluster <- rlt$cluster
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  rlt <- list(subnets=subnets,cluster=rlt$cluster,centers=rlt$centers)
  return(rlt)
}
#Markov Clustering
mkvclust <- function(x,addLoop=TRUE,allow1=FALSE){
  rlt <- mcl(x,addLoops=TRUE,allow1=FALSE)
  cluster <- rlt$Cluster
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  list(subnets=subnets,cluster=rlt$Cluster)
}
#Sparse Hierarchical Clustering
sparsehclust <- function(x,centers,wbounds=c(1.5,2:6),nperms=5,niter=100){
  perm.out <- HierarchicalSparseCluster.permute(x, wbounds=wbounds,nperms=nperms)
  sparsehc <- HierarchicalSparseCluster(dists=perm.out$dists,wbound=perm.out$bestw,method="complete",niter=niter)
  cluster <- as.numeric(cutree(sparsehc$hc,centers))
  subnets <- lapply(unique(cluster),function(i){x[which(cluster==i),which(cluster==i),drop=F]})
  list(subnets=subnets,cluster=cluster)
}
#Cut network by thres on weights
cutnet <- function(x,thres=0,w=T){
  x.raw <- x
  x[x<thres] <- 0
  if(!w){x[x>0]<-1}
  w<-as.vector(t(x))[t(x)>0]
  x.g <- graph_from_adjacency_matrix(x>0,mode='undirected')
  x.gs <- components(x.g)
  rlt <- lapply(1:x.gs$no,function(i){x.raw[x.gs$membership==i,x.gs$membership==i,drop=F]})
  return(list(subnets=rlt,cluster=(x.gs$membership)))
}
#Plot
plotnet <- function(x,       
                    edge.arrow.size=1,vertex.size=10,vertex.label.cex=1,edge.width= 1){
  plot(graph_from_adjacency_matrix(t(as.matrix(x>0)),
       mode='undirected'),
       edge.arrow.size=edge.arrow.size,
       vertex.size=vertex.size,
       vertex.label.cex=vertex.label.cex,
       edge.width=edge.width
       )
}
plotclust <- function(x,membership=NULL,main=''){
  G <- graph_from_adjacency_matrix(x>0)
  if(is.null(membership)){membership=rep(1,ncol(x))}
  plot(create.communities(G, membership), 
       as.undirected(G), 
       layout=layout.kamada.kawai(as.undirected(G)),
       main=main,
       edge.arrow.size=0.01,
       vertex.size=1,
       vertex.label.cex=.5)
}
#Evaluation of the subnetwork
calcclust <- function(x,x.clust){
  sapply(unique(x.clust),function(i){
    xin <- x[x.clust==i,x.clust==i]
    xout <- x[x.clust!=i,x.clust!=i]
    return(c(xin=sum(xin>0),xall=sum(x>0)-sum(xout>0),xinw=sum(xin),xallw=sum(x)-sum(xout)))
  })
}
checknets <- function(i,j,x.clust,w=T){
  seli <- which(x.clust==i)
  selj <- which(x.clust==j)
  xij <- x[seli,selj]
  c(sum(xij),sum(xij>0))
}
#Validation
clust_score <- function(x,x.clust){
  xbase <- sum(x)/sum(colSums(x)>0)
  sapply(unique(x.clust),function(g){
    xin <- (x[which(x.clust==g),which(x.clust==g)])
    xout <- (x[which(x.clust==g),which(x.clust!=g)])
    w_in <- sum(xin)/2#inloop weight
    c_in <- ncol(xin)#inloop count
    w_out <- sum(xout)#outloop weight
    c_out <- sum(colSums(xout)>0)#outloop weight
    (w_in/c_in)/(w_out/c_out)
  })/xbase
}

#Mix clustering
mixclust <- function(x.g,thres=0,w=TRUE,thres_score=NULL,layer=Inf){
  dimnames(x.g) <- list(1:nrow(x.g),1:ncol(x.g))
  float_thres <- is.null(thres_score)
  #Cutnet in the first stage
  x.clust <- cutnet(x.g,thres,w)$cluster
  x.sub <- subnetwork(x.g,x.clust)
  x.score <- clustscore(x.g,x.clust)
  x.score[is.na(x.score)] <- .9
  #Loop fclust till converge or subscore lt thres_score
  li <- 1
  while(li<layer){
    if(float_thres){
      if(mean(x.score==.9)==1) {thres_score <- .9} else{
        thres_score <- median(x.score[x.score>.9])
      }
    }
    x.run <- (x.score>=thres_score)
    x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
      subnetwork(x,fc(x))
    }))
    x.sub <- c(x.run_sub,x.sub[!x.run])
    x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
      names(x.clust) <- do.call(c,lapply(x.sub,colnames))
      x.clust <- x.clust[order(as.numeric(names(x.clust)))]
    x.score <- clustscore(x.g,x.clust)
    x.score[is.na(x.score)] <- .9
    print(paste('#loops',li,'#subs',length(x.score)))
    if(length(x.run)==length(x.score)){break}
    li <- li+1
  }
  #Summarise Result
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
    names(x.clust) <- do.call(c,lapply(x.sub,colnames))
    x.clust <- x.clust[order(as.numeric(names(x.clust)))]
  x.score <- clustscore(x.g,x.clust)
  subnets <- subnetwork(x.g,x.clust)
  rlt <- list(subnets=subnets,cluster=x.clust,score=x.score)
  return(rlt)
}
fc <- function(x){
  w<-as.vector(t(x))[t(x)>0]
  x <- graph_from_adjacency_matrix(x>0,mode='undirected')
  fc <- membership(fastgreedy.community(x,weight=w))
  fc[] <- match(fc,unique(fc))
  fc
}
clustscore <- function(x,x.clust){
  sapply(unique(x.clust),function(g){
    xin <- (x[which(x.clust==g),which(x.clust==g),drop=F])
    xout <- (x[which(x.clust==g),which(x.clust!=g),drop=F])
    w_in <- sum(xin)/2#inloop weight
    c_in <- ncol(xin)#inloop count
    w_out <- sum(xout)#outloop weight
    c_out <- sum(colSums(xout)>0)#outloop weight
    (w_in/c_in)/(w_out/c_out)
  })
}
subnetwork <- function(x,x.clust){
  lapply(unique(x.clust),function(i){
    x[x.clust%in%i,x.clust%in%i,drop=F]
  })
}
