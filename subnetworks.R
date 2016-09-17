
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
#Quick FastGreedy
fc <- function(x){
  w<-as.vector(t(x))[t(x)>0]
  x <- graph_from_adjacency_matrix(x>0,mode='undirected')
  fc <- membership(fastgreedy.community(x,weight=w))
  fc[] <- match(fc,unique(fc))
  fc
}
#FC with low weight removed
qfc_weight <- function(x.g,q=3/4){
  if(q==0){return(fc(x))}
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  x.bk <- x.g
  q <- quantile(x.g[x.g>0],q)
  x.g[x.g<q]<-0
  print(paste('rm',1-sum(x.g)/sum(x.bk),'weight,',1-sum(x.g>0)/sum(x.bk>0),'count.'))
  x.clust <- fc(x.g)
  #end node send back
  x.endnode <- which(table(x.clust)==1)#Figure out those groups with only 1 cell
  for(endi in x.endnode){
    # print(endi)
    (endi.connect <- rowSums(x.bk[,as.numeric(names(x.clust)[x.clust==endi]),drop=F]))#find the connected node of endi
    (endi.connect_g <- x.clust[endi.connect>0])#find the group of the connected node
    if(length(unique(endi.connect_g))>1){#If multi group connect, maximum weighting to grouping
      endi.connect_g <- tapply(endi.connect[endi.connect>0],endi.connect_g,sum)
      endi.connect_g <- as.numeric(names(which(endi.connect_g==max(endi.connect_g))))
    } else {
      endi.connect_g <- unique(endi.connect_g)
    }
    if(length(endi.connect_g)==1){
      x.clust[x.clust==endi] <- endi.connect_g
    } else {
      endi.connect_g <- tapply(x.clust[x.clust%in%endi.connect_g],x.clust[x.clust%in%endi.connect_g],length)
      endi.connect_g <- as.numeric(names(endi.connect_g[endi.connect_g==max(endi.connect_g)]))
      if(length(endi.connect_g)>1){endi.connect_g <- max(endi.connect_g)}
      x.clust[x.clust==endi] <- endi.connect_g
    }
  }
  return(as.numeric(x.clust))
}
#QFC with method of pagerank
pr <- function(x.g){
  x.g <- graph_from_adjacency_matrix(x.g>0,mode='undirected')
  return(page_rank(x.g)[[1]])
}
qfc <- function(x.g,q=3/4){ 
  if(q==0){return(fc(x))}
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  x.bk <- x.g
  x.pr <- pr(x.bk)
  q <- quantile(x.pr,q)
  x.g[x.pr<q,x.pr<q] <- 0
  print(paste('rm',1-sum(x.g)/sum(x.bk),'weight,',1-sum(x.g>0)/sum(x.bk>0),'count.'))
  x.clust <- fc(x.g)
  #end node send back
  x.endnode <- which(table(x.clust)==1)#Figure out those groups with only 1 cell
  for(endi in x.endnode){
    # print(endi)
    (endi.connect <- rowSums(x.bk[,as.numeric(names(x.clust)[x.clust==endi]),drop=F]))#find the connected node of endi
    (endi.connect_g <- x.clust[endi.connect>0])#find the group of the connected node
    if(length(unique(endi.connect_g))>1){#If multi group connect, maximum weighting to grouping
      endi.connect_g <- tapply(endi.connect[endi.connect>0],endi.connect_g,sum)
      endi.connect_g <- as.numeric(names(which(endi.connect_g==max(endi.connect_g))))
    } else {
      endi.connect_g <- unique(endi.connect_g)
    }
    if(length(endi.connect_g)==1){
      x.clust[x.clust==endi] <- endi.connect_g
    } else {
      endi.connect_g <- tapply(x.clust[x.clust%in%endi.connect_g],x.clust[x.clust%in%endi.connect_g],length)
      endi.connect_g <- as.numeric(names(endi.connect_g[endi.connect_g==max(endi.connect_g)]))
      if(length(endi.connect_g)>1){endi.connect_g <- max(endi.connect_g)}
      x.clust[x.clust==endi] <- endi.connect_g
    }
  }
  return(as.numeric(x.clust))
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
                    edge.arrow.size=1,vertex.size=10,vertex.label.cex=1,edge.width= 1,
                    cuts=0){
  x <- x+t(x)
  if(cuts>0){
    for(i in 1:cuts){
      x <- x[colSums(x>0)>1,colSums(x>0)>1,drop=F]
    }
  }
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

################################################
# Structural
################################################

#Mix clustering
mixclust <- function(x.g,thres=0,w=T,b.rank=2.5,layer=Inf,lambda=0.25,maxrank=0){
  # x.g=x.raw;thres=0;w=T;b.rank=2.5;layer=Inf;lambda=0.5;maxrank=3
  #Setup
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  if(maxrank==0){maxrank <- mat.degree(x.g)}
  if(b.rank==0){b.rank <- maxrank}
  #cut net in the first stage
  x.cutnet <- cutnet(x,thres,w)
  x.sub <- subnetwork(x.g,x.cutnet$cluster)
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
  names(x.clust) <- do.call(c,lapply(x.sub,colnames))
  print(paste('cutnet end at thres = ',thres,', #subnets =',length(x.sub)))
  #Loop fcclust
  li <- 1
  while(li < layer){
    x.sub_d <- sapply(x.sub,mat.degree)  
    x.run <- (x.sub_d >= min((b.rank+li*lambda),maxrank))
    x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
      subnetwork(x,fc(x))
    }))
    x.sub <- c(x.run_sub,x.sub[!x.run])
    x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
    names(x.clust) <- do.call(c,lapply(x.sub,colnames))
    x.clust <- x.clust[order(as.numeric(names(x.clust)))]
    if(length(x.run)==length(x.sub)){
      print(paste('prune end at layer at',li,', #subnets =',length(x.sub)))
      break
    }
    print(paste('#loop = ',li,', #subnets =',length(x.sub)))
    li <- li+1
  }
  #Summarise
  rlt <- list(subnets=x.sub,cluster=x.clust,score=x.sub_d)
  return(rlt)
}
#Generate subnetwork
subnetwork <- function(x,x.clust){
  lapply(unique(x.clust),function(i){
    x[x.clust%in%i,x.clust%in%i,drop=F]
  })
}
#Calculate the degree of a matrix
mat.degree <- function(x){
  x <- (x+t(x))>0
  mean(degree(graph_from_adjacency_matrix(x,mode='undirected')))
}
#Q of the network
kv <- function(v,mat,weight=T){
  if(!weight){
    mat<-(mat>0)
  }
  sum(mat[,v])
}
Avw <- function(v,w,mat,weight=T){
  A<-mat[v,w]
  if(weight){
    return(A)
  }else{
    return(A>0)
  }
}
Q <- function(mat,mem=NULL,weight=T){
  if(!weight) {mat<-(mat>0)}
  m <- sum(mat)
  q <- 0
  if(length(mem)!=ncol(mat)){
    print('Warning: length(mem)!=ncol(mat)), set mem as 1')
    mem <- rep(1,ncol(mat))
  }
  for(v in 1:length(mem)){
    for(w in 1:length(mem)){
      q <- q + (Avw(v,w,mat,weight)-kv(v,mat,weight)*kv(w,mat,weight)/m)*(mem[v]==mem[w])
    }
  }
  return(as.numeric(q/m))
}
#Cluster with subrun option
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
subrun <- function(x.sub,x.run,q=0){
  x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
    subnetwork(x,qfc(x,q))
  }))
  x.sub <- c(x.run_sub,x.sub[!x.run])
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
  names(x.clust) <- do.call(c,lapply(x.sub,colnames))
  return(list(subnets=x.sub,cluster=x.clust))
}
clust1 <- function(x.g,thres.run=2.5,lambda=0.1,layer=Inf,q=1/2,q2=1/2,maxthres=3){
  #Setup
  li <- 0
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  #First stage
  x.clust <- qfc(x.g,q)
  x.sub <- subnetwork(x.g,x.clust)
  x.score <- sapply(x.sub,matrank)
  x.run <- (x.score>thres.run)
  x.len <- length(x.sub)
  print(paste(li,thres.run,sum(x.run),length(x.run)))#layer,thres,#run,#subs
  #Loops
  while(li < layer){
    li <- li+1
    thres.run <- min(thres.run + lambda,maxthres)
    print(paste(li,thres.run,sum(x.run),length(x.run)))#layer,thres,#run,#subs
    x.subrun <- subrun(x.sub,x.run,q2)
    x.sub <- x.subrun$subnets
    x.clust <- x.subrun$cluster
    x.score <- sapply(x.sub,matrank)
    x.run <- (x.score>thres.run)
    if(length(x.run)==x.len){break}
    x.len <- length(x.sub)
  }
  #Output
  x.sub <- x.sub[order(-sapply(x.sub,ncol))]
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
  names(x.clust) <- do.call(c,lapply(x.sub,colnames))
  x.score <- sapply(x.sub,matrank)
  rlt <- list(
    subnets=x.sub,cluster=x.clust,score=x.score
  )
  return(rlt)
}
