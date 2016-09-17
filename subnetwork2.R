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
pc <- function(x){
  x.gi <- matrank(x,F)
  sel <- colnames(x) %in% colnames(x.gi)
  x.gi <- x; x[!sel,!sel] <- 0
  x.fc <- fc(x)
  x.clust <- unique(x.fc)[table(x.fc)>1]
  for (g in x.clust){
    g.connect <- x.fc[which(rowSums(x.gi[,x.fc==g,drop=F])>0)]
    g.connect <- g.connect[!g.connect%in%x.clust]
    if(length(g.connect)==0){next}
    x.fc[which(x.fc %in% g.connect)] <- g
  }
  return(match(x.fc,unique(x.fc)))
}

#####################################
# Supprting Macro
#####################################

#Calculation
subnetwork <- function(x,x.clust){
  lapply(unique(x.clust),function(i){
    x[x.clust%in%i,x.clust%in%i,drop=F]
  })
}
subrun <- function(x.sub,x.run){
  x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
    subnetwork(x,pc(x))
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
#Plot
plotnet <- function(x,       
                    edge.arrow.size=.1,vertex.size=3,vertex.label.cex=.1,edge.width= .1,
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

#####################################
# Test
#####################################

x.g <- x.raw
thres <- 3
layer <- Inf

#Setup
dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))

#First step, Rough clustering for the orignal network
x.clust <- rc(x.g)
x.sub <- subnetwork(x.g,x.clust)
x.score <- sapply(x.sub,matrank)
x.run <- (x.score > thres)
x.len <- length(x.run)

#Second Steup, Freddy Clustering for the run cells.

li <- 1
while(sum(x.run)>0){
  print(paste(li,sum(x.run),length(x.run)))
  li <- li+1
  if(li>layer){break}
  x.subrun <- subrun(x.sub,x.run)
  x.sub <- x.subrun$subnets
  x.clust <- x.subrun$cluster
  x.score <- sapply(x.sub,matrank)
  x.run <- (x.score > thres)
  if(length(x.run)==x.len){break}else{x.len <- length(x.run)}
}
  
  

