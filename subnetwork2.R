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
fc_core <- function(x.g,q=3/4){
  # x.bk <- x.g <- x.raw; q <- 3/4#
  x.bk <- x.g
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  x.g[x.g<quantile(x.g[x.g>0],q)] <- 0
  x.fc <- pc(x.g)
  x.ends <- unique(x.fc)[table(x.fc)==1]
  for (endi in x.ends){
    # print(endi)
    # endi <- x.ends[1]#
    endi.connect <- rowSums(x.bk[,which(x.fc==endi),drop=F])
    endi.connect <- tapply(endi.connect,x.fc,sum)[table(x.fc)>1]
    endi.connect <- endi.connect[order(-endi.connect)][1]
    x.fc[x.fc==endi] <- as.numeric(names(endi.connect))
  }
  return(x.fc)
}
fc_core_q <- function(x.g,lambda=0){
  q <- matrank(x.g)
  q <- (q/2+lambda)/(q/2+1+lambda)
  out <- fc_core(x.g,q)
  out
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
subrun <- function(x.sub,x.run,fun){
  # i <- 0
  x.run_sub <- do.call(c,lapply(x.sub[x.run],function(x){
    # print(i<<-i+1)
    subnetwork(x,fun(x))
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
mat.pagerank <- function(x){
  G <- graph_from_adjacency_matrix(x,weighted=TRUE)
  page.rank(G)$vector
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
# Building
#####################################

clust2 <- function(x.g,thres=3,layer=Inf){
  # x.g <- x.raw; thres=3; layer=Inf
  #Setup
  dimnames(x.g) <- list(1:ncol(x.g),1:ncol(x.g))
  #First step, Rough clustering for the orignal network
  x.clust <- rc(x.g)
  x.sub <- subnetwork(x.g,x.clust)
  x.score <- sapply(x.sub,matrank)
  x.dim <- sapply(x.sub,ncol)
  x.run <- (x.score > thres)|(x.dim > 100)
  x.len <- length(x.run)
  #Second Steup, Freddy Clustering for the run cells.
  li <- 1
  while(sum(x.run)>0){
    print(paste(li,sum(x.run),length(x.run)))
    li <- li+1
    if(li>layer){break}
    x.subrun <- subrun(x.sub,x.run,fc_core)
    x.sub <- x.subrun$subnets
    x.clust <- x.subrun$cluster
    x.score <- sapply(x.sub,matrank)
    x.dim <- sapply(x.sub,ncol)
    x.run <- (x.score > thres)|(x.dim > 100)
    if(length(x.run)==x.len){break}else{x.len <- length(x.run)}
  }
  x.run <- (x.dim>100)&(x.score>thres)
  while(sum(x.run)>0){
    print(paste(li,sum(x.run),length(x.run)))
    li <- li+1
    if(li>layer){break}
    fun <- function(x){fc_core_q(x,lambda=li)}
    x.subrun <- subrun(x.sub,x.run,fc_core_q)
    x.sub <- x.subrun$subnets
    x.clust <- x.subrun$cluster
    x.score <- sapply(x.sub,matrank)
    x.dim <- sapply(x.sub,ncol)
    x.run <- (x.dim>100)&(x.score>thres)
    if(length(x.run)==x.len){break}else{x.len <- length(x.run)}
  }
  x.run <- (x.dim>100)&(x.score>thres)
  lj <- 0
  while(sum(x.run)>0){
    print(paste(li,sum(x.run),length(x.run)))
    lj <- lj+1
    li <- li+1
    if(lj>10){break}
    fun <- function(x){fc_core_q(x,lambda=lj*2)}
    x.subrun <- subrun(x.sub,x.run,fc_core_q)
    x.sub <- x.subrun$subnets
    x.clust <- x.subrun$cluster
    x.score <- sapply(x.sub,matrank)
    x.dim <- sapply(x.sub,ncol)
    x.run <- (x.dim>100)&(x.score>thres)
  }
  #Summarise
  x.sub <- x.sub[order(-sapply(x.sub,ncol))]
  x.clust <- rep(1:length(x.sub),sapply(x.sub,ncol))
  names(x.clust) <- do.call(c,lapply(x.sub,colnames))
  x.score <- sapply(x.sub,matrank)
  x.dim <- sapply(x.sub,ncol)
  rlt <- list(
    subnets=x.sub,cluster=x.clust,score=x.score,dim=x.dim
  )
  return(rlt)
}
  
#####################################
# Test
#####################################

setwd('C:\\Users\\zhu2\\Documents\\dreamer\\subchallenge1\\data')
library(data.table)
library(slam)
library(igraph)
d <- 4
getdata <- function(d){
  x <- fread(dir()[d])
  v1 <- c(x$V1)+1
  v2 <- c(x$V2)+1
  v3 <- c(x$V3)
  v1 <- c(v1,max(v1,v2))
  v2 <- c(v2,max(v1,v2))
  v3 <- c(v3,0)
  x <- slam::simple_triplet_matrix(v1,v2,v3)
  x <- as.matrix(x)
  x.raw <- x <- x+t(x)
  x.raw
}
x.raw <- getdata(4)
sum(x.raw>0);dim(x.raw)
# rlt <- clust2(x.raw)

rlt <- lapply(1:6,function(d){
  print(d)
  return(clust2(raw[[d]]))
})

#####################################
# Output
######################################

rlti <- rlt[[1]]
out <- sapply(1:length(rlti$subnets),function(i){
  subi <- rlti$subnets[[i]]
  prank <- sort(mat.pagerank(subi),decreasing=TRUE)
  out <- paste(as.numeric(names(prank[1:min(100,ncol(subi))]))-1,collapse="\t")
  paste(i,1/(rlti$score[[i]]+1),out,sep='\t')
})
for(i in 1:length(rlt)){
  print(dir()[i])
  out <- sapply(1:length(rlti$subnets),function(i){
    subi <- rlti$subnets[[i]]
    prank <- sort(mat.pagerank(subi),decreasing=TRUE)
    out <- paste(as.numeric(names(prank[1:min(100,ncol(subi))]))-1,collapse="\t")
    paste(i,1/(rlti$score[[i]]+1),out,sep='\t')
  })
  write(out,file=paste0('rlt\\',dir()[i]))
}
  
