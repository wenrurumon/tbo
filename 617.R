
rm(list=ls())
setwd('/Users/wenrurumon/Documents/posdoc/617/v2')
library(data.table)
library(dplyr)
library(igraph)

raw <- lapply(dir(pattern='cor'),fread)
plotclust <- function(x,membership=NULL){
  G <- graph_from_adjacency_matrix(x>0)
  if(is.null(membership)){membership=rep(1,ncol(x))}
  plot(create.communities(G, membership), 
       # as.undirected(G), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=.1)
}
clusti <- function(i){
  x <- raw[[i]] %>% as.matrix
  x.score <- -log(x)
  x.score[x.score==Inf] <- max(x.score[x.score!=Inf])*2
  x.net <- (x<(0.01/length(x)))
  diag(x.net) <- 0
  g.net <- graph_from_adjacency_matrix(x.net)
  E(g.net)$weight <- as.vector(x.score)[as.vector(x.net)]
  g.net <- as.undirected(g.net)
  fc <- cluster_fast_greedy(g.net)
  mem <- (membership(fc))
  plotclust(x.net,mem)
  mem
}

clust <- function(x,...) UseMethod('clust')
clust.matrix <- function(x){
  x.score <- -log(x)
  x.score[x.score==Inf] <- max(x.score[x.score!=Inf])*2
  x.net <- (x<(0.01/length(x)))
  diag(x.net) <- 0
  g.net <- graph_from_adjacency_matrix(x.net)
  E(g.net)$weight <- as.vector(x.score)[as.vector(x.net)]
  g.net <- as.undirected(g.net)
  fc <- cluster_fast_greedy(g.net)
  mem <- (membership(fc))
  mem
}
getsubnet <- function(x,membership){
  x <- lapply(unique(membership),function(i){
    x[colnames(x) %in% names(which(membership==i)),colnames(x) %in% names(which(membership==i)),drop=F]
  })
  x
}
clust.list <- function(x){
  x <- lapply(x,clust)
  xlen <- c(0,cumsum(sapply(x,max)))
  xlen <- xlen[-length(xlen)]
  for (i in 1:length(x)){
    x[[i]] <- x[[i]] + xlen[i]
  }
  unlist(x)
}


################################################

x <- raw[[2]] %>% as.matrix
x.net <- (x<(0.01/length(x)))
diag(x.net) <- 0

x1 <- clust(x)
plotclust(x.net,x1[match(colnames(x),names(x1))])
x1 <- getsubnet(x,x1)

x2 <- clust(x1)
plotclust(x.net,x2[match(colnames(x),names(x2))])

#####

x <- raw[[3]] %>% as.matrix
x.net <- (x<(0.01/length(x)))
diag(x.net) <- 0
x1 <- clust(x)
m <- x1[match(colnames(x),names(x1))]
x1 <- getsubnet(x,x1)
for (j in 1:100){
  print(j)
  x2 <- clust(x1)
  m <- cbind(m,x2[match(colnames(x),names(x2))])
  x1 <- getsubnet(x,x2)
}
plot.ts(apply(m,2,function(x){length(unique(x))}))
diff(apply(m,2,function(x){length(unique(x))}))

################################################

out <- list()
out[[1]] <- m[,1:(which.max(diff(apply(m,2,function(x){length(unique(x))})))-1)]
out[[2]] <- m[,1:(which.max(diff(apply(m,2,function(x){length(unique(x))})))-1)]
out[[3]] <- m[,1:(which.max(diff(apply(m,2,function(x){length(unique(x))}))))]

for(i in 1:3){
  write.csv(cbind(out[[i]]),paste0(i,'.cluster'))  
}
