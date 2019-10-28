
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

out <- lapply(1:3,clusti)
for(i in 1:3){
  write.csv(cbind(out[[i]]),paste0(i,'.cluster'))  
}


