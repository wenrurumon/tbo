
rm(list=ls())
setwd('C:/Users/zhu2/Documents/forjr')

library(igraph)
library(dplyr)

#gene in Wnt pathways
load('C:/Users/zhu2/Documents/getpathway/model20170215/rlt_cluster_network.rda')
gene <- rownames(expnet[[32]])

#KEGG reference
data <- read.table('clipboard',header=T)
g <- graph_from_data_frame(data[,1:2])

#random network to data frame
library(pcalg)
randomDAGmat <- function(n,p){
  showAmat(randomDAG(n,p))==2
}
rmat <- randomDAGmat(145,0.002)+0
dimnames(rmat) <- list(gene,gene)
# rmat <- expnet[[32]][1:145,1:145]
rmat.df <- filter(melt(rmat),value!=0)[,1:2]

validate <- function(from,to,ref){
  # from <- paste(rmat.df[1,1])
  # to <- paste(rmat.df[1,2])
  # ref <- g
  ifrom <- which(names(V(ref))==from)
  ito <- which(names(V(ref))==to)
  igraph::shortest_paths(g,V(ref)[ifrom],V(ref)[ito])
}
validate2 <- function(rmat.df,ref){
  rlt <- apply(rmat.df,1,function(x){
    validate(x[1],x[2],ref)
  })
  cbind(rmat.df,paths=rlt)
}
# validate2(rmat.df,g)
validate3 <- function(x,ref){
  rlt <- lapply(1:nrow(x),function(i){
    # print(i)
    validate(x[i,1],x[i,2],ref)$vpath[[1]]
  })
  names(rlt) <- paste(x[,1],x[,2])
  rlt
}
rlt <- validate3(rmat.df,g)

#

x.reference <- data
x.sample <- rmat
g <- graph_from_data_frame(x.reference[,1:2])
rmat.df <- filter(melt(x.sample),value!=0)[,1:2]
rlt <- validate3(rmat.df,g)
rlt
