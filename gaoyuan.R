

############################
# Def Fucntion
############################

rm(list=ls())
library(data.table)
library(dplyr)
library(fda)
library(MASS)
# library(GenABEL)
library(flare)
library(corpcor)

p_ginv_sq <- function(X,p){
  X.eigen = eigen(X);
  X.rank = sum(X.eigen$values>1e-8);
  X.value = X.eigen$values[1:X.rank]^(-1*p);
  if (length(X.value)==1){
    D = as.matrix(X.value);
  }else{
    D = diag(X.value);
  }
  rlt = X.eigen$vectors[,1:X.rank] %*% D %*% t(X.eigen$vectors[,1:X.rank]);
  return(rlt);
}
mrank <- function(X){
  X.svd = svd(X);
  X.rank = sum(X.svd$d>1e-6);
  return(X.rank);
}
mrank_sq <- function(X){
  X.eigen = eigen(X);
  X.rank = sum(Re(X.eigen$values)>1e-6);
  return(X.rank);
}
CCA_chisq_test <- function(rho,n,p,q){
  tstat = -1*n*sum(log(1-rho^2));
  p_value = pchisq(tstat,(p*q),lower.tail=FALSE);
  return(p_value);          
}
cca <- function(A,B){
  n = nrow(A);
  p = mrank(A);
  q = mrank(B);
  if (p <= q){
    X = A;
    Y = B;
  }else{
    X = B;
    Y = A;
  }
  R = p_ginv_sq(cov(X),0.5) %*% cov(X,Y) %*% p_ginv_sq(cov(Y),1) %*% cov(Y,X) %*% p_ginv_sq(cov(X),0.5);
  k = mrank_sq(R);
  d = Re(eigen(R)$values);
  rho = d[1:k]^(0.5);
  rho[rho >= 0.9999]=0.9;
  chisq_p = CCA_chisq_test(rho,n,p,q);
  return(c("chisq_p"=chisq_p,"df"=p*q));
}
qqplot <- function(p_value){
  n = length(p_value);
  exp = -log10((c(1:n)-0.5)/n);
  rgen = -log10(sort(p_value));
  plot(exp,rgen,xlab="-log10(Expect)",ylab="-log10(Real)");
  abline(0,1,col="red")
}
ccap <- function(l1,l2){
  rlt <- sapply(l2,function(x2){
    sapply(l1,function(x1){
      cca(x1,x2)[[1]]
    })
  })
  dimnames(rlt) <- list(names(l1),sapply(l2,function(x){colnames(x)[1]}))
  rlt
}

############################
# SNP
############################

setwd('/Users/wenrurumon/Documents/gaoyuan')
qpca <- function(A,rank=0,ifscale=TRUE){
  if(ifscale){A <- scale(as.matrix(A))[,]}
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}
qpca2 <- function(x,p=0.99){
  A <- qpca(x)
  A <- qpca(x,rank=which(A$prop>p)[1])$X
  A
}
load('pheno.rda')
pheno <- pheno[1:4]
phenos <- lapply(pheno,function(x){do.call(cbind,x)})
phenolist <- names(which(table(unlist(lapply(phenos,colnames)))==4))
phenos <- lapply(phenos,function(x){
  x[,match(phenolist,colnames(x))]
})
phenos <- lapply(1:33,function(i){
  scale(cbind(phenos$pheno1[,i],phenos$pheno2[,i],phenos$pheno3[,i],phenos$pheno4[,i]))
})
names(phenos) <- phenolist
phnoes <- phenos[-7]
phenos$LLS <- phenos$headache+phenos$dizziness+phenos$fatigue+phenos$difficulty_sleep+phenos$GI_symptoms

#################

library(igraph)
plotnet <- function(x,mode='undirected'){
  diag(x) <- 0
  plot(graph_from_adjacency_matrix(t(x),mode=mode),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=1)
}
fc <- function(x){
  w<-as.vector(t(x))[t(x)>0]
  x <- graph_from_adjacency_matrix(x>0,mode='undirected')
  fc <- membership(fastgreedy.community(x,weight=w))
  fc[] <- match(fc,unique(fc))
  fc
}
fc2 <- function(x){
  x.mat <- (x<0.01/length(x))+0
  diag(x.mat) <- 0
  x.score <- -log(x)
  # x.score <- log(2^x)
  x.score[x.mat==0] <- 0
  x.score[x.score==Inf] <- max(x.score[x.score!=Inf]*2)
  x.score <- x.score/max(x.score)
  x.g <- graph_from_adjacency_matrix(x.mat,mode='directed')
  E(x.g)$weight <- as.vector(x.score)[x.mat>0]
  x.g <- as.undirected(x.g)
  # plotclust(x.mat,rlt <- fastgreedy.community(x.g)$membership,main=main)
  list(network = x.mat,
       cluster = fastgreedy.community(x.g)$membership)
}
plotclust <- function(x,membership=NULL,main=NULL){
  G <- graph_from_adjacency_matrix(x>0,mode='undirected')
  if(is.null(membership)){membership=rep(1,ncol(x))}
  plot(create.communities(G, membership), 
       # as.undirected(G), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=.3,
       vertex.label.cex=1,
       edge.width=.1,
       main=main)
}
pheno_cluster <- ccap(phenos,phenos)
dimnames(pheno_cluster) <- list(names(phenos),names(phenos))

###################

pc <- function(p,main=NULL){
  p.cca <- ccap(p,p)
  dimnames(p.cca) <- list(names(p),names(p))
  p.clust <- fc2(p.cca)
  G <- graph_from_adjacency_matrix(p.clust$network,mode='undirected')
  plot(create.communities(G, p.clust$cluster), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=.3,
       vertex.label.cex=1,
       edge.width=.1,
       main=main)
  G.legend <- tapply(names(V(G)),p.clust$cluster,function(x){paste(x,collapse=', ')})
  data.table(main=main,cluster=names(G.legend),nodes=paste(G.legend))
}
tmp <- pc(lapply(phenos[-7],function(x){x[,1,drop=F]}),main='P1')
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]}),main='P2'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,3,drop=F]}),main='P3'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]}),main='P4'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]-x[,1,drop=F]}),main='gap14'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]-x[,2,drop=F]}),main='gap24'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]-x[,3,drop=F]}),main='gap34'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,1,drop=F]-x[,3,drop=F]}),main='gap13'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]-x[,3,drop=F]}),main='gap23'))
tmp <- rbind(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]-x[,1,drop=F]}),main='gap12'))
tmp <- rbind(tmp,pc(phenos[-7],main='full'))

#########################################################
#########################################################

pc <- function(p,main=NULL){
  p.cca <- ccap(p,p)
  dimnames(p.cca) <- list(names(p),names(p))
  p.clust <- fc2(p.cca)
  G <- graph_from_adjacency_matrix(p.clust$network,mode='undirected')
  plot(create.communities(G, p.clust$cluster), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=.3,
       vertex.label.cex=1,
       edge.width=.1,
       main=main)
  p.net <- p.clust$network; p.net[,] <- 0
  p.clust <- p.clust$cluster
  for(i in unique(p.clust)){
    p.net[p.clust==i,p.clust==i] <- 1
  }
  p.net
}
pc2 <- function(p,main=NULL){
  p.cca <- ccap(p,p)
  dimnames(p.cca) <- list(names(p),names(p))
  rlt.clust <- cutree(hclust(as.dist(p.cca)),k=9)
  rlt.km <- kmeans(p.cca,centers=9)$cluster
  p1 <- p2 <- p.cca; p1[,] <- p2[,] <- 0
  p.clust <- fc2(p.cca)
  G <- graph_from_adjacency_matrix(p.clust$network,mode='undirected')
  # plot(create.communities(G, p.clust$cluster), 
  #      as.directed(G),
  #      layout=layout.kamada.kawai(as.undirected(G)),
  #      edge.arrow.size=.1,
  #      vertex.size=.3,
  #      vertex.label.cex=1,
  #      edge.width=.1,
  #      main=main)
  p.net <- p.clust$network; p.net[,] <- 0
  p.clust <- p.clust$cluster
  for(i in 1:9){
    p1[rlt.clust==i,rlt.clust==i] <- 1
    p2[rlt.km==i,rlt.km==i] <- 1
  }
  for(i in unique(p.clust)){
    p.net[p.clust==i,p.clust==i] <- 1
  }
  list(p1,p2,p.net)
}

fnet <- lapply(1,function(i){
  print(i)
  phenoi <-phenos[-7]
  pc2(phenoi)
}) 

simu600 <- lapply(1:1000,function(i){
  print(i)
  set.seed(i)
  seli <- sample(1:822,600)
  phenoi <- lapply(phenos[-7],function(x){x[seli,]})
  pc2(phenoi)
}) 
vali600 <- lapply(simu600,function(x){
  list(
    table(as.vector(x[[1]]),as.vector(fnet[[1]][[1]])),
    table(as.vector(x[[2]]),as.vector(fnet[[1]][[2]])),
    table(as.vector(x[[3]]),as.vector(fnet[[1]][[3]]))
  )
})
simu400 <- lapply(1:1000,function(i){
  print(i)
  set.seed(i)
  seli <- sample(1:822,400)
  phenoi <- lapply(phenos[-7],function(x){x[seli,]})
  pc2(phenoi)
}) 
vali400 <- lapply(simu400,function(x){
  list(
    table(as.vector(x[[1]]),as.vector(fnet[[1]][[1]])),
    table(as.vector(x[[2]]),as.vector(fnet[[1]][[2]])),
    table(as.vector(x[[3]]),as.vector(fnet[[1]][[3]]))
  )
})
simu200 <- lapply(1:1000,function(i){
  print(i)
  set.seed(i)
  seli <- sample(1:822,200)
  phenoi <- lapply(phenos[-7],function(x){x[seli,]})
  pc2(phenoi)
}) 
vali200 <- lapply(simu200,function(x){
  list(
    table(as.vector(x[[1]]),as.vector(fnet[[1]][[1]])),
    table(as.vector(x[[2]]),as.vector(fnet[[1]][[2]])),
    table(as.vector(x[[3]]),as.vector(fnet[[1]][[3]]))
  )
})

tmpi <- 0
for(i in simu600){tmpi <- tmpi + i[[3]]}
write.csv(tmpi/1000,'simu600.csv')
tmpi <- 0
for(i in simu400){tmpi <- tmpi + i[[3]]}
write.csv(tmpi/1000,'simu400.csv')
tmpi <- 0
for(i in simu200){tmpi <- tmpi + i[[3]]}
write.csv(tmpi/1000,'simu200.csv')

apply(sapply(vali600,function(x){c(x[[2]][4],x[[3]][4])})/155,1,mean)
apply(sapply(vali600,function(x){c(x[[2]][4],x[[3]][4])})/155,1,var)
apply(sapply(vali400,function(x){c(x[[2]][4],x[[3]][4])})/155,1,mean)
apply(sapply(vali400,function(x){c(x[[2]][4],x[[3]][4])})/155,1,var)

#########################################################

pc <- function(p,main=NULL){
  p.cca <- ccap(p,p)
  dimnames(p.cca) <- list(names(p),names(p))
  p.clust <- fc2(p.cca)
  # p.clust$network <- (lower.tri(p.clust$network) * p.clust$network)
  p.net <- p.clust$network; p.net[,] <- 0
  for (i in unique(p.clust$cluster)){
    p.net[p.clust$cluster==i,p.clust$cluster==i] <- 1
  }
  p.net[lower.tri(p.net)] <- 0
  diag(p.net) <- 0
  G <- graph_from_adjacency_matrix(p.net,mode='directed')
  list(cbind(main=main,igraph::as_data_frame(G)))
}
tmp <- pc(phenos[-7],main='full')
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,1,drop=F]}),main='P1'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]-x[,1,drop=F]}),main='gap12'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]}),main='P2'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,2,drop=F]-x[,3,drop=F]}),main='gap23'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,3,drop=F]}),main='P3'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]-x[,3,drop=F]}),main='gap34'))
tmp <- c(tmp,pc(lapply(phenos[-7],function(x){x[,4,drop=F]}),main='P4'))

names(tmp) <- sapply(tmp,function(x){paste(x$main)[1]})
fmap <- lapply(tmp,function(x){paste(x$from,x$to)})
fmap <- sapply(fmap,function(x) unique(unlist(fmap))%in%x )+0
fmap <- data.frame(edge=unique(unlist(lapply(tmp,function(x){paste(x$from,x$to)}))),fmap)
fmap <- data.frame(do.call(rbind,strsplit(paste(fmap$edge),' ')),fmap[,-1])
colnames(fmap)[1:2] <- c('from','to')

fmap <- merge(
  fmap,
  data.table(from=names(phenos[-7]),cluster=fc2(ccap(phenos[-7],phenos[-7]))$cluster),
  by='from'
) %>% arrange(cluster)# %>% filter(full==1)
write.csv(fmap,'fmap.csv')

#########################################################
#########################################################

pc <- function(p,main=NULL){
  pheno_cluster <- ccap(p,p)
  dimnames(pheno_cluster) <- list(names(p),names(p))
  g <- pheno_cluster
  g[g>(0.01/length(pheno_cluster))] <- 1
  g <- 1-g
  diag(g) <- 0
  g1 <- fc(g)
  g2 <- lapply(unique(g1),function(i){
    sel <- colnames(g)%in%names(which(g1==i))
    f <- fc(g[sel,sel,drop=F])
    f
  })
  for (i in 1:length(g2)){
    g2[[i]] <- g2[[i]] * 10^(i-1)
  }
  g2 <- unlist(g2)
  g2[] <- match(g2,unique(g2))
  g <- g>0
  g <- apply(g,2,function(x){
    names(which(x))
  })
  tmp <- matrix(0,0,3)
  colnames(tmp) <- c('source','target','value')
  for(i in 1:length(g)){
    tmp <- rbind(tmp,cbind(names(g)[i],names(g)[i],TRUE))
    if(length(g[[i]])>0){tmp <- rbind(tmp,cbind(names(g)[i],g[[i]],TRUE))}
  }
  # return(tmp)
  plink3 <- as.data.frame(tmp)
  pnode2 <- data.frame(name=names(g2),group=g2,size=1)
  return(apply(pnode2,2,paste))
  plink3$source <- match(plink3$source,pnode2$name)-1
  plink3$target <- match(plink3$target,pnode2$name)-1
  forceNetwork(Links = plink3, Nodes = pnode2, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               Nodesize = "size",linkColour = "#999",
               radiusCalculation = "Math.sqrt(d.nodesize)+6",
               Group = "group", opacity =10,charge=-5, legend = F
               ,zoom=T,opacityNoHove=100) 
}

p <- phenos
test <- rbind(
  cbind('full',pc(p)),
  cbind('p1',pc(lapply(p,function(x){x[,1,drop=F]}))),
  cbind('p2',pc(lapply(p,function(x){x[,2,drop=F]}))),
  cbind('p3',pc(lapply(p,function(x){x[,3,drop=F]}))),
  cbind('p4',pc(lapply(p,function(x){x[,4,drop=F]}))),
  cbind('gap12',pc(lapply(p,function(x){x[,1,drop=F]-x[,2,drop=F]}))),
  cbind('gap13',pc(lapply(p,function(x){x[,1,drop=F]-x[,3,drop=F]}))),
  cbind('gap14',pc(lapply(p,function(x){x[,1,drop=F]-x[,4,drop=F]}))),
  cbind('gap23',pc(lapply(p,function(x){x[,2,drop=F]-x[,3,drop=F]}))),
  cbind('gap24',pc(lapply(p,function(x){x[,2,drop=F]-x[,4,drop=F]}))),
  cbind('gap34',pc(lapply(p,function(x){x[,3,drop=F]-x[,4,drop=F]}))))

#############################################

#

p <- phenos[-7]
X.pca <- scale(do.call(cbind,lapply(p,rowMeans)))
X.pca <- kmeans(t(X.pca),9)$cluster
X.pca <- do.call(cbind,lapply(unique(X.pca),function(i){
  rowMeans(scale(do.call(cbind,p[X.pca==i])))
}))

pc2 <- function(p,main=NULL){
  p.cca <- ccap(p,p)
  dimnames(p.cca) <- list(names(p),names(p))
  rlt.clust <- cutree(hclust(as.dist(p.cca)),k=9)
  rlt.km <- kmeans(p.cca,centers=9)$cluster
  p1 <- p2 <- p.cca; p1[,] <- p2[,] <- 0
  p.clust <- fc2(p.cca)
  p.clust$cluster
}
X.pc2 <- pc2(p)
X.pc2 <- lapply(unique(X.pc2),function(i){
  qpca(scale(do.call(cbind,p[X.pc2==i])),rank=1)$X
})
X.pc2 <- do.call(cbind,X.pc2)

Y <- do.call(cbind,p)
model.pca <- lm(Y[1:500,]~X.pca[1:500,])
cbind(1,X.pca[-1:-500,]) %*% (coef(model.pca))
model.pca2 <- lm(Y[1:500,]~X.pc2[1:500,])

mean(abs(predict(model.pca,data)-Y))
mean(abs(predict(model.pca2)-Y))

mean(abs(predict(model.pca)-Y)^2)
mean(abs(predict(model.pca2)-Y)^2)


