#Functional Variable Remodeling

library(fda)
library(flare)

functional_transform <- function(X){
  #calculate the distance for each variables
  X.dist <- dist(t(X))
  pos <- hclust(X.dist)$order
  X <- X[,pos]
  X.dist <- as.matrix(X.dist)[pos,pos]
  pos <- c(0,cumsum(diag(X.dist[-1,-length(pos)])))
  pos <- scale0(pos,T,T)
  #set fourier basis
  fbasis<-create.fourier.basis(c(0,1),nbasis=length(pos)*2-1)
  fphi <- eval.basis(pos,fbasis)
  fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(X)
  rlt <- t(fcoef-rowMeans(fcoef))/sqrt(nrow(x))
  return(rlt)
}
