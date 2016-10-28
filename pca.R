
##########################
# Macro
##########################

library(fda)
library(MASS)
library(GenABEL)
library(flare)
library(corpcor)

myinv<-function(A){
  A_svd<-fast.svd(A)
  if(length(A_svd$d)==1){
    A_inv<-A_svd$v%*%as.matrix(1/A_svd$d)%*%t(A_svd$u)
  }else{
    A_inv<-A_svd$v%*%diag(1/A_svd$d)%*%t(A_svd$u)
  }
  return(A_inv)
}

fourier.expansion<- function(x,pos,nbasis,lambda){
  frange <- c(pos[1], pos[length(pos)])
  rlt=list();
  rlt$fbasis<-create.fourier.basis(frange,nbasis=nbasis)
  rlt$phi = eval.basis(pos,rlt$fbasis) + eval.basis(pos,rlt$fbasis,2)*lambda
  rlt$coef<-myinv(t(rlt$phi)%*%rlt$phi)%*%t(rlt$phi)%*%t(x)
  return(rlt)
}

fpca <- function(x,pos,nbasis,lambda){
	nbasis <- min(nrow(x),nbasis)
	#process data
	x <- snp[,order(pos),drop=F]
	pos <- pos[order(pos)]
	pos <- (pos-min(pos))/(max(pos)-min(pos))
	#fourier expansion
	x.expanded <- fourier.expansion(x,pos,nbasis,lambda)
	fcoef<-scale(t(x.expanded$coef-rowMeans(x.expanded$coef))/sqrt(ncol(x)))
	#PCA
	A.svd <- svd(fcoef)
	prop1 <- (A.svd$d)^2; prop1 <- cumsum(prop1)/sum(prop1)
	r <- which(prop1>0.8)[1]
	d <- A.svd$d-A.svd$d[min(r+1,dim(fcoef))]
	d <- d[d>1e-10]
	prop2 <- d^2; prop2 <- cumsum(prop2)/sum(prop2)
	d <- diag(d,length(d),length(d))
	score1 <-  fcoef %*% A.svd$v
	score2 <- A.svd$u[,1:length(d),drop=F] %*% sqrt(d)
	list(fpca=list(scores=score1,prop=prop1),qfpca=list(scores=score2,prop=prop2))
}

qpca <- function(A,rank=0){
  A <- scale(A)
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}
