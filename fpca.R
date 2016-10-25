library(fda)
library(MASS)
fpca <- function(x,pos,nbasis=37){
	if(length(pos)<=3){
			return(list(scores=x,prop=rep(1,length(pos))))
		}else{
			fbasis<-create.fourier.basis(c(0,1),nbasis=nbasis)
			fphi <- eval.basis(pos,fbasis)
			fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(x)
			coef<-t(fcoef-rowMeans(fcoef))/sqrt(nrow(x))
			scores<-coef%*%prcomp(coef)$rotation
	    	prop<-apply(scores,2,var)
	    	prop<-cumsum(prop/sum(prop))
	    	return(list(scores=scores,prop=prop))
   		}
}
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
qfpca <- function(x,pos){
	nbasis <- length(pos)*2-1
	x.fpca <- try(fpca(x,pos))
	if(class(x.fpca)=='try-error'){
		nbasis <- nbasis - 2
		if(nbasis < 0){break}
		x.fpca <- try(fpca(x,pos))
	}
	score.fpca <- x.fpca$scores[,1:which(x.fpca$prop>0.9)[1],drop=F]
	r <- which(x.fpca$prop>0.8)[1]
	if(ncol(score.fpca)==1){
		score.qfpca<-score.fpca
	}else{
		score.qfpca <- qpca(score.fpca,rank=r)$Z
	}
	return(list(fpca=score.fpca,qfpca=score.qfpca))
}
