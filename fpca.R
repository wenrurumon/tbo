rm(list=ls())

library(fda)
library(MASS)
library(GenABEL)

ivn <- function(x){sapply(1:ncol(x),function(i) rntransform(x[,i]))}
fpca <- function(x,pos,nbasis=37){
	pos <- (pos-min(pos))/(max(pos)-min(pos))
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
qpca <- function(A,rank=0){
  A <- ivn(A)
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

load('/home/zhu/qtl/rawdata/gene39761.rda')
pid <- rownames(mcinpath[[1]][[1]])
setwd('/home/zhu/qtl/rawdata/methylation_raw')
temp2 <- lapply(1:22,function(chr){
	load(paste0('methydata_chr',chr,'.rda'))	
	print(chr)
	i <- 0
	temp <- lapply(methyii,function(x){
		print(i<<-i+1)
		x1 <- list(gene=x$gene[match(pid,rownames(x$gene)),,drop=F],pos=x$pos)
		x2 <- qfpca(x1$gene,x1$pos)
		return(c(x1,x2))
	})
	return(temp)
})
