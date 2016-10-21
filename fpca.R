
rm(list=ls())

#macro

library(CCA)
library(GenABEL)

cca <- function(X,Y){
  rho <- cc(X,Y)[[1]]
  n <- dim(X)[1]
  p <- dim(X)[2]
  q <- dim(Y)[2]
  m <- n-(3+p+q)/2
  s <- sqrt(((p*q)^2-4)/(p^2+q^2-5))
  df1 <- p*q
  df2 <- m*s - p*q/2 +1
  if(is.nan(s)){df2=0}
  
  if(df2>0){
    lambda <- prod(1-rho^2)
    f_test <- ((1-lambda^(1/s))*df2)/(lambda^(1/s)*df1)
    f_pvalue <- pf(f_test,df1,df2,lower.tail=FALSE)
  } else {
    f_test <- NA; f_pvalue <- NA
  }
  
  chi_test <- -n * sum(log(1-rho^2))
  #chi_test <- -n * (log(prod(1-rho^2)))
  chi_pvalue <- pchisq(chi_test,(p*q),lower.tail=FALSE)
  c(rho=paste(cc(X,Y)[[1]],collapse=' '),ftest=f_test,fpvalue=f_pvalue,chitest=chi_test,chipvalue=chi_pvalue)
}
ccap <- function(x,y){
  if(all(is.na(x))|all(is.na(y))){return(NA)}
  return(as.numeric(cca(x,y)[5]))
}

library(fda)
library(MASS)
ivn <- function(x){sapply(1:ncol(x),function(i) rntransform(x[,i]))}
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
fpca <- function(x,pos,nbasis){
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
    nscores <- which(prop>0.9999)[1]
    scores <- scores[,1:nscores,drop=F]
    prop <- prop[1:nscores]
    return(list(scores=scores,prop=prop))
  }
}
fpca_smoothed <- function(x,pos,nbasis,lambda=0.1){
  if(length(pos)<=3){
    return(list(scores=x,prop=rep(1,length(pos))))
  }else{
    fbasis<-create.fourier.basis(c(0,1),nbasis=nbasis)
    fphi <- eval.basis(pos,fbasis)+eval.basis(pos,fbasis,2)*lambda
    fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(x)
    coef<-t(fcoef-rowMeans(fcoef))/sqrt(nrow(x))
    scores<-coef%*%prcomp(coef)$rotation
    prop<-apply(scores,2,var)
    prop<-cumsum(prop/sum(prop))
    nscores <- which(prop>0.9999)[1]
    scores <- scores[,1:nscores,drop=F]
    prop <- prop[1:nscores]
    return(list(scores=scores,prop=prop))
  }
}
fpca2 <- function(x,pos){
  nbasis <- length(pos)*2-1
  x.fpca <- try(fpca(x,pos,nbasis))
  while(class(x.fpca)=='try-error'){
    nbasis <- nbasis-2
    x.fpca <- try(fpca(x,pos,nbasis))
  }
  r <- which(x.fpca$prop>0.8)[1]
  x.fpca <- x.fpca$scores[,1:which(x.fpca$prop>0.9)[1],drop=F]
  nbasis <- length(pos)*2-1
  x.sfpca <- try(fpca_smoothed(x,pos,nbasis,lambda=0.1))
  while(class(x.sfpca)=='try-error'){
    nbasis <- nbasis - 2
    x.sfpca <- try(fpca_smoothed(x,pos,nbasis,lambda=0.1))
  }
  x.sfpca <- x.sfpca$score[,1:which(x.sfpca$prop>0.9)[1],drop=F]
  x.qfpca <- qpca(ivn(x.fpca),rank=r)$Z
  return(list(raw=x,pos=pos,fpca=x.fpca,sfpca=x.sfpca))
}
pca2 <- function(x){
  x.pca <- qpca(x)
  r <- which(x.pca$prop>0.8)[1]
  x.qpca <- x.pca <- x.pca$X[,1:which(x.pca$prop>0.9)[1],drop=F]
  if(ncol(x.pca)>1){x.qpca <- qpca(ivn(x.pca),rank=r)$X}
  return(list(pca=x.pca,qpca=x.qpca))
}

#data

load("C:/Users/zhu2/Documents/getpathway/gene39761.rda")
load("C:/Users/zhu2/Documents/qtl/methylation/pmethylation_448.rda")
rm(disease,ptwmap,raw_exp,rlt2)

path <- lapply(1:length(expinpath),function(i){
  print(i)
  pathi.mc <- mcinpath[[i]][[1]][,1:which(cumsum(mcinpath[[i]][[2]])>0.9)[1],drop=F]
  pathi.pca <- pca2(expinpath[[i]])
  list(mc=pathi.mc,qpca=pathi.pca[[2]])
})

i <- 0
methy <- lapply(rlt.fpca,function(rlti){
  i <<- i+1; j <<- 0
  lapply(rlti,function(rltij){
    print(paste(i,j<<-j+1))
    fpca2(rltij$raw,rltij$pos)
  })
})

setwd('C:\\Users\\zhu2\\Documents\\qtl')
save(methy,file='methylation2.rda')
save(path,file='pathway2,rda')
