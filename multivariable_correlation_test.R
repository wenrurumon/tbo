
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
ivn <- function(x){sapply(1:ncol(x),function(i) rntransform(x[,i]))}
pca <- function(X,ifscale=T){
  if(ifscale) {X <- scale(as.matrix(X))}
  m = nrow(X)
  n = ncol(X)
  X = scale(X)
  Xeigen <- svd(as.matrix(X))
  value <- (Xeigen$d)^2/m
  value <- cumsum(value/sum(value))
  score <- X %*% Xeigen$v
  mat <- Xeigen$v
  list(score=score,prop=value,mat=mat)
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
ccap <- function(x,y){
  if(all(is.na(x))|all(is.na(y))){return(NA)}
  return(as.numeric(cca(x,y)[5]))
}

##

setwd("/home/zhu/qtl/rawdata/1707snp_new")
args = commandArgs(trailingOnly=TRUE)
chr = as.numeric(args[1])
print(chr)
load(paste0('chr',chr,'.448fpca'))
load('/home/zhu/qtl/rawdata/gene39761.rda')
rm(disease,ptwmap,raw_exp,rlt2)

##

path.transform <- lapply(1:length(expinpath),function(i){
  print(i)
  pathi.mc <- mcinpath[[i]][[1]][,1:which(cumsum(mcinpath[[i]][[2]])>0.9)[1],drop=F]
  pathi.pca <- pca(expinpath[[i]])
  r <- which(pathi.pca$prop>0.9)[1]
  pathi.pca <- pathi.pca$score[,1:which(pathi.pca$prop>0.9)[1],drop=F]
  if(ncol(pathi.pca)==1){
    pathi.qpca <- pathi.pca
  } else {
    pathi.qpca <- qpca(ivn(pathi.pca),rank=r,ifscale=F)
    pathi.qpca <- pathi.qpca$X[,1:which(pathi.qpca$prop>0.9)[1],drop=F]
  }
  list(pathi.mc=pathi.mc,pathi.pca=pathi.pca,pathi.qpca=pathi.qpca)
})

genes <- lapply(rlt_fpca,function(xj){
  if(is.null(xj[[3]])|class(xj[[3]])=='try-error'){
    xj1 <- xj2 <- xj3 <- NULL
  } else {
    xj1 <- xj[[3]][[1]][,1:which(xj[[3]][[2]]>0.9)[1],drop=F]#fpca
    xj1[260,] <- (xj1[260,]+xj1[261,])/2; xj1 <- xj1[-261,,drop=F]
    xj2 <- xj[[6]][[1]][,1:which(xj[[6]][[2]]>0.9)[1],drop=F]#sfpca
    xj2[260,] <- (xj2[260,]+xj2[261,])/2; xj2 <- xj2[-261,,drop=F]
    if(ncol(xj1)==1){
      xj3 <- xj1
    } else {
      xj3 <- qpca(ivn(xj1),rank=which(xj[[3]][[2]]>0.9)[1],ifscale=F)
      xj3 <- xj3$X[,1:which(xj3$prop>0.9)[1],drop=F]
    }}
    return(list(fpca=xj1,sfpca=xj2,qfpca=xj3))
    })

qtl <- function(i,j){
	pathi.mc <- path.transform[[i]]$pathi.mc
  pathi.pca <- path.transform[[i]]$pathi.pca
  pathi.qpca <- path.transform[[i]]$pathi.qpca

	xj <- rlt_fpca[[j]]
	
	if(is.null(xj[[3]])|class(xj[[3]])=='try-error'){
		return(as.numeric(c(pathid=i,chr=chr,geneid=j,NA,NA)))
	} else {
    xj1 <- xj[[3]][[1]][,1:which(xj[[3]][[2]]>0.9)[1],drop=F]#fpca
    xj1[260,] <- (xj1[260,]+xj1[261,])/2; xj1 <- xj1[-261,,drop=F]
    xj2 <- xj[[6]][[1]][,1:which(xj[[6]][[2]]>0.9)[1],drop=F]#sfpca
    xj2[260,] <- (xj2[260,]+xj2[261,])/2; xj2 <- xj2[-261,,drop=F]
    if(ncol(xj1)==1){
      xj3 <- xj1
    } else {
      xj3 <- qpca(ivn(xj1),rank=which(xj[[3]][[2]]>0.9)[1],ifscale=F)
      xj3 <- xj3$X[,1:which(xj3$prop>0.9)[1],drop=F]
    }
		return(as.numeric(c(pathid=i,chr=chr,geneid=j,
            cca11 = ccap(pathi.mc,xj1),
            cca12 = ccap(pathi.mc,xj2),
            cca13 = ccap(pathi.mc,xj3),
            cca21 = ccap(pathi.pca,xj1),
            cca22 = ccap(pathi.pca,xj2),
            cca23 = ccap(pathi.pca,xj3),
            cca31 = ccap(pathi.qpca,xj1),
            cca32 = ccap(pathi.qpca,xj2),
            cca33 = ccap(pathi.qpca,xj3)
      )))
	}
}

system.time(rlt <- lapply(1:length(mcinpath),function(i){
	sapply(1:length(rlt_fpca),function(j){print(paste(i,j));qtl(i,j)})
}))

setwd('/home/zhu/qtl/rawdata/rlt20161016')
save(rlt,file=paste0('pathway_snp_chr_new',chr,'.cca_qtl'))


