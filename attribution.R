
#reference
#https://cibersort.stanford.edu/

#########################################
# SVM Module
#########################################

library(e1071)
library(parallel)
library(preprocessCore)

CoreAlg <- function(X, y){
  
  res <- function(i){
    if(i==1){nus <- 0.25}
    if(i==2){nus <- 0.5}
    if(i==3){nus <- 0.75}
    model<-svm(X,y,type="nu-regression",kernel="linear",nu=nus,scale=F)
    model
  }
  
  svn_itor <- 3
  out <- lapply(1:3,res) # if mclapply doesnt work
  
  nusvm <- rep(0,svn_itor)
  corrv <- rep(0,svn_itor)
  
  #do cibersort
  t <- 1
  while(t <= svn_itor) {
    weights = t(out[[t]]$coefs) %*% out[[t]]$SV
    weights[which(weights<0)]<-0
    w<-weights/sum(weights)
    u <- sweep(X,MARGIN=2,w,'*')
    k <- apply(u, 1, sum)
    nusvm[t] <- sqrt((mean((k - y)^2)))
    corrv[t] <- cor(k, y)
    t <- t + 1
  }
  
  #pick best model
  rmses <- nusvm
  mn <- which.min(rmses)
  model <- out[[mn]]
  
  #get and normalize coefficients
  q <- t(model$coefs) %*% model$SV
  q[which(q<0)]<-0
  w <- (q/sum(q))
  
  mix_rmse <- rmses[mn]
  mix_r <- corrv[mn]
  
  newList <- list("w" = w, "mix_rmse" = mix_rmse, "mix_r" = mix_r)
  
}

#########################################
# Deconvolution
#########################################

deconv <- function(x,y,QN=TRUE){
  #y is the matrix of sample and result, x is the matrix of x and action
  #Input Setup
  rawX <- X <- data.matrix(x)
  rawY <- Y <- data.matrix(y)
  #Process Y Matrix
  Y <- scale(Y)
  if(max(Y) < 50) {Y <- 2^Y}
  dnmY <- dimnames(Y)
  if(QN == TRUE){Y <- normalize.quantiles(Y);dimnames(Y) <- dnmY}
  #Process X matrix
  X <- scale(X)
  #Run SVM
  eP <- sapply(1:ncol(Y),function(i){CoreAlg(X,Y[,i])$w})
  dimnames(eP) <- list(colnames(x),colnames(y))
  eY <- rawX %*% eP
  rsquare <- sapply(1:ncol(Y),function(i){summary(lm(rawY[,i]~eY[,i]-1))$r.square})
  names(rsquare) <- colnames(Y)
  return(
    list(coef = rbind(eP),fit=eY,rsquare=rsquare)
  )
}

#########################################
# Test
#########################################

x <- matrix(0,nrow=100,ncol=5)
rownames(x) <- paste0('sample',1:100)
colnames(x) <- paste0('action',1:5)
x[sample(1:length(x),length(x)/2)] <- 1

p <- matrix(rnorm(15,mean=0.5),nrow=5,ncol=3)
rownames(p) <- paste0('action',1:5)
colnames(p) <- paste0('rlt',1:3)
p[p<0] <- 0
p <- t(t(p)/colSums(p))
y <- x %*% p

deconv(x,y)
deconv(x,y>0.5)
deconv(scale(x),qpca(y>0.5,0.2,TRUE)$Z)
