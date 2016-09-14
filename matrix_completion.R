#Reference
#Generalized Low Rank Models
#Robust Recovery of Subspace Structures by Low-Rank Representation

################################################
# Macro
################################################

#PCA
pca <- function(X){
  X <- scale(as.matrix(X))
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
#quadratically regularized PCA
qpca <- function(A,lambda=0,ifscale=TRUE){
  if(ifscale){
    A <- scale(as.matrix(A))
    A[is.na(A)] <- 0
  }else{
    A <- as.matrix(A)
  }
  A.svd <- svd(A)
  d <- A.svd$d-lambda*A.svd$d[1]
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
}
scale0 <- function(x){
  x <- pnorm(scale(x))
  x[is.na(x)] <- 0.5
  x
}
mc0 <- function(X,lambda=0.2,ifprint=FALSE){
  ### input the initial values
  m <- dim(X)[1]
  n <- dim(X)[2]                                  
  Z <- matrix(rep(0,n*n),nrow=n)
  J <- matrix(rep(0,n*n),nrow=n)
  E <- matrix(rep(0,m*n),nrow=m)
  Y1 <- matrix(rep(0,m*n),nrow=m)
  Y2 <- matrix(rep(0,n*n),nrow=n)
  mu <- 10^(-6) 
  mu_max <- 10^6
  rho <- 1.1
  epsilon <- 10^(-8)
  A <- X
  I <- diag(n)
  ### compute the while loop to run matrix completion
  while ((max(abs(X-A %*% Z -E)) > epsilon) | (max(abs(Z-J)) > epsilon)) {
    if(ifprint){print(paste((max(abs(Z-J))),(max(abs(X-A %*% Z -E)))))}
    ## update the J matrix
    U_J_svd  <-  svd(Z+Y2/mu)
    big_eps <- diag(U_J_svd$d - 1/mu)
    big_eps[big_eps<0] <- 0
    J <- U_J_svd$u %*% big_eps %*% t(U_J_svd$v)
    ## update the Z matrix
    I_A_svd <- svd(I + t(A) %*% A )
    Z <- I_A_svd$v %*% diag(1/(I_A_svd$d)) %*% t(I_A_svd$u) %*% (t(A)%*%(X-E)+J
                                                                 +(t(A) %*% Y1 - Y2)/mu)
    ## update the E matrix
    Q <- X - A %*% Z + Y1/mu
    Q_col_norm <- sqrt(colSums(Q^2))
    Q_mlambda_o_mu <- Q_col_norm - lambda/mu
    Q_mlambda_o_mu[Q_mlambda_o_mu < 0] <- 0 
    Q_c_coef <- matrix(rep((Q_mlambda_o_mu/Q_col_norm),m),nrow=m,byrow=TRUE)
    E <- Q_c_coef * Q
    ## update the multipliers
    Y1 <- Y1 + mu*(X-A%*%Z-E)
    Y2 <- Y2 + mu*(Z-J)
    ## update the parameter mu
    mu <- min(rho*mu,mu_max)
  }
  #rlt <- A
  rlt <- list(Z = A%*%Z, X=A, Y=Z ,prop=cumsum(svd(X-E)$d/sum(svd(X-E)$d)))
  return(rlt)  
}

