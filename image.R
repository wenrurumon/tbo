
rm(list=ls())

load("C:/Users/zhu2/Documents/Lung_CT_CHINA/rlt2.rda")
library(oro.nifti)
library(oro.dicom)
library(RNiftyReg)

raw <- rlt2

#First Stage Feature
for(i in 1:dim(rlt2[[1]])[1]){
  print(i)
  rlt2[[1]][,,i] <- image.process(rlt2[[1]][,,i])
}

test2 <- niftyreg.linear(rlt2[[2]],rlt2[[1]])
test3 <- niftyreg.nonlinear(rlt2[[3]],rlt2[[1]])

#######################
#Process Image

# g <- test
# par(mfrow=c(2,2))
# graphics::image(g,col=grey(0:64/64))
g.grad <- function(g,block=1){
  out <- matrix(0,nrow(g),ncol(g))
  for(i in (block+1):(ncol(g)-block)){
    for(j in (block+1):(ncol(g)-block)){
      out[i,j] <- max(abs(g[i,j]-g[-block:block+i,-block:block+j]))
    }
  }
  out
}
# graphics::image(g.grad(g,1),col=grey(0:64/64))
# g_grad <- g.grad(g,1)
# graphics::image(out>quantile(out,0.9),col=grey(0:64/64))
# g_grad_filter <- (out>quantile(out,0.9))

edge.clean <- function(g){
  for(i in 1:ncol(g)){
    j = which(g[,i])[1]
    if(is.na(j)){next}
    while(g[j,i]){
      g[j,i] <- FALSE
      j=j+1
    }
  }
  for(i in ncol(g):1){
    j = which(g[,i])[1]
    if(is.na(j)){next}
    while(g[j,i]){
      g[j,i] <- FALSE
      j=j+1
    }
  }
  g
}
edge.clean2 <- function(g,times=1){
  for(i in 1:times){
    g <- edge.clean(g)
  }
  g
}

# g <- test
# par(mfrow=c(2,2))
# graphics::image(g,col=grey(0:64/64))
# graphics::image(g.grad(g,1),col=grey(0:64/64))
# graphics::image(edge.clean2(g_grad_filter,2),col=grey(0:64/64))
# gf <- ifelse(edge.clean2(g_grad_filter,2),g,0)
# gf[gf==-2000] <- 0
# graphics::image(gf,col=grey(0:64/64))

#######################
#

image.process <- function(g){
  g_grad <- g.grad(g,1)
  g_grad_filter <- (g_grad>quantile(g_grad,0.9))
  gf <- ifelse(edge.clean2(g_grad_filter,2),g,0)
  gf
}

