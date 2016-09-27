
#####################################################
# Input
#####################################################

rm(list=ls())
load(file='test_input.rda')
library(flare)
library(MASS)
library(data.table)
library(dplyr)

#Input
y <- input$y
prop <- 0.8
lambda <- 0.4
times <- 10

#####################################################
# Macro
#####################################################

sem1 <- function(y,prop,lambda,times=1){
  #Setup
  Y <- scale(do.call(cbind,lapply(y,function(x){x$score[,1:which(x$prop>=prop)[1],drop=F]})))[,]
  Y.prop <- do.call(c,lapply(y,function(x){diff(c(0,x$prop[1:which(x$prop>=prop)[1]]))}))
  Y.group <- rep(1:length(y),sapply(y,function(x){which(x$prop>=prop)[1]}))
  #building the original network with lasso regression
  if(times==1){
    adj <- lapply(1:ncol(Y),function(i){
      slimi <- slim(X=Y[,-i],Y=Y[,i],lambda=lambda,method='lasso',verbose=FALSE)
      temp <- rep(FALSE,ncol(Y))
      temp[which(slimi$beta!=0)] <- TRUE
      temp
    })
  } else {
    adjs <- lapply(1:100,function(i){
      Y <- Y[sample(1:nrow(Y),nrow(Y)/2),]
      adj <- do.call(rbind,lapply(1:ncol(Y),function(i){
        slimi <- slim(X=Y[,-i],Y=Y[,i],lambda=lambda,method='lasso',verbose=FALSE)
        temp <- rep(FALSE,ncol(Y))
        temp[which(slimi$beta!=0)] <- TRUE
        temp
      }))
    })
    adj <- 0
    for(i in adjs){adj <- i+adj}
    adj <- lapply(1:nrow(adj),function(i){adj[i,]>=0.8})
  }
  #Parameter Estimation
  model <- do.call(rbind,lapply(1:length(adj),function(i){
    yi <- Y[,adj[[i]],drop=F]
    yi.coef <- ginv(t(yi)%*%yi) %*% t(yi) %*% Y[,i]  
    yi.sigma <- (1/nrow(yi))*sum((Y[,i]-yi%*%yi.coef)^2)
    yi.SIGMA <- yi.sigma * ginv(t(yi)%*%yi)
    yi.pvalue <- pchisq(as.vector(yi.coef^2)/diag(yi.SIGMA),df=1,lower.tail=FALSE)
    data.table(y=colnames(Y)[i],x=colnames(yi),
               y.group=Y.group[i],x.group=Y.group[adj[[i]]],
               y.prop=Y.prop[i],x.prop=Y.prop[adj[[i]]],
               coef=as.vector(yi.coef),pvalue=yi.pvalue)
  }))
  #Summarise the model into group
  adj2 <- sapply(adj,function(x){tapply(x,Y.group,sum)>0})
  adj2 <- t(sapply(1:nrow(adj2),function(i){tapply(adj2[i,],Y.group,sum)>0}))
  model2 <- model %>% 
    group_by(y.group,x.group) %>% 
    summarise(y.prop=sum(y.prop),x.prop=sum(x.prop),coef=sum(y.prop*x.prop*coef),pvalue=mean(pvalue))
  #Output
  list(network=do.call(rbind,adj)+0,network2=adj2+0,
       model=model,model2=model2)
}

#####################################################
# Test
#####################################################

system.time(test1 <- sem1(y=input$y,prop=.8,lambda=.3,times=10))
