
#####################################################
# Input
#####################################################

rm(list=ls())
load(file='test_input.rda')
library(flare)
library(MASS)
library(data.table)
library(dplyr)
library(grplasso)

#Input
y <- input$y
prop <- 0.8
lambda <- 1
times <- 1

#####################################################
# Macro
#####################################################

plotnet <- function(x,mode='undirected'){
  diag(x) <- 0
  plot(graph_from_adjacency_matrix(t(x),mode=mode),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=.1)
}

sem_l1 <- function(y,prop,lambda,times=1){
  #Setup
  Y <- scale(do.call(cbind,lapply(y,function(x){x$score[,1:which(x$prop>=prop)[1],drop=F]})))[,]
  Y.prop <- do.call(c,lapply(y,function(x){diff(c(0,x$prop[1:which(x$prop>=prop)[1]]))}))
  Y.group <- rep(1:length(y),sapply(y,function(x){which(x$prop>=prop)[1]}))
  #Lasso Network
  if(times==1){
    adj <- lapply(1:ncol(Y),function(i){
      slimi <- slim(X=Y[,-i],Y=Y[,i],lambda=lambda,method='lasso',verbose=FALSE)
      temp <- rep(FALSE,ncol(Y))
      temp[-i][which(slimi$beta!=0)] <- TRUE
      temp
    })
  } else {
    adjs <- lapply(1:times,function(i){
      Y <- Y[sample(1:nrow(Y),nrow(Y)/2),]
      adj <- do.call(rbind,lapply(1:ncol(Y),function(i){
        slimi <- slim(X=Y[,-i],Y=Y[,i],lambda=lambda,method='lasso',verbose=FALSE)
        temp <- rep(FALSE,ncol(Y))
        temp[-i][which(slimi$beta!=0)] <- TRUE
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
    if(length(yi)==0){return(NULL)}
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

sem_grplasso <- function(y,prop=0.8,lambda=1){
  #Setup
  Y <- scale(do.call(cbind,lapply(y,function(x){x$score[,1:which(x$prop>=prop)[1],drop=F]})))[,]
  Y.prop <- do.call(c,lapply(y,function(x){diff(c(0,x$prop[1:which(x$prop>=prop)[1]]))}))
  Y.group <- rep(1:length(y),sapply(y,function(x){which(x$prop>=prop)[1]}))
  #Group Lasso Network
  if(times==1){
    adj <- lapply(1:ncol(Y),function(i){
      lambda <- lambdamax(x=cbind(1,Y[,-i]),y=Y[,i], 
                          index=c(NA,Y.group[-i]), 
                          penscale = sqrt, model = LinReg(),
                          center=TRUE,standardized=TRUE) * 0.5^(1/lambda-1)
      fit <- grplasso(x=cbind(1,Y[,-i]),y=Y[,i],
                      index=c(NA,Y.group[-i]),lambda=lambda,model=LinReg(),
                      penscale = sqrt,
                      control = grpl.control(update.hess = "lambda", trace = 0))
      temp <- rep(0,ncol(Y))
      temp[-i] <- coef(fit)[-1]
      temp
    })
  } else {
    adjs <- lapply(1:times,function(i){
      Y <- Y[sample(1:nrow(Y),nrow(Y)/2),]
      adj <- do.call(rbind,lapply(1:ncol(Y),function(i){
        lambda <- lambdamax(x=cbind(1,Y[,-i]),y=Y[,i], 
                            index=c(NA,Y.group[-i]), 
                            penscale = sqrt, model = LinReg(),
                            center=TRUE,standardized=TRUE) * 0.5^(1/lambda-1)
        fit <- grplasso(x=cbind(1,Y[,-i]),y=Y[,i],
                        index=c(NA,Y.group[-i]),lambda=lambda,model=LinReg(),
                        penscale = sqrt,
                        control = grpl.control(update.hess = "lambda", trace = 0))
        temp <- rep(0,ncol(Y))
        temp[-i] <- coef(fit)[-1]
        temp
      }))
    })
    adj <- 0
    for(i in adjs){adj <- (i!=0)+adj}
    adj <- lapply(1:nrow(adj),function(i){adj[i,]>=0.8})
  }
  #Summarise network
  adj2 <- sapply(adj,function(x){tapply(x,Y.group,sum)>0})
  adj2 <- t(sapply(1:nrow(adj2),function(i){tapply(adj2[i,],Y.group,sum)>0}))
  #Output
  list(network=do.call(rbind,adj>0)+0,network2 <- adj2+0)
}

#####################################################
# Test
#####################################################

system.time(test1 <- sem_l1(y=input$y,prop=.8,lambda=.5,times=10))
system.time(test2 <- sem_grplasso(y=input$y,prop=.8,lambda=1,times=10))
