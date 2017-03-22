
rm(list=ls())
setwd('C:\\Users\\WenluluSens\\Downloads')
library(data.table)
df <- read.csv('20170321ICC_sample100.txt')

fun_ct <- function(icc,x=df,row=1){
  x <- x[row,,F]
  c1 <- with(x,ifelse(is.na(roe1)==FALSE&roe1>=0,(roe1-icc)*be/(1+icc),0));
  c2 <- with(x,ifelse(is.na(roe2)==FALSE&roe2>=0&be1>=0,(roe2-icc)*be1/(1+icc)^2,0));
  c3 <- with(x,ifelse(is.na(roe3)==FALSE&roe3>=0&be2>=0,(roe3-icc)*be2/(1+icc)^3,0));
  c4 <- with(x,ifelse(is.na(roe4)==FALSE&roe4>=0&be3>=0,(roe4-icc)*be3/(1+icc)^4,0));
  c5 <- with(x,ifelse(is.na(roe5)==FALSE&roe5>=0&be4>=0,(roe5-icc)*be4/(1+icc)^5,0));
  c6 <- with(x,ifelse(is.na(roe5)==FALSE&roe5>=0&be4>=0&is.na(lt)==FALSE,(roe5-icc)*be4*(1+lt)/((icc-lt)*(1+icc)^5),0));
  abs(exp(x$mc)-x$be-c1-c2-c3-c4-c5-c6)
};

#dichotomy
dichotomy <- function(xmin, xmax, itn = 100, cost=cost){
  i <- 0
  while(i<itn){
    i <- i+1
    xmed <- (xmin+xmax)/2
    rlti <- sapply(c(xmin,xmax),cost)
    print((rlti))
    if(rlti[1]>rlti[2]){
      xmin <- xmed
    } else {
      xmax <- xmed
    }
    if(xmin==xmax){break}
  }
  return(
    c(x=(xmin+xmax)/2,cost=cost((xmin+xmax)/2),itn=i)
  )
}

#dichotomy tree
dt <- function(xmin,xmax,ntree=5,itn=100,cost=cost){
  tree <- sort(c(xmin,xmax,runif(ntree,xmin,xmax)))
  while(ntree>0){
    rlt.tree <- sapply(1:ntree,function(i){
      dichotomy(tree[i],tree[i+1],itn=itn,cost=cost)
    })
    tree <- sort(rlt.tree[1,])
    ntree <- length(tree)-1
  }
  return(rlt.tree[1:2,1])
}

####################################
#Modeling

model <- sapply(1:nrow(df),function(rowi){
  cost <- function(icc){fun_ct(icc,df,rowi)}
  test <- dt(xmin=-1,xmax=1,ntree=5,itn=100,cost=cost)
  test
})

