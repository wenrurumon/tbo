rm(list=ls())
library(stringr)
library(data.table)
library(dplyr)

x <- '\\sum_{Z_2,Z_3,Z_1}\\frac{\\sum_{X}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}{\\sum_{X,Y}P(Y|Z_2,X,,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}\\left(\\sum_{X,Z_3,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)\\right)P(Z_1|Z_2,X)P(Z_3|Z_2)'

getraw <- function(x,remove=F){
  if(remove){
    return(gsub('\\\\','',x))
  }
  sapply(strsplit(x,''),function(x){
    x[x%in%c('(',')','|','{','}')] <- paste0('\\',x[x%in%c('(',')','|','{','}')])
    paste(x,collapse='')
  })
}

getsyn <- function(x){
  x <- (gsub('\\\\','',tolower(x)))
  p <- getraw(unique(unlist(str_extract_all(x,'p\\(.+?\\)'))))
  for(i in 1:length(p)){x <- gsub(p[i],paste0('(P',i,')'),x)}
  p <- getraw(p,T)
  f <- getraw(unique(unlist(str_extract_all(x,'sum_\\{.+?\\}'))))
  for(i in 1:length(f)){x <- gsub(f[i],paste0('@F',i+2,'@'),x)}
  f <- getraw(f,T)
  x <- gsub('right\\)','\\}',gsub('left\\(','\\{',x))
  frac <- lapply(str_extract_all(x,'frac\\{.+?\\}\\{.+?\\}'),function(x){
    x1 <- getraw(str_extract(x,'frac\\{.+?\\}'),T)
    x2 <- getraw(gsub('frac\\{.+?\\}','',x),T)
    c(getraw(x),paste0(gsub('frac','',x1),'\\{@F1@',x2,'\\}'))
  })
  for(i in 1:length(frac)){x <- gsub(frac[[i]][1],frac[[i]][2],x)}
  list(syn=x,p=p,f=c('devide','',f))
}

splitsyn <- function(x){
  x <- strsplit(x,'')[[1]]
  if(sum(cumsum(x=='{') == cumsum(x=='}'))==1){x <- x[-c(1,length(x))]}
  k <- 1
  j <- 0
  rlt <- list()
  for(i in 1:length(x)){
    if(x[i]%in%c('{')){
      if(j==0){
        k <- k+1
        rlt[k][[1]] <- c(rlt[k][[1]],x[i])
      }
      j <- j+1
    } else if(x[i]%in%c('}')){
      j <- j-1
      if(j==0){
        rlt[k][[1]] <- c(rlt[k][[1]],x[i])
        k <- k+1
      }
    } else {
      rlt[k][[1]] <- c(rlt[k][[1]],x[i])
    }}
  rlt <- rlt[!sapply(rlt,is.null)]
  rlt <- sapply(rlt,paste,collapse='')
  fun <- do.call(rbind,lapply(unlist(str_extract_all(rlt[1],'@.+?@')),function(x){
    c(0,nchar(x))+regexpr(x,rlt[1])
  }))
  if(is.null(fun)){
    rlt <- c('@F2@',rlt)
  } else if(min(fun)!=1){
    rlt <- c('@F2@',rlt)
  } else if(nrow(fun)==1){
    fun <- range(fun)
    rlt <- unlist(list(substr(rlt[1],fun[1],fun[2]-1),substr(rlt[1],fun[2],nchar(rlt[1])),(rlt[-1])))
  } else {
    fun <- range(fun[1,])
    rlt <- unlist(list(substr(rlt[1],fun[1],fun[2]-1),substr(rlt[1],fun[2],nchar(rlt[1])),(rlt[-1])))
  }
  rlt <- rlt[sapply(rlt,function(x){x!=''})]
  rlt <- list(fun=rlt[1],cell=rlt[-1],branch=grepl('@',rlt[-1]))
  rlt
} 

splitsyns <- function(x){
  rlt <- splitsyn(x)
  if(sum(rlt$branch)==0){
    return(rlt)
  } else {
    rlt$cell[rlt$branch] <- lapply(rlt$cell[rlt$branch],splitsyns)
    rlt$cell[!rlt$branch] <- lapply(rlt$cell[!rlt$branch],splitsyn)
  }
  return(rlt)
}

getloop <- function(x,x1){
  x.names <- strsplit(names(x),'\\.')
  x <- data.table(do.call(rbind,lapply(x.names,function(x){
    c(x,rep('NA',max(sapply(x.names,length))-length(x)))
  })),n=sapply(x.names,length),x)
  x <- x[!grepl('branch',sapply(x.names,paste,collapse='')),,drop=F]
  x <- data.table(module=sapply(1:nrow(x),function(i){x[[x$n[i]]][i]}),stage=x$n,value=x$x)
  x$value <- sapply(1:nrow(x),function(i){
    if(x$module[i]=='fun'){
      x1$f[as.numeric(gsub('@F|@','',x$value[i]))]
    } else if(x$module[i]=='cell'){
      paste(
        x1$p[as.numeric(gsub('\\(P|\\)','',str_extract_all(x$value[i],'\\(P.+?\\)')[[1]]))],
        collapse='*'
      )
    }
  })
  x
}

###############################################

parseletax <- function(x){
  x1 <- getsyn(x)
  x1$f <- gsub('\\}','\\)',gsub('\\{','\\(',x1$f))
  x2 <- (splitsyns(x1$syn))
  getloop(unlist(x2),x1)
}

##############################################

x <- c('\\sum_{Improved}P(Age|Treatment,Improved)P(Improved|Treatment)'
       ,'\\sum_{Treatment}\\sum_{Treatment1}\\sum_{Improved}P(Age|Treatment,Improved)P(Improved|Treatment)'
       ,'\\frac{\\sum_{X}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}{\\sum_{X,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}\\left(\\sum_{X,Z_3,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)\\right)P(Z_1|Z_2,X)P(Z_3|Z_2)'
       ,'\\sum_{Improved}\\left(\\sum_{Treatment}P(Age|Treatment,Improved)P(Treatment)\\right)P(Improved|Treatment)'
       ,'\\sum_{Z_2,Z_3,Z_1}\\frac{\\sum_{X}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}{\\sum_{X,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}\\left(\\sum_{X,Z_3,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)\\right)P(Z_1|Z_2,X)P(Z_3|Z_2)'
       ,'\\sum_{W,Z}\\left(\\sum_{X}P(Y|W,X,Z)P(X|W)\\right)P(Z|W,X)P(W)'
       ,'\\sum_{Z_2,Z_3,Z_1}\\frac{\\sum_{X}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}{\\sum_{X,Y}P(Y|Z_2,X,,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)}\\left(\\sum_{X,Z_3,Y}P(Y|Z_2,X,Z_3,Z_1)P(Z_3|Z_2,X)P(X|Z_2)P(Z_2)\\right)P(Z_1|Z_2,X)P(Z_3|Z_2)')

lapply(x,parseletax)

