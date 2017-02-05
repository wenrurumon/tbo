
# par(mfrow=c(5,4))

###########################
# Load Data and Library
###########################

setwd("C:/Users/zhu2/Documents/mindshare/uniq")
library(sqldf)
library(data.table)
library(dplyr)

rm(list=ls())
load('data.Rdata')

retention <- function(x,rate=0.75,code){
  for(i in 2:length(x)){
    if(code[i]==code[i-1]){
      x[i] <- x[i-1]*rate + x[i]
    }
  }
  return(x)
}
meancenter <- function(x,code){
  grpmean <- tapply(x,code,mean)
  abs(grpmean[match(code,names(grpmean))])
}
getdummy <- function(x){
  rlt <- outer(x,unique(x),'==')+0
  colnames(rlt) <- unique(x)
  return(rlt[,-ncol(rlt)])
}
model <- function(beta,Y,X,betacons){
  beta1 <- ifelse(betacons*beta<0,0,beta)
  beta2 <- optim(beta1,function(b){
    b <- ifelse(b * betacons<0, -b, b)
    sum((Y-b%*%t(X))^2)
  })
  beta2 <-  ifelse(beta2[[1]] * betacons<0, -beta2[[1]], beta2[[1]])
  beta2
}
aggplot <- function(x,l,main=NULL){
  x <- as.numeric(tapply(x,l,sum))
  plot.ts(x,main=main)
}
agglines<- function(x,l,col=2){
  x <- as.numeric(tapply(x,l,sum))
  lines(x,col=col)
}

###########################
# check data
###########################

sales_ssn <- sales %>% group_by(yr_ssn,week) %>% summarise(sum(Qty))
sales_ssn <- cbind(week=filter(sales_ssn,yr_ssn=="FW")[[2]],
                   fw=as.numeric(filter(sales_ssn,yr_ssn=="FW")[[3]]),
                   ss=as.numeric(filter(sales_ssn,yr_ssn=="SS")[[3]]),
                   yr=as.numeric(filter(sales_ssn,yr_ssn=="Yr")[[3]]))
# plot.ts(cbind(as.numeric(substr(sales_ssn[,1],5,6)),sales_ssn[,-1]))
sales$cate <- paste(sales$yr_ssn,sales$cate)

###########################
# Total model
###########################

model_cati <- function(cati){
  cati <- paste(cati)
  print(cati)
  tsales <- filter(sales,cate==cati,Qty>0)
  tcode <- paste0(tsales$week,tsales$city,tsales$cate,tsales$yr_ssn)
  tsales <- mutate(tsales,
                   year=substr(week,1,4),
                   month=substr(week,5,6),
                   aphh=Amnt/hh,qphh=Qty/hh,
                   Amnt=ifelse(Amnt<0,0,Amnt),Qty=ifelse(Qty<0,0,Qty),Disc=ifelse(Disc<0,0,Disc),
                   Disc_Ratio=ifelse(Amnt+Disc<=0,0,Disc/(Amnt+Disc)),
                   pris=ifelse(Qty==0,0,-Amnt/Qty),
                   pr=FRCN_DE_imp_pr+FRCN_SOLW_imp_pr+jogger_imp_pr+
                     kaws_imp_pr+shanghai_lemaire_imp_pr+shanghai_magic_imp_pr+
                     shanghai_reopen_imp_pr+victoria_imp_pr,
                   pr.ret = retention(pr,0.75,tcode),
                   digital=digital_pr
                   +digital_all_fw
                   +digital_ut
                   +digital_all_ss
                   +digital_jeans
                   +digital_kids
                   +digital_branding
                   +digital_sports
                   +digital_festival
                   +digital_polo
                   +digital_jogger_and_jeans
                   +digital_ut_and_linen
                   +digital_airism
                   +digital_jogger
                   +digital_knit
                   +digital_uld
                   +digital_ht
                   +digital_down,
                   digital.ret = retention(digital,rate=0.75,code=tcode
                   ),otv=otv_uld
                   +otv_ht
                   +otv_ut
                   +otv_jeans
                   +otv_down_and_uld_and_wool,
                   otv.ret = retention(otv,0.75,tcode
                   ),tv=tv_uld
                   +tv_ht
                   +tv_ut
                   +tv_airism
                   +tv_jeans
                   +tv_down_and_uld_and_wool,
                   tv.ret = retention(tv,0.75,tcode
                   ),search=search_ht
                   +search_uld_and_down,
                   search.ret = retention(search,0.75,tcode
                   ),ooh=ooh_branding
                   +ooh_flannel
                   +ooh_ht
                   +ooh_jeans
                   +ooh_jogger_and_jeans
                   +ooh_knit
                   +ooh_linen
                   +ooh_sport_and_flannel
                   +ooh_sports
                   +ooh_uld
                   +ooh_ut
                   +ooh_ut_and_linen,
                   ooh.ret = retention(ooh,rate=0.75,code=tcode
                   ),magazine=magazine_jeans
                   +magazine_knit
                   +magazine_uld
                   +magazine_kids
                   +magazine_pr
                   +magazine_linen
                   +magazine_polo,
                   magazine.ret = retention(magazine,rate=0.75,code=tcode
                   ),
                   storecount=LFL_countStr+new_countStr+others_countStr,
                   discphh=Disc/hh)
  
  ##########################
  # Linear Model + Optim1
  ##########################
  
  X.dummy <- cbind(select(tsales,city),paste(tsales$cate,tsales$month,tsales$yr_ssn))
  X.code <- paste(X.dummy$city,X.dummy$cate)
  X.dummy <- do.call(cbind,lapply(X.dummy,getdummy))
  X.base <- select(tsales,storecount,pris)
  X.incr <- select(tsales,y=qphh,
                   Disc_Ratio,
                   digital.ret,pr.ret,ooh.ret,magazine.ret,tv.ret,otv.ret,search.ret)
  hh <- tsales$hh
  X.base <- apply(X.base,2,function(x){
    x <- x/meancenter(x,X.code)
    ifelse(is.na(x)|is.infinite(x),0,x)
  })
  rownames(X.base) <- NULL
  X.ss <- cbind(X.dummy,X.base,X.incr)
  # corrplot(cor(cbind(X.base,X.incr)))
  
  x.lm <- lm(y~.,data=X.ss)
  x.fit <- tapply(predict(x.lm) * hh,tsales$week,sum)
  x.raw <- tapply(tsales$Qty,tsales$week,sum)
  # plot.ts(as.numeric(x.raw)); lines(as.numeric(x.fit),col=2)
  # print(tail(coef(summary(x.lm)),(ncol(X.base)+ncol(X.incr)-1)))
  
  x.coef <- model(beta=coef(x.lm),
                  Y=select(X.ss,y),
                  X=cbind(1,select(X.ss,-y)),
                  betacons=c(rep(0,ncol(X.dummy)+1),rep(1,ncol(X.ss)-ncol(X.dummy)-1)))
  x.fit2 <- tapply(as.matrix(cbind(1,select(X.ss,-y))) %*% cbind(x.coef) * hh,tsales$week,sum)
  # lines(x.fit2,col=4)
  
  ##################################
  # Optim 2
  ##################################
  
  X <- cbind(1,select(X.ss,-y))
  X.driven <- sapply(1:length(x.coef),function(b){
    # print(sum(X[,b]))
    X[,b] * hh
  })
  colnames(X.driven) <- colnames(X)
  X.driven <- X.driven[,-(ncol(X.base)+ncol(X.incr)-1):-1+1+ncol(X.driven)]
  piecons <- c(#0.327387203,0.362720871,
    # NA,NA,
    0.041465459,0.000279447,0.002634099,0.006745688,0.002364053,0.013693191,0.000600579)
  piemodel <- (colSums(X.driven)/sum(tsales$Qty))[-1:-3]
  cons <- 1
  coefcons <- piecons/piemodel*cons+
    x.coef[-1:-(length(x.coef)-length(piecons))]*(1-cons)
  x.coef[-1:-(length(x.coef)-length(piecons))] <- ifelse(is.na(coefcons),x.coef[-1:-(length(x.coef)-length(piecons))],coefcons)
  
  X <- cbind(1,select(X.ss,-y))
  X.driven <- sapply(1:length(x.coef),function(b){
    x.coef[b] * X[,b] * hh
  })
  colnames(X.driven) <- colnames(X)
  X.driven <- X.driven[,colnames(X.driven)%in%names(coefcons)]
  
  X.ss2 <- cbind(X.ss[,1:40]); X.ss2$y <- (X.ss2$y *hh-rowSums(X.driven))/hh
  x.lm2 <- lm(y~.,data=X.ss2)
  # aggplot(tsales$Qty,tsales$week)
  # agglines(predict(x.lm2)*hh + rowSums(X.driven),tsales$week)
  x.coef2 <- model(beta=c(coef(x.lm2),coefcons),
                   Y=select(X.ss,y),
                   X=cbind(1,select(X.ss,-y)),
                   betacons=c(rep(0,ncol(X.dummy)+1),rep(1,ncol(X.ss)-ncol(X.dummy)-1)))
  
  res <- as.matrix(cbind(1,select(X.ss,-y))) %*% x.coef2*hh
  res2 <- (tsales$Qty-res)/hh
  aggplot(tsales$Qty,tsales$week,main=cati);agglines(res,tsales$week)
  X <- cbind(1,select(X.ss,-y))
  X.driven <- sapply(1:length(x.coef2),function(b){
    x.coef2[b] * X[,b] * hh
  })
  colnames(X.driven) <- colnames(X)
  X.driven <- X.driven[,-(ncol(X.base)+ncol(X.incr)-1):-1+1+ncol(X.driven)]
  # print(cbind(new=round(colSums(X.driven)/sum(tsales$Qty),5),cons=round(c(Inf,-Inf,Inf,piecons),5)))
  
  ##################################
  # Optim 2
  ##################################
  rlt <- list(
    coef = cbind(coef=x.coef[names(x.coef)%in%colnames(X.driven)]),
    support = t(apply(tsales[,match(colnames(X.driven),colnames(tsales))],2,function(x){
      tapply(x,substr(tsales$week,1,4),function(x){sum(as.numeric(x))})
    })),
    driven = t(apply(X.driven,2,function(x){
      tapply(x,substr(tsales$week,1,4),function(x){sum(as.numeric(x))})
    })),
    vol = matrix(rep(tapply(tsales$Qty,tsales$year,function(x){sum(as.numeric(x))}),each=10),ncol=length(unique(tsales$year)),dimnames=list(NULL,unique(tsales$year))),
    val = matrix(rep(tapply(tsales$Amnt,tsales$year,function(x){sum(as.numeric(x))}),each=10),ncol=length(unique(tsales$year)),dimnames=list(NULL,unique(tsales$year)))
  )
  for(i in 1:length(rlt)){
    colnames(rlt[[i]]) <- paste(names(rlt)[i],colnames(rlt[[i]]),sep="_")
  }
  rlt <- do.call(cbind,rlt)
  fit=cbind(fit=tapply(res,tsales$week,sum),vol=tapply(tsales$Qty,tsales$week,sum),val=tapply(tsales$Amnt,tsales$week,sum))
  write.csv(rlt,'clipboard')
  list(rlt=cbind(cat=cati,rlt),fit=fit)
  
}

cats <- unique(sales$cate)
test <- lapply(cats,model_cati)
rlt <- do.call(rbind,lapply(test,function(x){
  x[[1]][,match(colnames(test[[1]][[1]]),colnames(x[[1]]))]
}))


