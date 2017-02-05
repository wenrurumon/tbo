
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
  grpmean[match(code,names(grpmean))]
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

###########################
# check data
###########################

sales_ssn <- sales %>% group_by(yr_ssn,week) %>% summarise(sum(Qty))
sales_ssn <- cbind(week=filter(sales_ssn,yr_ssn=="FW")[[2]],
                   fw=as.numeric(filter(sales_ssn,yr_ssn=="FW")[[3]]),
                   ss=as.numeric(filter(sales_ssn,yr_ssn=="SS")[[3]]),
                   yr=as.numeric(filter(sales_ssn,yr_ssn=="Yr")[[3]]))
plot.ts(cbind(as.numeric(substr(sales_ssn[,1],5,6)),sales_ssn[,-1]))

###########################
# SS model
###########################

sssales <- filter(sales,yr_ssn=='SS')
sscode <- paste0(sssales$week,sssales$city,sssales$cate)
ssdata <- mutate(sssales,
                 year=substr(week,1,4),
                 month=substr(week,5,6),
                 aphh=Amnt/hh,qphh=Qty/hh,
                 Amnt=ifelse(Amnt<0,0,Amnt),Qty=ifelse(Qty<0,0,Qty),Disc=ifelse(Disc<0,0,Disc),
                 Disc_Ratio=ifelse(Amnt+Disc<=0,0,Disc/(Amnt+Disc)),
                 pr=FRCN_DE_imp_pr+FRCN_SOLW_imp_pr+jogger_imp_pr+
                   kaws_imp_pr+shanghai_lemaire_imp_pr+shanghai_magic_imp_pr+
                   shanghai_reopen_imp_pr+victoria_imp_pr,
                 pr.ret = retention(pr,0.75,sscode),
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
                 digital.ret = retention(digital,rate=0.75,code=sscode
                 ),otv=otv_uld
                 +otv_ht
                 +otv_ut
                 +otv_jeans
                 +otv_down_and_uld_and_wool,
                 otv.ret = retention(otv,0.75,sscode
                 ),tv=tv_uld
                 +tv_ht
                 +tv_ut
                 +tv_airism
                 +tv_jeans
                 +tv_down_and_uld_and_wool,
                 tv.ret = retention(tv,0.75,sscode
                 ),search=search_ht
                 +search_uld_and_down,
                 search.ret = retention(search,0.75,sscode
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
                 ooh.ret = retention(ooh,rate=0.75,code=sscode
                 ),magazine=magazine_jeans
                 +magazine_knit
                 +magazine_uld
                 +magazine_kids
                 +magazine_pr
                 +magazine_linen
                 +magazine_polo,
                 magazine.ret = retention(magazine,rate=0.75,code=sscode
                 ),
                 storecount=LFL_countStr+new_countStr+others_countStr)
X.dummy <- select(ssdata,city,cate,month)
X.code <- paste(X.dummy$city,X.dummy$cate)
X.dummy <- do.call(cbind,lapply(X.dummy,getdummy))
  
X.base <- select(ssdata,storecount,Disc,trans)
X.incr <- select(ssdata,y=qphh,
                 # Disc_Ratio,
                 pr.ret,ooh.ret,magazine.ret,tv.ret,otv.ret,search.ret)
# hh <- meancenter(X.base$y,X.code)
hh <- ssdata$hh
X.base <- apply(X.base,2,function(x){
  x <- x/meancenter(x,X.code)
  ifelse(is.na(x)|is.infinite(x),0,x)
})
rownames(X.base) <- NULL
X.ss <- cbind(X.dummy,X.base,X.incr)
corrplot(cor(cbind(X.base,X.incr)))
x.lm <- lm(y~.,data=X.ss)
x.fit <- tapply(predict(x.lm) * hh,ssdata$week,sum)
x.raw <- tapply(ssdata$Qty,ssdata$week,sum)
plot.ts(as.numeric(x.raw)); lines(as.numeric(x.fit),col=2)
summary(x.lm)

x.coef <- model(beta=coef(x.lm),
                Y=select(X.ss,y),
                X=cbind(1,select(X.ss,-y)),
                betacons=c(rep(0,ncol(X.dummy)+1),rep(1,ncol(X.ss)-ncol(X.dummy)-1)))
x.fit2 <- tapply(as.matrix(cbind(1,select(X.ss,-y))) %*% cbind(x.coef) * hh,ssdata$week,sum)
lines(x.fit2,col=4)

##################################
# Summary
##################################

X <- cbind(1,select(X.ss,-y))
X.driven <- sapply(1:length(x.coef),function(b){
  x.coef[b] * X[,b] * hh
})
colnames(X.driven) <- colnames(X)
X.driven <- cbind(select(ssdata,yr_ssn,cate,city,week),X.driven)

