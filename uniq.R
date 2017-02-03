# setwd('C:\\\\Users\\\\admin\\\\Documents\\\\mindshare\\\\uniq\\\\data')
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\mindshare\\Uniqlo')
# setwd('C:\\Users\\zhu2\\Documents\\mindshare\\uniq')

library(sqldf)
library(data.table)
library(dplyr)

# sales <- '
# select a.*, b.* from raw_sales a left join raw_media b on a.week = b.week and a.city = b.city
# '
# sales <- sqldf(sales)
# sales <- sales[,-28:-29]

rm(list=ls())
load('data.Rdata')

# trans2 <- data.frame(sales %>% group_by(city) %>% summarise(mean(trans)))
# sales <- data.frame(sales,hh=trans2[match(sales$city,trans2[,1]),2])
# trans <- read.table('clipboard',header=T)
# trans <- as.data.frame(trans %>%　group_by(CoreCity,WK) %>% summarise(trans=sum(Trans)))
# sales <- cbind(sales,trans=as.numeric(trans[match(paste(sales$city,sales$WK),paste(trans$CoreCity,trans$WK)),3]))
# rm(trans)
# 
# sales$week <- sapply(strsplit(sales$week,"/"),function(x){as.numeric(x[1])*10000+as.numeric(x[2])*100+as.numeric(x[3])})
# sales <- select(arrange(sales,yr_ssn, cate, city, week),-WK)

retention <- function(x,rate=0.75,code=code){
  for(i in 2:length(x)){
    if(code[i]==code[i-1]){
      x[i] <- x[i-1]*rate + x[i]
    }
  }
  return(x)
}
getdummy <- function(x){
  rlt <- outer(x,unique(x),'==')+0
  colnames(rlt) <- unique(x)
  return(rlt)
}
model <- function(beta,Y,X,betacons){
  # beta <- ifelse(is.na(x.coef),0,x.coef)
  # Y <- x.data[,1]
  # X <- x.data[,-1]
  # betacons <- c(rep(0,38),rep(1,8))
  beta1 <- ifelse(betacons*beta<0,0,beta)
  
  beta2 <- optim(beta1,function(b){
    b <- ifelse(b * betacons<0, -b, b)
    sum((Y-b%*%t(X))^2)
  })
  beta2 <-  ifelse(beta2[[1]] * betacons<0, -beta2[[1]], beta2[[1]])
  beta2
}

#
unique(sales$cate) 
# model_cate <- function(cate){
  (cate <- paste(unique(sales$cate))[2])
  m <- mutate(filter(sales[sales$cate==cate,]),
              aphh=Amnt/hh,qphh=Qty/hh,
              Amnt=ifelse(Amnt<0,0,Amnt),Qty=ifelse(Qty<0,0,Qty),Disc=ifelse(Disc<0,0,Disc),
              Disc_Ratio=ifelse(Amnt+Disc<=0,0,Disc/(Amnt+Disc)))
  m.code <- paste0(m$week,m$city)
  
  m <- mutate(m,
              pr=FRCN_DE_imp_pr+FRCN_SOLW_imp_pr+jogger_imp_pr+
                kaws_imp_pr+shanghai_lemaire_imp_pr+shanghai_magic_imp_pr+
                shanghai_reopen_imp_pr+victoria_imp_pr,
              pr.ret = retention(pr,0.75,m.code),
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
              digital.ret = retention(digital,rate=0.75,code=m.code
              ),otv=otv_uld
              +otv_ht
              +otv_ut
              +otv_jeans
              +otv_down_and_uld_and_wool,
              otv.ret = retention(otv,0.75,m.code
              ),tv=tv_uld
              +tv_ht
              +tv_ut
              +tv_airism
              +tv_jeans
              +tv_down_and_uld_and_wool,
              tv.ret = retention(tv,0.75,m.code
              ),search=search_ht
              +search_uld_and_down,
              search.ret = retention(search,0.75,m.code
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
              ooh.ret = retention(ooh,rate=0.75,code=m.code
              ),magazine=magazine_jeans
              +magazine_knit
              +magazine_uld
              +magazine_kids
              +magazine_pr
              +magazine_linen
              +magazine_polo,
              magazine.ret = retention(magazine,rate=0.75,code=m.code
              ),
              storecount=LFL_countStr+new_countStr+others_countStr
  )
  x.data <- as.matrix(cbind(y=m$qphh,
    getdummy(paste(m$city)),getdummy(substr(m$week,5,6)),
    select(m,storecount,Disc_Ratio,pr.ret,ooh.ret,magazine.ret,tv.ret,otv.ret,search.ret)
  ))
  x.lm <- lm(y~-1+.,data=as.data.frame(x.data))
  x.coef <- coef(x.lm)
  coef2 <- model(beta=ifelse(is.na(x.coef),0,x.coef),Y=x.data[,1],X=x.data[,-1],betacons=c(rep(0,38),rep(1,8)))
  pred <- x.data[,-1] %*% coef2 * m$hh
  plot.ts(as.numeric(tapply(m$Qty,m$week,sum))); lines(as.numeric(tapply(pred,m$week,sum)),col=2)
  
  
# }

##########################################
# Trail Round
##########################################

test1 <- lapply(unique(sales$cate)[1],model_cate))
