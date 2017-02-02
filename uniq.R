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
sales$week <- sapply(strsplit(sales$week,"/"),function(x){as.numeric(x[1])*10000+as.numeric(x[2])*100+as.numeric(x[3])})
sales <- select(arrange(sales,yr_ssn, cate, city, week),-WK)

code <- paste0(sales$yr_ssn,sales$cate,sales$city)
retention <- function(x,rate=0.75,code=code){
  for(i in 2:length(x)){
    if(code[i]==code[i-1]){
      x[i] <- x[i-1]*rate + x[i]
    }
  }
  return(x)
}
checksales <- function(){
  as.data.frame(m %>% group_by(week) %>% summarise(qty=sum(Qty),amt=sum(Amnt)))
}

#
unique(sales$cate) 
model_cate <- function(cate){
  m <- select(filter(sales,cate==cate),-yr_ssn,-cate)
  m <- mutate(m,pris=Amnt/Qty,Disc=ifelse(Disc<0,0,Disc),Disc_ratio=Disc/(Disc+Amnt),
              pr=retention(FRCN_DE_imp_pr+FRCN_SOLW_imp_pr+jogger_imp_pr+
                             kaws_imp_pr+shanghai_lemaire_imp_pr+shanghai_magic_imp_pr+
                             shanghai_reopen_imp_pr+victoria_imp_pr,
                           rate=0.75,code=paste0(m$week,m$city)),
              digital=retention(digital_pr
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
                                +digital_down,rate=0.75,code=paste0(m$week,m$city)
              ),otv=retention(otv_uld
                            +otv_ht
                            +otv_ut
                            +otv_jeans
                            +otv_down_and_uld_and_wool,rate=0.75,code=paste0(m$week,m$city)
              ),tv=retention(tv_uld
                             +tv_ht
                             +tv_ut
                             +tv_airism
                             +tv_jeans
                             +tv_down_and_uld_and_wool,rate=0.75,code=paste0(m$week,m$city)
              ),search=retention(search_ht
                                 +search_uld_and_down,rate=0.75,code=paste0(m$week,m$city)
              ),ooh=retention(ooh_branding
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
                              +ooh_ut_and_linen,rate=0.75,code=paste0(m$week,m$city)
              ),magazine=retention(magazine_jeans
                                   +magazine_knit
                                   +magazine_uld
                                   +magazine_kids
                                   +magazine_pr
                                   +magazine_linen
                                   +magazine_polo,rate=0.75,code=paste0(m$week,m$city)
              ),
              storecount=LFL_countStr+new_countStr+others_countStr
  )
  m <- filter(m,Qty>0&Amnt>0)
  
  x.lm <- lm(Qty~-1
             +paste(city)+paste(substr(week,5,6))+storecount+avg_temp
             # +pris
             +Disc_ratio
             +pr+ooh+magazine+tv+otv+search
             ,data=m)
  
  print(cate)
  print(cor(tapply(m$Amnt,m$week,sum),tapply(predict(x.lm),m$week,sum)))
  plot.ts(as.numeric(tapply(m$Qty,m$week,sum))); lines(tapply(predict(x.lm),m$week,sum),col=2)
  
  return(list(data=m,model=x.lm))
}

##########################################
# Trail Round
##########################################

test1 <- lapply(unique(sales$cate),model_cate)

