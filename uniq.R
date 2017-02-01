# setwd('C:\\\\Users\\\\admin\\\\Documents\\\\mindshare\\\\uniq\\\\data')
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\mindshare\\Uniqlo')

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
retention <- function(x,rate=0.75){
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
m <- select(filter(sales,cate=='core_down'),-yr_ssn,-cate)
m <- mutate(m,pris=Amnt/Qty)
m <- filter(m,Qty>0)

x.lm <- lm(Amnt~-1
             +paste(city)+paste(substr(week,5,6))
             +pris+Disc
           ,data=m)
summary(x.lm)
