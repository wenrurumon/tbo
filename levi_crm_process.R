rm(list=ls())
setwd('/Users/wenrurumon/Documents/levis')
library(data.table)
library(dplyr)
raw <- lapply(dir(pattern='pcmaster_merge'),fread)
for(i in 1:2){
  colnames(raw[[i]]) <- tolower(colnames(raw[[i]]))
}
names(raw) <- dir(pattern='pcmaster_merge')

#CRM

x <- raw[[1]] %>% select(store=storeid,item=pc9,vol=quantity,val=payment,
                         month=month,tagprice=cn_msrp,channel=product_line_cn) %>% mutate(
                           tval=tagprice*vol
                         ) %>% as.data.table()
x$store <- as.numeric(x$store)

ec <- unique(filter(x,grepl('EC',channel,ignore.case=T))$channel)[c(1:2,4:5)]
x <- x %>% mutate(channel=ifelse(channel%in%ec,'EC',"BK"),ifdiscount=(val/vol<tagprice)) %>% as.data.table()
x <- filter(x,store%in%(11048:11050))

s <- function(x){sum(x,na.rm=T)}

write.csv(x,'processed_CRM_data.csv',row.names=F)

out <- x %>% group_by(store,month,channel) %>% summarise(vol=s(vol),val=s(val),tval=s(tval)) %>% arrange(channel,store,month)
write.csv(out,'vol_val_tval_by_store_month_channel.csv',row.names=F)

out <- x %>% group_by(month,store,ifdiscount) %>% 
  summarise(vol=s(vol),val=s(val),tval=s(tval)) %>% mutate(dl=val/tval)
write.csv(out,'sales_by_store_month_ifdiscount.csv',row.names=F)

#Z24
