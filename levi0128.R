
rm(list=ls())
setwd('/Users/wenrurumon/Documents/levis')
library(data.table)
library(dplyr)
library(sqldf)

#CRM data
crm <- fread('CRM_pcmaster_merge_0118.raw')
colnames(crm) <- gsub('-','',tolower(colnames(crm)))
rawcrm <- crm
crm <- crm %>% select(month=month,week=week,date=date,time=ordertime,
                      oid=orderid,store=storeid,item=pc9,
                      vol=quantity,val=payment,tprice=cn_msrp,
                      mem=membercard,promo=name,category,fit,gender,
                      channel=product_line_cn,subcat=subcategory,
                      province,region,tiercity,fo=fr_oo) %>% mutate(
                        tval = tprice * vol
                      )
crm <- mutate(crm,mem=(mem!='')+0)
crm <- mutate(crm,promo=(promo!='')+0)
crm <- filter(crm,(!is.na(vol))&month>=201712)
crm <- unique(crm)
crm <- mutate(crm,
              fo=ifelse(store%in%11048:11050,"EC",fo),
              region=ifelse(store%in%11048:11050,'EC',region),
              province=ifelse(store%in%11048:11050,'EC',province),
              tiercity=ifelse(store%in%11048:11050,'EC',tiercity))

ec <- crm %>% filter(store%in%11048:11050)
ol <- crm %>% filter(!(store%in%11048:11050))
  
#Z24 data
z24 <- fread("z24_merge_0118.raw" )
z24 <- select(z24,month,week,date,time,oid,store,item=pc9,
              vol=vol,val=val,tprice=cn_msrp,category,fit,gender,
              channel=productline,subcat=subcategory,
              province,region,tiercity,fo=fr_oo)
z24 <- mutate(z24,oid = paste(date,time,store,oid)) %>% mutate(
  tval = tprice* vol
) %>% filter(date!=20181130)
z24$store <- as.numeric(z24$store)

test <- lapply(unique(z24$month),function(i){
  unique(filter(z24,month==i)$store)
})

#BZ data
bz <- fread("bz_ec0120.raw",encoding='UTF-8')
bz <- select(bz,store,month,week,date,time=ordertime,oid=orderid,
             item,vol,val,tprice=tag_price) %>% mutate(
               tval = tprice * vol
             )
bz <- unique(bz) %>% filter(date!=20181130)

#######

# 交易相关：
# 2018年线上交易记录条数953643/758102、直营门店交易记录条数、加盟门店交易记录条数
# 客户相关：客户/会员表的记录条数，××%的人有年龄信息，××%的人有性比信息，××%的人有城市信息。××%的人有身高/体重信息。附件数据字典中的customer表和member表有什么区别和联系，customer表记录的是否也只是会员信息，而非普通用户信息？
# 商品相关：SKU的总数目大概是多少？

# 2018年线上交易记录条数
filter(ec,month>201801) %>% nrow()
filter(bz,month>201801) %>% nrow()
# 直营门店交易记录条数、加盟门店交易记录条数
ol %>% group_by(fo) %>% summarise(n())
z24 %>% group_by(fo) %>% summarise(n())
# SKU的总数目大概是多少？
length(unique(crm$item))
length(unique(c(bz$item,z24$item)))
# 客户/会员表的记录条数，
sum(crm$mem)



#######################################################
#######################################################

s <- function(x){sum(x,na.rm=T)}
x1 <- ol %>% group_by(date) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))
x2 <- z24 %>% group_by(date) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-date)
x3 <- ec %>% group_by(date) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-date)
x4 <- bz %>% group_by(date) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))%>% select(-date)

colnames(x1)[-1] <- paste0(colnames(x1)[-1],'_daily_crmol')
colnames(x2) <- paste0(colnames(x2),'_daily_z24')
colnames(x3) <- paste0(colnames(x3),'_daily_crmec')
colnames(x4) <- paste0(colnames(x4),'_daily_bz')
x <- cbind(x1,x2,x3,x4)
write.csv(x,'daily_descriptive.csv',row.names=F)

x1 <- ol %>% group_by(week) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))
x2 <- z24 %>% group_by(week) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-week)
x3 <- ec %>% group_by(week) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-week)
x4 <- bz %>% group_by(week) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))%>% select(-week)

colnames(x1)[-1] <- paste0(colnames(x1)[-1],'_week_crmol')
colnames(x2) <- paste0(colnames(x2),'_week_z24')
colnames(x3) <- paste0(colnames(x3),'_week_crmec')
colnames(x4) <- paste0(colnames(x4),'_week_bz')
x <- cbind(x1,x2,x3,x4)
write.csv(x,'week_descriptive.csv',row.names=F)

x1 <- ol %>% group_by(month) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))
x2 <- z24 %>% group_by(month) %>% 
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-month)
x3 <- ec %>% group_by(month) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item)) %>% select(-month)
x4 <- bz %>% group_by(month) %>%
  summarise(vol=sum(vol),val=sum(val),trans=n_distinct(oid),tval=s(tval),
            avp=val/vol,atp=val/trans,vpt=vol/trans,discount=val/tval,
            store=n_distinct(store),item=n_distinct(item))%>% select(-month)

colnames(x1)[-1] <- paste0(colnames(x1)[-1],'_month_crmol')
colnames(x2) <- paste0(colnames(x2),'_month_z24')
colnames(x3) <- paste0(colnames(x3),'_month_crmec')
colnames(x4) <- paste0(colnames(x4),'_month_bz')
x <- cbind(x1,x2,x3,x4)
write.csv(x,'month_descriptive.csv',row.names=F)
