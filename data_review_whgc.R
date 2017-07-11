rm(list=ls())
library(data.table)
library(dplyr)
library(slam)
library(sqldf)

############################

setwd('C:/Users/admin/Documents/wenhuaguangchang/') #set up the working folder
load("rawdata_20170707.rdata") # load data
head(rawdata) #check variables

############################

# 总体 总票房 和售出票数
# 总体 平均每场次票房 和 售出票数
# 总体 by 平均票单价分组 （看看数据具体情况份） 总票房，票数
# 总体 by 剧目 总票房, 票数, 平均票单价
# 总体 by 剧目 平均每场次票房 和 售出票数
# 总体 by 购买渠道 总票房 和 售出张数
# 总体 by 人均观剧次数分组 总票房 和 售出票数 (1次，2~5次，5~10次...之类，视具体数据情况来定)
# 总体 by 单笔transaction 票数分组 总票房 和 售出票数 （1张，2~5张...之类，视具体数据情况来定)
# 总体 by 价格段分组 总票房 和 售出票数
# 总体 by 折扣 和 原价 分组 总票房 和 售出票数（或者按照不同折扣力度分组汇总，看看数据具体情况）
# 总体 by 提前购票时间分组  (2天，10天，1个月…之类,视具体数据情况来定)

#Generate dataset for descriptive
mfile <- arrange(rawdata,sessionnmtop,session_time) #sort the data by name and date of the show
mfile <- select(rawdata,order_type,sessionnmtop,session_time,order_num,order_time,payment,list_payment,session_timeperiod) #select the variables we need
mfile <- filter(mfile,!is.na(session_time)) #filter the obs with <NA>
mfile <- mutate(mfile,
                session_month=substr(session_time,1,7),
                avp = payment/order_num,
                rp = list_payment/order_num) #generate more variables

############################
#out1-out5需要rita基本上搞清楚并且能短期逐渐自己搞定
#基本上就是通过不同方式的group by来summarise数据
#n_distinct这个函数是数有多少个unique选项
############################

out1 <- mfile %>% group_by(session_month) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol
) # 总体 总票房 和售出票数
out2 <- mfile %>% group_by(session_month,sessionnmtop,session_time) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol
) # 总体 平均每场次票房 和 售出票数
out3 <- mfile %>% group_by(session_month,rp) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol
) #总体 by 平均票单价分组 （看看数据具体情况份） 总票房，票数
out4 <- mfile %>% group_by(session_month,sessionnmtop) %>% summarise(
  n=n_distinct(session_time),vol=sum(order_num),val=sum(payment),avp=val/vol
) ## 总体 by 剧目 平均每场次票房 和 售出票数
out5 <- mfile %>% group_by(session_month,order_type) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol
) ## 总体 by 购买渠道 总票房 和 售出张数

############################
#out1-out5需要rita基本上搞清楚并且能短期逐渐自己搞定
#基本上就是通过不同方式的group by来summarise数据
#n_distinct这个函数是数有多少个unique选项
############################

mfile <- arrange(rawdata,sessionnmtop,session_time) #sort the data by name and date of the show
mfile <- select(rawdata,mem_mobile,mem_level_t,order_type,sessionnmtop,session_time,order_time,order_num,order_time,payment,list_payment,session_timeperiod) #select the variables we need
mfile <- filter(mfile,!is.na(session_time)) #filter the obs with <NA>
mfile <- mutate(mfile,
                session_month=substr(session_time,1,7),
                avp = payment/order_num,
                rp = list_payment/order_num,
                sessionid = paste(sessionnmtop,session_time),
                tpr = ifelse(avp/rp>1,1,avp/rp),
                tpr_range = round(tpr*5)/5,
                timediff=floor(as.numeric((session_time-order_time)/60/60/24)),
                timediff_range = "over2month") #generate more variables
mfile$timediff_range[mfile$timediff<=60] <- "in2months"
mfile$timediff_range[mfile$timediff<=30] <- "in1month"
mfile$timediff_range[mfile$timediff<=7] <- "in1week"
mfile$timediff_range[mfile$timediff<=2] <- "in2days"
mfile$timediff_range[mfile$timediff<1] <- "justbuyit"

mfile <- unique(mfile)
mem_profile <- mfile %>% group_by(mem_mobile) %>% summarise(
  n=n_distinct(sessionid),vol=sum(order_num),val=sum(payment),avp=val/vol
)
out6 <- mem_profile %>% group_by(n) %>% summarise(
  vol=sum(vol),val=sum(val),count=n(),avp=val/vol
)# 总体 by 人均观剧次数分组 总票房 和 售出票数 (1次，2~5次，5~10次...之类，视具体数据情况来定)
out7 <- mfile %>% group_by(order_num,session_month) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol
) # 总体 by 单笔transaction 票数分组 总票房 和 售出票数 （1张，2~5张...之类，视具体数据情况来定)
out8 <- mfile %>% group_by(tpr_range,session_month) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol 
)# 总体 by 折扣 和 原价 分组 总票房 和 售出票数（或者按照不同折扣力度分组汇总，看看数据具体情况）
out9 <- mfile %>% group_by(timediff_range,session_month) %>% summarise(
  vol=sum(order_num),val=sum(payment),avp=val/vol 
) # 总体 by 提前购票时间分组  (2天，10天，1个月…之类,视具体数据情况来定)
