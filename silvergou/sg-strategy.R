
Sys.sleep(60)
library(reticulate)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(rvest)
library(data.table)
library(devtools)
library(RSelenium)
library(stringr)
library(methods)
library(RS.Driver)
library(reshape2)
library(lubridate)
library(TTR)
use_condaenv(condaenv='/Users/wzy/Library/r-miniconda/envs/r-reticulate/bin/python',required=TRUE)
py_module_available('akshare')
ak <- reticulate::import("akshare") 
options(scipen = 200)

#############################################################################
###################################对应码表##################################
#############################################################################

#akshare接口文档地址：https://akshare.akfamily.xyz/index.html
#股票代码和名称的对应表，用于通过code查询name

stockname <- ak$stock_zh_a_spot_em() %>%
  select(code=代码, name=名称)


#############################################################################
###################################历史走势##################################
#############################################################################

#获取所有股票信息，分别获取上交所和深交所然后拼接

back.stocks <- ak$stock_sh_a_spot_em() %>% select(code=代码) %>%
  rbind(ak$stock_sz_a_spot_em() %>% select(code=代码))

#获取日维度交易数据

back.stocks.data <- do.call(rbind,lapply(1:nrow(back.stocks),function(i){
  #获取交易数据的函数
  print(i)
  Sys.sleep(0.01)
  codei <- back.stocks$code[i]
  ak$stock_zh_a_hist(symbol=codei, period='daily', start_date='20220101',
                     end_date=gsub('-','',Sys.Date()), adjust='qfq') %>% 
    mutate(code=codei)
}))

back.stocks.data.store <- back.stocks.data

#对需要用到的日维度交易数据进行选取

back.stocks.data2 <- back.stocks.data %>%
  select(date=日期,code,open=开盘,close=收盘,high=最高,low=最低,val=成交额,shift=涨跌幅) %>%
  mutate(date=gsub('-','',date))

#筛选符合时段要求的股票，屏蔽那些最近2个月刚上市的股票

back.stocks.data3 <- back.stocks.data2[back.stocks.data2$code%in%(back.stocks.data2 %>% filter(date<=20230531))$code,]

length(unique(back.stocks.data2$code))
length(unique(back.stocks.data3$code))

#整理时间序列

back.date <- sort(unique(back.stocks.data2$date))

#业绩比较基准CSI800的走势情况

back.CSI800 <- ak$stock_zh_index_daily(symbol='sh000906')
for(i in 1:dim(back.CSI800)[1]){
  back.CSI800$date[[i]] <- paste(as.Date(paste(back.CSI800$date[[i]]), format='%Y-%m-%d'))
}

back.CSI800 <- back.CSI800 %>%
  select(date,close) %>%
  mutate(date=gsub('-','',date))

back.CSI800 <- back.CSI800  %>%
  mutate(did=match(date,back.CSI800$date))

back.CSI800 <- back.CSI800 %>% 
  merge(back.CSI800 %>% mutate(did=did+1),by='did') %>%
  mutate(profit=(close.x-close.y)/close.x) %>%
  select(date=date.x,profit) %>%
  filter(date>=20220101 & date<=gsub('-','',Sys.Date()))


#############################################################################
###################################计算五日线################################
#############################################################################

back.stocks.5MA <- do.call(rbind,lapply(1:length(unique(back.stocks.data3$code)),function(i){
  #计算5日线的函数
  print(i)
  codei <- unique(back.stocks.data3$code)[i]
  back.stocks.5MAi <- back.stocks.data3 %>%
    filter(code==codei)
  back.stocks.5MAi <- back.stocks.5MAi %>%
    cbind(MA=SMA(back.stocks.5MAi$close,5)) %>%
    #SMA函数，用于计算移动平均线，来自TTR包
    filter(!is.na(MA))
}))


#############################################################################
#################################基础数据####################################
#############################################################################

#收盘价一阶导
#成交额一阶导
#收盘价一阶导的20日均线
#成交额一阶导的20日均线

silver.basic <- do.call(rbind,lapply(1:length(unique(back.stocks.data3$code)),function(i){
  #获取必备参数的函数
  print(i)
  codei <- unique(back.stocks.data3$code)[i]
  summary.backi <- back.stocks.data3 %>% filter(code==codei)
  test2.linei <- summary.backi %>%
    cbind(MA=SMA(summary.backi$close,5)) %>%
    filter(!is.na(MA))
  test2.linei <- test2.linei %>%
    cbind(pace=c(NA,diff(test2.linei$MA)/test2.linei$close[-1])) %>%
    filter(!is.na(pace))
  test2.linei <- test2.linei %>%
    cbind(paceMA=SMA(test2.linei$pace,20),
          valMA=SMA(test2.linei$val,20)) %>%
    filter(!is.na(paceMA)) 
  test2.linei <- test2.linei %>%
    mutate(did=match(date,test2.linei$date))
  test2.lini <- test2.linei %>%
    mutate(open1=open,close1=close,high1=high,low1=low,val1=val,
           shift1=shift,MA1=MA,pace1=pace,paceMA1=paceMA,valMA1=valMA) %>%
    select(date,code,did,open1,close1,high1,low1,val1,shift1,MA1,
           pace1,paceMA1,valMA1) %>%
    merge(test2.linei %>%
            mutate(did=did+1,open2=open,close2=close,high2=high,low2=low,val2=val,
                   shift2=shift,MA2=MA,pace2=pace,paceMA2=paceMA,valMA2=valMA) %>%
            select(did,open2,close2,high2,low2,val2,shift2,MA2,
                   pace2,paceMA2,valMA2),by='did') %>%
    merge(test2.linei %>%
            mutate(did=did+2,open3=open,close3=close,high3=high,low3=low,val3=val,
                   shift3=shift,MA3=MA,pace3=pace,paceMA3=paceMA,valMA3=valMA) %>%
            select(did,open3,close3,high3,low3,val3,shift3,MA3,
                   pace3,paceMA3,valMA3),by='did') %>%
    merge(test2.linei %>%
            mutate(did=did+3,open4=open,close4=close,high4=high,low4=low,val4=val,
                   shift4=shift,MA4=MA,pace4=pace,paceMA4=paceMA,valMA4=valMA) %>%
            select(did,open4,close4,high4,low4,val4,shift4,MA4,
                   pace4,paceMA4,valMA4),by='did') %>%
    merge(test2.linei %>%
            mutate(did=did+4,open5=open,close5=close,high5=high,low5=low,val5=val,
                   shift5=shift,MA5=MA,pace5=pace,paceMA5=paceMA,valMA5=valMA) %>%
            select(did,open5,close5,high5,low5,val5,shift5,MA5,
                   pace5,paceMA5,valMA5),by='did') 
})) 


#############################################################################
#################################存储数据####################################
#############################################################################

setwd('/Users/wzy/Desktop/goldgou/R scripts')

save(stockname,back.stocks.data2,back.date,back.CSI800,
     back.stocks.data3,back.stocks.5MA,silver.basic,
     file = "basic_data.RData")


#############################################################################
#################################选股模式1###################################
#############################################################################

# T-1日收盘价一阶导大于T-1日收盘价一阶导20日均线*3
# T-1日收盘价一阶导大于0.01
# T-1日成交额大于T-1日成交额20日均线*4
# T-1日收盘价小于最高价
# T-1日开盘价小于收盘价
# T-1日收盘价一阶导大于T-2日收盘价一阶导
# T-1日成交额大于T-2日成交额
# T-1日收盘价大于T-2日收盘价
# T-2日收盘价一阶导大于T-2日收盘价一阶导20日均线*2
# T-1日K线实体部分*2>上下引线部分之和

silver.select1 <- silver.basic %>%
  filter(pace1>abs(paceMA1)*3 &
           pace1>0.01 &
           val1>valMA1*4 &
           close1<high1 &
           open1<close1 &
           pace1>pace2 &
           val1>val2 &
           close1>close2 &
           pace2>abs(paceMA2)*2 &
           (close1-open1)*2>(open1-low1+0.01)+(high1-close1+0.01))

#出现买点后，日期加一

silver.select1 <- silver.select1 %>%
  cbind(date2 = back.date[match(silver.select1$date,back.date)+1]) %>%
  select(date=date2,code=code)

#选股

silver.select1.code <- silver.select1 %>%
  select(code,date) %>%
  mutate(forecast=date) %>%
  arrange(date)

silver.select1.code %>% 
  group_by(date) %>%
  summarise(n()) %>%
  as.data.frame

silver.select1.code %>% 
  filter(is.na(date)) %>%
  merge(stockname,by='code') 


#############################################################################
#################################选股模式2###################################
#############################################################################

# T-1日收盘价一阶导大于T-1日收盘价一阶导20日均线
# T-1日成交额大于T-1日成交额20日均线*2
# T-1日开盘价小于收盘价
# T-1日收盘价大于T-2日收盘价
# T-2日收盘价一阶导大于T-2日收盘价一阶导20日均线
# T-2日涨幅超过9.8%
# T-1日K线实体部分*2>上下引线部分之和

silver.select2 <- silver.basic %>%
  filter(pace1>abs(paceMA1) &
           val2>valMA2*2 &
           open1<close1 &
           close1 > close2 &
           pace2>abs(paceMA2) &
           shift2>9.8 &
           (close1-open1)*2>((open1-low1+0.01)+(high1-close1+0.01)))

#出现买点后，日期加一

silver.select2 <- silver.select2 %>%
  cbind(date2 = back.date[match(silver.select2$date,back.date)+1]) %>%
  select(date=date2,code=code)

#选股

silver.select2.code <- silver.select2 %>%
  select(code,date) %>%
  mutate(forecast=date) %>%
  arrange(date)

silver.select2.code %>% 
  group_by(date) %>%
  summarise(n()) %>%
  as.data.frame

silver.select2.code %>% 
  filter(is.na(date)) %>%
  merge(stockname,by='code') 


#############################################################################
#################################选股模式4###################################
#############################################################################

# T-1日涨幅超过5%
# T-2日涨幅小于0
# T-3日涨幅超过5%
# T-1日成交额大于T-2日
# T-3日成交额大于T-2日
# T-1日上引线不超过2.5%

silver.select4 <- silver.basic %>%
  filter(shift1>5,
         shift2<0,
         shift3>5,
         val1>val2,
         val3>val2,
         (high1-close1)/close1<=0.025)

#出现买点后，日期加一

silver.select4 <- silver.select4 %>%
  cbind(date2 = back.date[match(silver.select4$date,back.date)+1]) %>%
  select(date=date2,code=code)

#选股

silver.select4.code <- silver.select4 %>%
  select(code,date) %>%
  mutate(forecast=date) %>%
  arrange(date)

silver.select4.code %>% 
  group_by(date) %>%
  summarise(n()) %>%
  as.data.frame

silver.select4.code %>% 
  filter(is.na(date)) %>%
  merge(stockname,by='code') 


#############################################################################
#################################每日选股####################################
#############################################################################

#根据情绪周期原理，不一定取Top 8，也可能多看一些然后主观选择

temp <- rbind(silver.select1.code,
              silver.select2.code,
              silver.select4.code) %>%
  unique %>% 
  filter(is.na(date))

stock3 <- (temp %>% merge(stockname,by='code') %>%
             filter(!grepl('ST',name)) %>%
             filter(!grepl('退',name)) %>%
             merge(silver.basic %>%
                     filter(date==max(silver.basic$date)) %>%
                     select(-date),by='code') %>%
             arrange(desc(pace1)))[1:10,] %>%
  merge(stockname,by='code') %>%
  select(code,name=name.x)


#############################################################################
#################################历史选股####################################
#############################################################################

#测试集从2023年6月1日开始

raw.backtest <- rbind(silver.select2,silver.select1,silver.select4) %>% unique %>%
  mutate(buydate=date,
         forecast=date) %>%
  filter(forecast>=20230601,
         !is.na(forecast)) %>%
  mutate(valdate=back.date[match(forecast,back.date)+1])

#每天选取pace1排序最前的最多8个股票

backtest1 <- raw.backtest %>%
  merge(silver.basic,by=c('code','date')) %>%
  merge(
    raw.backtest %>%
      merge(silver.basic,by=c('code','date')) %>%
      group_by(forecast) %>%
      summarise(threshold=sort(pace1,decreasing=T)[8]) %>%
      mutate(threshold=if_else(is.na(threshold),0,threshold))
  ) %>%
  filter(pace1>=threshold)

backtest1 %>%
  group_by(forecast) %>%
  summarise(count=n())

#平均分配每日持股份额

backtest1 <- do.call(rbind,lapply(1:length(unique(backtest1$forecast)),function(i){
  print(i)
  forecasti <- unique(backtest1$forecast)[i]
  stocki <- backtest1 %>% filter(forecast==forecasti)
  numi <- nrow(stocki)
  backtest1i <- stocki %>%
    mutate(share=1/numi)
}))

#############################################################################
#################################仿真交易####################################
#############################################################################

#T日开盘买入，T+1日收盘卖出
#T日如果一字涨停，则买入轮空
#T日如果开盘涨幅在inner%以内，则开盘买入
#T日如果开盘在3%开外，则以盘中回落到inner%买入，否则轮空
#T+1日如果盘中涨停，则以涨停价卖出
#T+1日如果收盘跌停，则用T+2日收盘价卖出

#买入涨幅限制，比如设置为0.03，则意味着只有当天股票涨幅在3%或以内，才会被买入

inner <- 0.03

mock.transaction <- function(x) {
  #计算每日滚动收益率的函数
  #半仓轮动操作，T日买，T+1日收盘卖
  do.call(rbind,lapply(1:(length(unique(x$forecast))-1),function(i){
    print(i)
    forecasti <- unique(x$forecast)[i]
    stocki <- x %>% filter(forecast==forecasti)
    todayi <- forecasti
    afteri <- back.date[which(back.date==forecasti)+1]
    after2i <- back.date[which(back.date==forecasti)+2]
    beforei <- back.date[which(back.date==forecasti)-1]
    datai_today <- back.stocks.data2 %>% filter(date==todayi)
    datai_afteri <- back.stocks.data2 %>% filter(date==afteri)
    datai_after2i <- back.stocks.data2 %>% filter(date==after2i)
    datai_beforei <- back.stocks.data2 %>% filter(date==beforei)
    stock2i <- stocki %>%
      merge(datai_today,by='code') %>%
      merge(datai_beforei,by='code') %>%
      mutate(open.shift=(open.x-close.y)/close.y,
             low.shift=(low.x-close.y)/close.y,
             inner.point=floor(close.y*(1+inner)*100)/100,
             one.zt.flag=ifelse((high.x==low.x)&(shift.x>0),1,0),
             open.buy.flag=ifelse(open.shift<inner,1,0),
             inner.buy.flag=ifelse((open.shift>=inner)&(low.shift<inner),1,0),
             buyprice=ifelse(one.zt.flag==1,
                             NA,
                             ifelse(open.buy.flag==1,
                                    open.x,
                                    ifelse(inner.buy.flag==1,
                                           inner.point,
                                           NA)))) %>%
      filter(!is.na(buyprice)) %>%
      select(code,forecast,share,buydate,buyprice,valdate,benchmark=close.x)
    stock3i <- stock2i %>%
      merge(datai_afteri,by='code') %>%
      merge(datai_after2i,by='code') %>%
      mutate(limit.flag=ifelse(substr(code,1,2) %in% c('68','30'),1,2),
             high.shift=(high.x-benchmark)/benchmark,
             one.dt.flag=ifelse((high.x==low.x)&(shift.x<0),1,0),
             sellprice=ifelse(one.dt.flag==1,
                              close.y,
                              ifelse((high.shift>=0.098)&(limit.flag=2),
                                     high.x,
                                     ifelse((high.shift>=0.198)&(limit.flag=1),
                                            high.x,
                                            close.x)))) %>%
      select(code,forecast,share,buydate,buyprice,valdate,sellprice) %>%
      mutate(profit=(sellprice-buyprice)*share/buyprice)
    data.frame(date=forecasti,
               profit=sum(stock3i$profit)/2)
    #profit要除以2，因为每天半仓轮动操作
  })) %>% mutate(accu.profit=cumprod(1+profit)-1)
}

profit.silver <- mock.transaction(backtest1)


#############################################################################
#################################做图对比####################################
#############################################################################

#当天收益

spot.gram <- function(x,y,z){
  #获取每日收益的函数
  x <- x %>% filter(date>=z) %>% 
    mutate(temp=profit)
  colnames(x)[ncol(x)] <- y
  x  %>% select(date,y)
}

spot.gram(profit.silver,'sg',20230601) %>%
  merge(spot.gram(back.CSI800,'CSI800',20230601),by='date') %>%
  melt(id=1) %>%
  ggplot() + 
  geom_line(aes(x=match(date,profit.silver$date),y=value,colour=variable))

#累计收益

accu.gram <- function(x,y,z){
  #获取累计收益的函数
  x <- x %>% filter(date>=z) %>% 
    mutate(temp=cumprod(1+profit)-1)
  colnames(x)[ncol(x)] <- y
  x %>% select(date,y)
}

accu.gram(profit.silver,'sg',20230601) %>%
  merge(accu.gram(back.CSI800,'CSI800',20230601),by='date') %>%
  melt(id=1) %>%
  ggplot() + 
  geom_line(aes(x=match(date,profit.silver$date),y=value,colour=variable))


#############################################################################
#################################统计检验####################################
#############################################################################

test1 <- do.call(rbind,lapply(1:length(unique(backtest1$date)),function(i){
  print(i)
  datei <- unique(backtest1$date)[i]
  datei_after <- back.date[match(datei,back.date)+1]
  silver.basici <- silver.basic %>% 
    filter(date==datei) %>% 
    select(date,code,pace1,pace2,val1) %>%
    mutate(date1=datei,
           date2=datei_after) %>%
    merge(back.stocks.data2 %>%
            filter(date==datei) %>%
            select(code,open),by='code') %>%
    merge(back.stocks.data2 %>%
            filter(date==datei_after) %>%
            select(code,close),by='code') %>%
    mutate(contribution=close/open-1)
}))

#pace1

cor.test(test1$pace1,test1$contribution)

#pace2

cor.test(test1$pace2,test1$contribution)

#val1

cor.test(test1$val1,test1$contribution)


#############################################################################
#################################更多探索####################################
#############################################################################
