
rm(list=ls())
library(rvest)
library(dplyr)
library(data.table)
library(xgboost)
library(ggplot2)
library(reticulate)
setwd('/Users/wenrurumon/Documents/training/1112/rich')

##################################################
# Load Data
##################################################

stocklist <- "002747,002979,603416,688697,688257,688308,603507,605222,600458,603667,601778,688560,300757,601222,300566,688223,300554,600732,688170,002943,002529,300438,688663,301162,688226,002121,600478,605286,300490,000035,600580,300174,000539,600509,001289,688628,688630,002436,688187,603936,002643,688300,002426,300331,688200,603773,688608,002371,600641,300217,002385,300087,600809,000860,688089,300146,300888,600814,600859,000963,300765,600380,301017,002422,688212,301015,300633,600998,300253,688029,002332,300206,603309,000516,300803,300033,601688,600095,000002,601155,002285,601800,601186,601390,002084,002043,300737,603208,002271,603060,688156,002935,600435,000534,002338,002389,002683,300860,600826,002027,002614,002104,002803,000625,300203,002965,300003,300869,301078,601890,002487"
stocklist <- strsplit(stocklist,',')[[1]]

getstock <- function(stockid,yearid,seasonid){
  url <- paste0(
    'http://quotes.money.163.com/trade/lsjysj_',
    paste(stockid),
    '.html?year=',
    yearid,
    '&season=',
    seasonid
  )
  print(url)
  html <- read_html(url)
  out <- html %>%
    html_nodes('.limit_sale td') %>%
    html_text()
  stockname <- html %>%
    html_nodes('#menuCont strong') %>%
    html_text()
  out <- t(matrix(out,nrow=11))
  data.table(stockname,out) %>%
    select(stockname,date=2,open=3,high=4,low=5,close=6,value=9)
}

getstock_ys <- function(stockid){
  do.call(rbind,lapply(2020:2022,function(y){
    do.call(rbind,lapply(1:4,function(s){
      out <- try(getstock(stockid,yearid=y,seasonid=s))
      if(class(out)[1]=='try-error'){
        return(NULL)
      } else {
        return(out)
      }
    }))
  })) %>%
    arrange(date)
}

getstocks <- function(stocklist){
  do.call(rbind,lapply(stocklist,getstock_ys))
}

# system.time(raw <- getstocks(stocklist))
# write.csv(
#   raw %>% filter(!is.na(date)),
#   '/Users/wenrurumon/Documents/training/1112/rich/rollingrich.csv',
#   row.names=F
# )
raw <- read.csv('/Users/wenrurumon/Documents/training/1112/rich/rollingrich.csv')
raw <- apply(raw,2,function(x){
  gsub(',','',x)
}) %>% as.data.frame
raw[,-1:-2] <- apply(raw[,-1:-2],2,as.numeric)

##################################################
# Modelfile
##################################################

stock2test <- unique(raw$stockname)

model <- function(stocki){
  #Modelfile Processing
  # stocki <- stock2test[2]
  rawi <- raw %>% 
    filter(stockname==stocki) %>%
    arrange(date) %>%
    filter(date>='2021-01-01')
  rawi$did <- match(rawi$date,rawi$date)
  hy <- 30
  Y <- t(sapply(1:(nrow(rawi)-hy),function(i){
    xi <- rawi[1:hy+i-1,]
    c(did=xi$did[1],
      win2=xi$close[1]/xi$open[1],
      winrate=mean(xi$open[-1]/xi$open[1]>=1.1),
      retrace=1/exp(diff(log(range(xi$open[1:max(which(xi$open==max(xi$open)))]))))-1)
  }))
  hx <- 10
  X <- t(sapply((hx+1):nrow(rawi),function(i){
    xi <- rawi[hx:1+i-1-hx,]
    max()
    c(did=xi$did[1]+1,
      xi %>%
        select(open,value) %>%
        as.matrix() %>%
        scale() %>%
        as.vector())
  }))
  modelfile <- Y %>%
    merge(X,by='did') %>%
    merge(
      rawi %>% 
        select(did,date) %>%
        unique()
    ) %>% mutate(set=ifelse(date<'2022-06-01','Training','Test')) %>%
    select(-date)
  #Train and Test Dataseting
  train <- list(
    data = modelfile %>%
      filter(set=='Training') %>%
      select(-match(c(colnames(Y),'set'),colnames(modelfile))) %>%
      as.matrix(),
    label = modelfile %>%
      filter(set=='Training') %>%
      select(match(colnames(Y)[-1],colnames(modelfile))) %>%
      as.matrix
  )
  test <- list(
    data = modelfile %>%
      filter(set=='Test') %>%
      select(-match(c(colnames(Y),'set'),colnames(modelfile))) %>%
      as.matrix(),
    label = modelfile %>%
      filter(set=='Test') %>%
      select(match(colnames(Y)[-1],colnames(modelfile))) %>%
      as.matrix
  )
  #Modeling
  # model <- xgb.train(
  #   data=dtrain,
  #   booster='gblinear',
  #   nthread = 3,
  #   nrounds=200,
  #   watchlist=list(train=dtrain,test=dtest),
  #   verbose=0,
  #   objective = "reg:squarederror")
  # plot.ts(model$evaluation_log[,-1])
  scores <- apply(train$label,2,function(y){
    model <- xgboost(
      data=train$data,label=y,
      booster='gbtree', max_depth = 10, eta = 0.3, nthread = 2, nrounds = 30, 
      lambda = 0.2,
      objective = "reg:squarederror")
    predict(model,
            newdata = modelfile %>%
              select(-match(c(colnames(Y),'set'),colnames(modelfile))) %>%
              as.matrix())
  })
  colnames(scores) <- paste0('predict_',colnames(scores))
  rlt <- rawi %>% 
    select(did,date) %>%
    merge(
      cbind(stock=stocki,modelfile,scores) 
    ) %>%
    select(-did)
  rlt %>%
    group_by(set) %>%
    summarise(cor(predict_win2,win2),
              cor(predict_winrate,winrate),
              cor(predict_retrace,retrace))

}

system.time(scores <- do.call(rbind,lapply(stock2test,model)))
scores %>%
  group_by(stock,set) %>%
  summarise(r=cor(winrate,score)) %>%
  group_by(stock) %>%
  summarise(train=mean(r[set=='Training']),test=mean(r[set=='Test']))

scores %>%
  group_by(set,date) %>%
  summarise(r=cor(winrate,score)) %>%
  ggplot() +
  geom_point(aes(x=date,y=r,colour=set)) +
  theme(axis.text.x=element_text(angle=90))

##################################################
# Prediction
##################################################

pmodel <- function(stocki){
  #Modelfile Processing
  # stocki <- stock2test[3]
  rawi <- raw %>% 
    filter(stockname==stocki) %>%
    arrange(date) %>%
    filter(date>='2021-01-01')
  rawi$did <- match(rawi$date,rawi$date)
  hy <- 30
  Y <- t(sapply(1:(nrow(rawi)-hy),function(i){
    xi <- rawi[1:hy+i-1,]
    c(did=xi$did[1],
      winrate=mean(xi$open[-1]/xi$open[1]>=1.1),
      retrace=1/exp(diff(log(range(xi$open[1:max(which(xi$open==max(xi$open)))]))))-1)
  }))
  hx <- 10
  X <- t(sapply((hx+1):nrow(rawi),function(i){
    xi <- rawi[hx:1+i-1-hx,]
    max()
    c(did=xi$did[1]+1,
      xi %>%
        select(open,value) %>%
        as.matrix() %>%
        scale() %>%
        as.vector())
  }))
  modelfile <- Y %>%
    merge(X,by='did',all.y=T) %>%
    merge(
      rawi %>% 
        select(did,date) %>%
        unique()
    ) %>%
    select(-date)
  #Train and Test Dataseting
  train <- list(
    data = modelfile %>%
      filter(!is.na(winrate)) %>%
      select(-did,-winrate) %>%
      as.matrix(),
    label = modelfile %>%
      filter(!is.na(winrate)) %>%
      select(winrate) %>%
      as.matrix
  )
  #Modeling
  model <- xgboost(
    data=train$data,label=train$label,
    booster='gblinear',
    nthread = 3, 
    nrounds=200, 
    verbose=0,
    objective = "reg:squarederror")
  #Resulting
  score <- predict(model,
                   newdata = modelfile %>%
                     select(-did,-winrate) %>%
                     as.matrix())
  rawi %>% 
    select(did,date) %>%
    merge(
      cbind(stock=stocki,modelfile,score=score) 
    ) %>%
    select(-did)
  # modelfile %>% 
  #   mutate(score=score) %>%
  #   group_by(set) %>%
  #   summarise(cor(score,winrate))
  # modelfile %>%
  #   ggplot() + 
  #   geom_point(aes(x=winrate,y=score,colour=set)) +
  #   facet_grid(~set)
}
system.time(pscores <- do.call(rbind,lapply(stock2test,pmodel)))
pscores %>%
  filter(date==max(raw$date)) %>%
  select(stock,score) %>%
  arrange(desc(score))
