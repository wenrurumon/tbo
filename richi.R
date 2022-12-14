
rm(list=ls())
library(dplyr)
library(xgboost)
library(ggplot2)
library(openxlsx)
setwd('/Users/wenrurumon/Documents/training/1112/rich/20221212')

####################################
# Data
####################################

(stocklist <- dir())
stocklist <- stocklist[-grep('CSI',stocklist)]
raw <- lapply(dir(pattern='csv'),function(stocki){
  x <- read.csv(stocki)
  x <- x[,sapply(c('时间','开盘','最高','最低','收盘','金额'),function(xi){grep(xi,colnames(x))})]
  colnames(x) <- c('date','open','high','low','close','value')
  x$date <- as.Date(x$date)
  x[,-1] <- apply(x[,-1],2,as.numeric)
  cbind(stock=stocki,x)
})
names(raw) <- dir(pattern='csv')

RAW <- raw[sapply(raw,nrow)>400]

####################################
# Modeli
####################################

# i <- 0
goldgo <- function(X){
  # print(i<<-i+1)
  #Load Data
  raw <- X
  raw$did <- match(raw$date,sort(unique(raw$date)))
  raw <- raw %>% filter(date>='2021-01-01')
  #Predictor
  x0 <- raw
  hx <- 10
  X0 <- t(sapply(hx:nrow(x0),function(i){
    xi <- x0[1:(hx)+i-hx,]
    out <- sapply(hx:1,function(j){
      c(mean(xi$open[1:j]>xi$open[hx]),
        mean(xi$open[1:j]/xi$open[hx]),
        mean(xi$high[1:j]>xi$open[hx]),
        mean(xi$high[1:j]/xi$open[hx]),
        mean(xi$low[1:j]>xi$open[hx]),
        mean(xi$low[1:j]/xi$open[hx]),
        mean(xi$close[1:j]>xi$open[1:j]),
        mean(xi$close[1:j]/xi$open[1:j]))
    })
    c(did=max(xi$did)+1,as.vector(out))
  }))
  #Responsor
  y0 <- raw
  hy <- 30
  Y0 <- t(sapply(1:(nrow(y0)-hy),function(i){
    yi <- y0[i:(i+hy),]
    c(
      did=yi$did[1],
      prop=mean(yi$open[-1]>yi$open[1]),
      mean=mean(yi$open[-1]/yi$open[1]),
      prop8=mean(quantile(yi$open[-1],0.8)>yi$open[1]),
      mean8=mean(quantile(yi$open[-1],0.8)/yi$open[1])
    )
  }))
  #Modeling
  modelfile <- X0 %>% 
    merge(Y0,by='did') %>%
    merge(raw %>% select(did,date))
  train <- list(
    data = (modelfile %>%
              filter(date<'2022-06-01'))[,2:81],
    label = (modelfile %>%
               filter(date<'2022-06-01'))[,-1:-81]
  )
  test <- list(
    data = (modelfile %>%
              filter(date>='2022-06-01'))[,2:81],
    label = (modelfile %>%
               filter(date>='2022-06-01'))[,-1:-81]
  )
  #Modeling
  model <- xgboost(data = as.matrix(train$data), label = train$label$mean8>1.1,
                   max.depth = 10, eta = 2, nthread = 2, nrounds = 100,
                   objective = "binary:logistic",verbose=0)
  plot.ts(model$evaluation_log$train_logloss)
  score <- predict(model,newdata=as.matrix(rbind(train$data,test$data)))
  #Resulting
  rlt <- modelfile %>%
    select(date,prop,mean,prop8,mean8) %>%
    mutate(score=score,stock=unique(raw$stock)) %>%
    merge(raw %>% select(date,open))
  rlt
}
system.time(decomp <- do.call(rbind,lapply(RAW,goldgo)))

decomp %>% 
  group_by(date>='2022-06-01',stock) %>% 
  summarise(cor(mean8,score)) %>%
  as.data.frame

####################################
# Validation
####################################

valid <- sapply(1:6,function(threshold){
  threshold <- 1-10^(-threshold)
  #Transaction
  log <- decomp %>%
    filter(score>threshold) %>%
    merge(
      decomp %>%
        filter(score>threshold) %>%
        group_by(date) %>%
        summarise(p=sum(score))
    ) %>%
    mutate(p=score/p) %>%
    select(date,stock,score,p) %>%
    mutate(did=match(date,sort(unique(decomp$date))))
  if(nrow(log)==0){
    return(c(threshold=threshold,profit=0,cost=0))
  }
  logs <- rep(list(NA),min(log$did)-1)
  for(i in (min(log$did)):max(log$did)){
    logi <- log %>% filter(did==i)
    if(nrow(logi)>0){
      logs[[i]] <- logi
    } else {
      logs[[i]] <- logs[[i-1]] %>% mutate(did=i)
    }
  }
  log <- do.call(rbind,logs)[,-1] %>%
    as.data.frame %>%
    filter(!is.na(did)) %>%
    mutate(date=sort(unique(decomp$date))[did])
  #Price
  price <- decomp %>%
    select(date,stock,open) %>%
    mutate(did=match(date,sort(unique(decomp$date)))) %>%
    merge(
      decomp %>%
        select(date,stock,open) %>%
        mutate(did=match(date,sort(unique(decomp$date)))) %>%
        mutate(did=did-1),
      by=c('did','stock')
    ) %>%
    select(did,date=date.x,stock,buy=open.x,sell=open.y)
  #Trend
  price %>%
    filter(did%in%range((price %>% filter(date>='2022-01-01'))$did)) %>%
    group_by(did) %>%
    summarise(buy=mean(buy))
  #Balance
  cost <- log %>%
    merge(log %>% mutate(did=did-1),by=c('did','stock')) %>%
    mutate(change=p.y-p.x) %>%
    group_by(date=date.x) %>%
    summarise(cost=sum(abs(change))*0.001) %>%
    summarise(cost=sum(cost))
  profit <- log %>%
    filter(date>='2022-06-01') %>%
    arrange(did,date,stock) %>%
    merge(price) %>%
    mutate(profit=sell/buy) %>%
    group_by(did,date) %>%
    summarise(profit=sum(p*profit)) %>%
    group_by(threshold=threshold) %>%
    summarise(profit=prod(profit)-1)
  profit <- ifelse(nrow(profit)==0,0,profit$profit)
  c(threshold=threshold,profit=profit,cost)
}) %>% t
valid

####################################
# Test
####################################

plot.ts(unlist(valid[,2]))
threshold <- 1-10^(-which.max(unlist(valid[,2])-unlist(valid[,3])))

#Transaction
log <- decomp %>%
  filter(score>threshold) %>%
  merge(
    decomp %>%
      filter(score>threshold) %>%
      group_by(date) %>%
      summarise(p=sum(score))
  ) %>%
  mutate(p=score/p) %>%
  select(date,stock,score,p) %>%
  mutate(did=match(date,sort(unique(decomp$date))))
logs <- rep(list(NA),min(log$did)-1)
for(i in (min(log$did)):max(log$did)){
  logi <- log %>% filter(did==i)
  if(nrow(logi)>0){
    logs[[i]] <- logi
  } else {
    logs[[i]] <- logs[[i-1]] %>% mutate(did=i)
  }
}
log <- do.call(rbind,logs)[,-1] %>%
  as.data.frame %>%
  filter(!is.na(did)) %>%
  mutate(date=sort(unique(decomp$date))[did])

#Price
price <- decomp %>%
  select(date,stock,open) %>%
  mutate(did=match(date,sort(unique(decomp$date)))) %>%
  merge(
    decomp %>%
      select(date,stock,open) %>%
      mutate(did=match(date,sort(unique(decomp$date)))) %>%
      mutate(did=did-1),
    by=c('did','stock')
  ) %>%
  select(did,date=date.x,stock,buy=open.x,sell=open.y)

#Trend
price %>%
  filter(did%in%range((price %>% filter(date>='2022-06-01'))$did)) %>%
  group_by(did) %>%
  summarise(buy=mean(buy))

#Balance
cost <- log %>%
  merge(log %>% mutate(did=did-1),by=c('did','stock')) %>%
  mutate(change=p.y-p.x) %>%
  group_by(date=date.x) %>%
  summarise(cost=sum(abs(change))*0.001) %>%
  summarise(cost=sum(cost))

test <- (log %>%
           filter(date>='2022-06-01') %>%
           arrange(did,date,stock) %>%
           merge(price) %>%
           mutate(profit=sell/buy) %>%
           group_by(did,date) %>%
           summarise(profit=sum(p*profit))) %>%
  merge(price %>%
          group_by(did) %>%
          summarise(price=mean(buy)))
test$profit <- cumprod(test$profit)
test$price <- test$price/test$price[1]
test %>% 
  ggplot() + 
  geom_line(aes(x=date,y=profit-price))
test %>% 
  reshape2::melt(id=1:2) %>%
  ggplot() + 
  geom_line(aes(x=date,y=value,colour=variable))

profit <- log %>%
  filter(date>='2022-06-01') %>%
  arrange(did,date,stock) %>%
  merge(price) %>%
  mutate(profit=sell/buy) %>%
  group_by(did,date) %>%
  summarise(profit=sum(p*profit)) %>%
  group_by(threshold=threshold) %>%
  summarise(gain=mean(profit>1),profit=prod(profit)-1)
profit <- ifelse(nrow(profit)==0,0,profit$profit)
c(threshold=threshold,profit=profit,cost)

####################################
# Prediction
####################################

goldgo2 <- function(X){
  # print(i<<-i+1)
  #Load Data
  raw <- X
  raw$did <- match(raw$date,sort(unique(raw$date)))
  raw <- raw %>% filter(date>='2021-01-01')
  #Predictor
  x0 <- raw
  hx <- 10
  X0 <- t(sapply(hx:nrow(x0),function(i){
    xi <- x0[1:(hx)+i-hx,]
    out <- sapply(hx:1,function(j){
      c(mean(xi$open[1:j]>xi$open[hx]),
        mean(xi$open[1:j]/xi$open[hx]),
        mean(xi$high[1:j]>xi$open[hx]),
        mean(xi$high[1:j]/xi$open[hx]),
        mean(xi$low[1:j]>xi$open[hx]),
        mean(xi$low[1:j]/xi$open[hx]),
        mean(xi$close[1:j]>xi$open[1:j]),
        mean(xi$close[1:j]/xi$open[1:j]))
    })
    c(did=max(xi$did)+1,as.vector(out))
  }))
  #Responsor
  y0 <- raw
  hy <- 30
  Y0 <- t(sapply(1:(nrow(y0)-hy),function(i){
    yi <- y0[i:(i+hy),]
    c(
      did=yi$did[1],
      prop=mean(yi$open[-1]>yi$open[1]),
      mean=mean(yi$open[-1]/yi$open[1]),
      prop8=mean(quantile(yi$open[-1],0.8)>yi$open[1]),
      mean8=mean(quantile(yi$open[-1],0.8)/yi$open[1])
    )
  }))
  #Modeling
  modelfile <- X0 %>% 
    merge(Y0,by='did') %>%
    merge(raw %>% select(did,date))
  train <- list(
    data = (modelfile)[,2:81],
    label = (modelfile)[,-1:-81]
  )
  colnames(train$data) <- paste0('V',1:ncol(train$data))
  #Modeling
  model <- xgboost(data = as.matrix(train$data), label = train$label$mean8>1.1,
                   max.depth = 10, eta = 2, nthread = 2, nrounds = 100,
                   objective = "binary:logistic",verbose=0)
  plot.ts(model$evaluation_log$train_logloss)
  score <- predict(model,newdata=as.matrix(rbind(train$data,test$data)))
  pred <- predict(model,newdata=X0[,-1] %>% as.data.frame %>% as.matrix)
  pred <- cbind(did=X0[,1],pred) %>%
    merge(raw %>% select(did,date)) %>%
    mutate(stock=unique(raw$stock))
  #Resulting
  rlt <- modelfile %>%
    select(date,prop,mean,prop8,mean8) %>%
    mutate(score=score,stock=unique(raw$stock)) %>%
    merge(raw %>% select(date,open))
  list(train=rlt,predict=pred)
}
system.time(decomp2 <- lapply(RAW,goldgo2))

pred <- do.call(rbind,lapply(decomp2,function(x){x$predict}))
i <- max(pred$date)
pred <- pred %>%
  filter(date==i) %>%
  arrange(-pred) %>%
  select(stock,pred) %>%
  mutate(threshold=1-10^(-floor(-log(1-pred,10)))) %>%
  mutate(level=floor(-log(1-pred,10))) %>%
  merge(valid) %>%
  mutate(roi=as.numeric(profit)-as.numeric(cost)) %>%
  filter(roi>0) %>%
  mutate(score=roi*level)

pred$prop <- pred$score/sum(pred$score)
pred %>% arrange(desc(pred)) %>%
  select(stock,level,roi,prop)
