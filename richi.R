
rm(list=ls())
library(dplyr)
library(xgboost)
library(ggplot2)
library(openxlsx)
setwd('/Users/wenrurumon/Documents/training/1112/rich/20221207')

####################################
# Data
####################################

(stocklist <- dir())
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
  #Historical Data
  x0 <- raw %>% select(stock,date,did,open)
  for (i in 1:10){
    xi <- raw %>%
      mutate(did=did+i) %>%
      select(-stock)
    colnames(xi)[1:6] <- paste(colnames(xi)[1:6],i,sep='_')
    x0 <- x0 %>%
      merge(xi,by=c('did'))
  }
  X0 <- x0
  X <- X0[,!grepl('date_',colnames(X0))]
  #Profit Data
  y0 <- raw %>% select(stock,date,did,open)
  for (i in 1:15){
    yi <- raw %>%
      mutate(did=did-i) %>%
      select(did,date,open)
    colnames(yi)[-1] <- paste(colnames(yi)[-1],i,sep='_')
    y0 <- y0 %>%
      merge(yi,by=c('did'))
  }
  Y0 <- y0
  Y <- Y0[,!grepl('date_',colnames(Y0))]
  Y <- Y %>%
    select(did,stock,date) %>%
    mutate(profit=apply((Y[,-1:-4]),1,function(x){quantile(x,0.8)})/Y[,4])
  #Modeling
  modelfile <- X %>% merge(Y,by=c('did','stock','date'))
  train <- list(
    data = modelfile %>%
      filter(date<'2022-06-01') %>%
      select(-did,-date,-stock,-profit,-open) %>%
      as.matrix,
    label = modelfile %>%
      filter(date<'2022-06-01') %>%
      select(profit) %>%
      as.matrix
  )
  test <- list(
    data = modelfile %>%
      filter(date>='2022-06-01') %>%
      select(-did,-date,-stock,-profit,-open) %>%
      as.matrix,
    label = modelfile %>%
      filter(date>='2022-06-01') %>%
      select(profit) %>%
      as.matrix
  )
  #Modeling
  model <- xgboost(data = as.matrix(train$data), label = train$label>1.01,
                   max.depth = 10, eta = 2, nthread = 2, nrounds = 100,
                   objective = "binary:logistic",verbose=0)
  plot.ts(model$evaluation_log$train_logloss)
  score <- predict(model,newdata=rbind(train$data,test$data))
  #Resulting
  rlt <- modelfile %>%
    select(date,stock,open,profit) %>%
    mutate(score=score)
  rlt
}
system.time(decomp <- do.call(rbind,lapply(RAW,goldgo)))

decomp %>% 
  group_by(date>='2022-06-01',stock) %>% 
  summarise(cor(profit,score)) %>%
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

# print(i<<-0)
goldgo2 <- function(X){
  # print(i<<-i+1)
  #Load Data
  raw <- X
  raw$did <- match(raw$date,sort(unique(raw$date)))
  raw <- raw %>% filter(date>='2021-01-01')
  #Historical Data
  x0 <- raw %>% select(stock,date,did,open)
  for (i in 1:10){
    xi <- raw %>%
      mutate(did=did+i) %>%
      select(-stock)
    colnames(xi)[1:6] <- paste(colnames(xi)[1:6],i,sep='_')
    x0 <- x0 %>%
      merge(xi,by=c('did'))
  }
  X0 <- x0
  X <- X0[,!grepl('date_',colnames(X0))]
  #Profit Data
  y0 <- raw %>% select(stock,date,did,open)
  for (i in 1:15){
    yi <- raw %>%
      mutate(did=did-i) %>%
      select(did,date,open)
    colnames(yi)[-1] <- paste(colnames(yi)[-1],i,sep='_')
    y0 <- y0 %>%
      merge(yi,by=c('did'))
  }
  Y0 <- y0
  Y <- Y0[,!grepl('date_',colnames(Y0))]
  Y <- Y %>%
    select(did,stock,date) %>%
    mutate(profit=apply((Y[,-1:-4]),1,function(x){quantile(x,0.8)})/Y[,4])
  #Modeling
  modelfile <- X %>% merge(Y,by=c('did','stock','date'))
  train <- list(
    data = modelfile %>%
      select(-did,-date,-stock,-profit,-open) %>%
      as.matrix,
    label = modelfile %>%
      select(profit) %>%
      as.matrix
  )
  #Modeling
  model <- xgboost(data = as.matrix(train$data), label = train$label>1.01,
                   max.depth = 10, eta = 2, nthread = 2, nrounds = 100,
                   objective = "binary:logistic",verbose=0)
  plot.ts(model$evaluation_log$train_logloss)
  score <- predict(model,newdata=rbind(train$data,test$data))
  pred <- predict(model,newdata=X %>% select(-did,-date,-stock,-open) %>% as.matrix)
  pred <- cbind(X %>% select(did,stock,date),pred)
  #Resulting
  rlt <- modelfile %>%
    select(date,stock,open,profit) %>%
    mutate(score=score)
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
