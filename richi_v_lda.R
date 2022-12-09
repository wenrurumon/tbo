
rm(list=ls())
library(dplyr)
library(ggplot2)
library(openxlsx)
setwd('/Users/wenrurumon/Documents/training/1112/rich/old')

####################################
# Rbasic
####################################

"600418.xlsx" %>% read.xlsx %>% head
"600418.xlsx" %>% read.xlsx %>% select(时间,开盘) %>% head
"600418.xlsx" %>% read.xlsx %>% filter(开盘<1) %>% head
"600418.xlsx" %>% read.xlsx %>% mutate(test=开盘<1) %>% head
"600418.xlsx" %>% read.xlsx %>% arrange(-开盘) %>% head
merge

match(
  c(1,2,3,4,5,8,9,10,11,12,14,15,16,17,18),
  c(1,2,3,4,5,8,9,10,11,12,14,15,16,17,18)
)

paste0(123,'#$&@*#&$(*@#&$',456)

####################################
# Data
####################################

#Import Data
stocki <- "埃斯顿.xlsx"
raw <- stocki %>%
  read.xlsx %>%
  select(date=1,open=2,high=3,low=4,end=5,value=9) %>%
  mutate(date=substr(date,1,10)) %>%
  mutate(date=as.Date(date)) %>%
  filter(date>='2020-01-01')
raw$value <- as.numeric(raw$value)

#有哪几天跳空高开

(raw %>% 
    select(date,low0=low,high0=high)) %>%
  merge((raw %>% 
           select(date,low1=low,high1=high)) %>%
          mutate(date=date+1),
        by='date') %>% filter(low0>high1)

#Why did

raw <- raw %>% arrange(date)
raw$did <- match(raw$date,raw$date)

############################################
#Jump
############################################

#X

X <- raw %>% 
  select(did,open) %>%
  merge(raw %>% 
          select(did,low1=low,high1=high,
                 open1=open,end1=end,value1=value) %>%
          mutate(did=did+1),
        by='did') %>% 
  merge(raw %>% 
          select(did,low2=low,high2=high,
                 open2=open,end2=end,value2=value) %>%
          mutate(did=did+2),
        by='did')

#Y
Y <- raw %>% 
  select(did,date,open) %>%
  merge(
    raw %>%
      select(did,date,open) %>%
      mutate(did=did-10),
    by='did'
  ) %>%
  mutate(profit=open.y/open.x) %>%
  select(did,profit)

X %>% 
  merge(Y,by='did') %>%
  group_by(jump=(low1>high2),
           trend=(end1>open1),
           value=(value1/value2)>2) %>%
  summarise(test=median(profit),n()) %>%
  arrange(-test)

##########
#KDJ

X <- raw %>%
  mutate(rsv = (end-low)/(high-low)*100) %>%
  mutate(rsv=ifelse(is.na(rsv),0,rsv)) %>%
  mutate(k=50,d=50)
for(i in 2:nrow(X)){
  X$k[i] <- 2/3*X$k[i-1] + 1/3*X$rsv[i]
  X$d[i] <- 2/3*X$d[i-1] + 1/3*X$k[i]
}
X <- X %>% mutate(j=3*k-2*d)

kdj <- X %>%
  select(did,k1=k,d1=d,j1=j) %>%
  merge(
    X %>%
      mutate(did=did+1) %>%
      select(did,k0=k,d0=d,j0=j),
    by='did'
  ) %>% 
  mutate(status1=((j0<k0)+(j0<d0)+(j1>k1)+(j1>d1)+(k0<d0)+(k1>d1)),
         status2=(j0>k0)+(k0>d0))
kdj$buy <- 0
for(i in 5:nrow(kdj)){
  kdj$buy[i] <- ifelse((kdj$status1[i-4]==6)&
                         (mean(kdj$status2[i-1:3]==2)==1),1,0)
}

log <- data.table(
  buy=which(kdj$buy==1),
  sell=sapply(which(kdj$buy==1),function(i){
    which(kdj$j0[i:nrow(kdj)]<0)[1]+i-1
  })
) %>%
  merge(raw %>% select(buy=did,buyprice=open),by='buy') %>%
  merge(raw %>% select(sell=did,sellprice=open),by='sell') %>%
  mutate(profit=sellprice/buyprice)
log %>% summarise(mean(sell-buy,na.rm=T))

Y <- raw %>% 
  select(did,date,open) %>%
  merge(
    raw %>%
      select(did,date,open) %>%
      mutate(did=did-50),
    by='did'
  ) %>%
  mutate(profit=open.y/open.x) %>%
  select(did,profit)

dim(log)
summary(Y$profit)
summary(log$profit,na.rm=T)

############################################

#X
X <- raw %>% select(did,open)
for(i in 1:5){
  xi <- raw %>% 
    select(-date) %>%
    mutate(did=did+i)
  colnames(xi)[1:5] <- paste0(colnames(xi)[1:5],"_",i)
  X <- X %>% merge(xi,by='did')
}

#Y
Y <- raw %>% 
  select(did,date,open) %>%
  merge(
    raw %>%
      select(did,date,open) %>%
      mutate(did=did-30),
    by='did'
  ) %>%
  mutate(profit=open.y/open.x) %>%
  select(did,profit)

#Mfile
modelfile <- Y %>% merge(X,by='did')
modelfile %>%
  ggplot() + 
  geom_point(aes(x=did,y=open,colour=(profit>1.2))) +
  theme_bw()

####################################
# Classification
####################################

#Modeling
model0 <- MASS::lda(
  (profit>1.1)~.,
  data=modelfile %>% select(-did)
)

#rlt
rlt0 <- modelfile %>% 
  mutate(buy=predict(model0)$class,score=predict(model0)$posterior[,2])

#Plot
rlt0 %>%
  ggplot() + 
  geom_point(aes(x=did,y=open,colour=profit>1)) +
  theme_bw()

rlt0 %>%
  ggplot() + 
  geom_point(aes(x=did,y=open,colour=buy)) +
  theme_bw()

rlt0 %>%
  ggplot() + 
  geom_point(aes(x=did,y=open,colour=score>0.6)) +
  theme_bw()

####################################
# Train and Test
####################################

#Training
model1 <- MASS::lda(
    (profit>1)~.,
    data=modelfile %>% filter(did<487) %>% select(-did)
)
rlt1 <- cbind(modelfile,predict(model1,newdata=modelfile %>% select(-did))$posterior[,2])

rlt1 <- modelfile %>%
  mutate(buy=predict(model0,newdata=modelfile)$class,
         score=predict(model0,newdata=modelfile)$posterior[,2])

#Test
rlt1 %>% filter(did>=487) %>%
  ggplot() +
  geom_point(aes(x=score,y=profit))

rlt1 %>%
  group_by(score>0.6) %>%
  summarise(profit=mean(profit),n=n()) %>%
  mutate((n*profit-n*0.001-n)/n)

####################################
# Train, Validate and Test
####################################

#Validate
rlt1 %>% filter(did>=487,did<587) %>%
  ggplot() +
  geom_point(aes(x=score,y=profit))

#Test
rlt1 %>%
  filter(did>=587) %>%
  group_by(score>0.35) %>%
  summarise(profit=mean(profit),n=n()) %>%
  mutate((n*profit-n*0.001-n)/n)

####################################
# Modeling
####################################

library(xgboost)

#X
X <- raw %>% select(did,open)
for(i in 1:10){
  xi <- raw %>% 
    select(-date) %>%
    mutate(did=did+i)
  colnames(xi)[1:5] <- paste0(colnames(xi)[1:5],"_",i)
  X <- X %>% merge(xi,by='did')
}

#Zscore the outcome

h <- 30
Z <- t(sapply(1:(max(raw$did)-(h-1)),function(i){
  rawi <- raw %>% 
    filter(did>=i,did<=i+(h-1))
  c(did=i,
    z=(rawi$open[1]-mean(rawi$open))/sd(rawi$open),
    profitavg=(mean(rawi$open[-1])/rawi$open[1]),
    profitmax=(max(rawi$open)/rawi$open[1]),
    countlower=mean(rawi$open<rawi$open[1]),
    counthigher=mean(rawi$open>rawi$open[1]))
})) %>%
  as.data.frame

#ModelFile
modelfile2 <- X %>% merge(Z,by='did')
train <- list(
  data = modelfile2 %>%
    filter(did<487) %>%
    select(-did,-z,-profitavg,-profitmax,-countlower,-counthigher) %>%
    as.matrix,
  label = modelfile2 %>%
    filter(did<487) %>%
    select(z,profitavg,profitmax,countlower,counthigher) %>%
    as.matrix
)
test <- list(
  data = modelfile2 %>%
    filter(did>=487) %>%
    select(-did,-z,-profitavg,-profitmax,-countlower,-counthigher) %>%
    as.matrix,
  label = modelfile2 %>%
    filter(did>=487) %>%
    select(z,profitavg,profitmax,countlower,counthigher) %>%
    as.matrix
)

models <- lapply(1:(ncol(Z)-1),function(i){
  xgboost(data = as.matrix(train$data), 
          label = train$label[,i],
          max.depth = 5, eta = 2, nthread = 2, nrounds = 200,
          objective = "reg:squarederror",verbose=1)
})
names(models) <- colnames(Z)[-1]
plot.ts(sapply(models,function(i){i$evaluation_log$train_rmse}))
rlt <- sapply(models,function(model){
  predict(model,newdata=rbind(train$data,test$data))
})
colnames(rlt) <- paste0(colnames(rlt),'_pred')
test <- cbind(modelfile2,rlt)
test %>%
  group_by(group=ifelse(did>=487,'Test','Train'),zscore=sign(z_pred)*(abs(z_pred)>1.96)) %>%
  summarise(n(),mean(profitavg),mean(profitmax),mean(countlower),mean(counthigher))
