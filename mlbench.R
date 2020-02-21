
rm(list=ls())
library(mlbench)

data("Ozone")
data <- na.omit(Ozone)

set.seed(1)

rlt <- lapply(1:1000,function(i){
  print(i)
  set.seed(i)
  train_row <- sample(nrow(data), nrow(data)/2)
  #train
  train <- data[train_row, ]
  #test
  test  <- data[-train_row, ]
  #model
  train <- apply(train,2,as.numeric) %>% as.data.frame
  test <- apply(test,2,as.numeric) %>% as.data.frame
  model <- lm(V4~.,data=train)
  #Step by step modeling
  y <- train[,4]
  x.pool <- train[,-4]
  x.in <- cbind()
  while(ncol(x.pool)>0){
    x.sel <- apply(x.pool,2,function(x){mean((predict(lm(y~x))-y)^2)})
    x.sel <- which.min(x.sel)
    x.in <- cbind(x.in,x.pool[,x.sel])
    x.pool <- x.pool[,-x.sel,drop=F]
  }
  x.in <- as.matrix(x.in)
  train.mse <- sapply(1:12,function(i){
    mean((y-predict(lm(y~x.in[,1:i,drop=F])))^2)
  }) %>% sqrt
  x.sel <- match(apply(x.in,2,var),apply(train,2,var))
  #Validation
  test.mse <- sapply(1:length(x.sel),function(i){
    mean((test$V4-predict(lm(test$V4~as.matrix(test)[,x.sel[1:i]])))^2)
  }) %>% sqrt
  #result
  rlt <- cbind(x.sel,train.mse,test.mse)
  rlt <- cbind(rlt,loss=sqrt(mean((y-mean(y))^2))-rlt[,2])
  rlt <- cbind(rlt,margin_loss=c(0,diff(rlt[,4])))
  rlt <- cbind(rlt,avg_loss=(rlt[,4]-min(rlt[,4]))/(1:nrow(rlt)))
  list(rlt=rlt,rmse=rlt[which.max(rlt[,6]),3])
})
out.rmse <- sapply(rlt,function(x){x$rmse})
hist(out.rmse)
out.sel <- sapply(rlt,function(x){which.max(x$rlt[,6])})
hist(out.sel)
