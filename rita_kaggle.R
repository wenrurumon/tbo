
rm(list=ls())
library(data.table)
library(dplyr)
setwd("/Users/wenrurumon/Documents/temp")

pca <- function(X,ifscale=T){
  if(ifscale) {X <- scale(as.matrix(X))}
  m = nrow(X)
  n = ncol(X)
  X = scale(X)
  Xeigen <- svd(as.matrix(X))
  value <- (Xeigen$d)^2/m
  value <- cumsum(value/sum(value))
  score <- X %*% Xeigen$v
  mat <- Xeigen$v
  list(score=score,prop=value,mat=mat)
}

#Input data
train <- data.frame(class='train',fread('train.csv'))
train <- train[,-ncol(train)]
test <- data.frame(class='test',fread('test.csv'))
variables <- fread('field_descriptions.txt')
variables <- variables[-nrow(variables),]
variables <- data.table(variables,code=paste0('V',1:nrow(variables)))
colnames(test) <- colnames(train) <- c('class',variables$code)
dataset <- rbind(train,test) %>% as.data.frame

#Rough Process: Convert No/Yes to 0/1
dataset[dataset=='No'] <- 0
dataset[dataset=='Yes'] <- 1

#Rough Process: Convert ?/NA aligned with the overall data distribution
dataset[dataset=='?'] <- NA
for(i in which(colSums(is.na(dataset))>0)){
  xi <- dataset[,i]
  dataset[is.na(dataset[,i]),i] <- sample(xi[!is.na(xi)],sum(is.na(xi)),replace=T)
}
colnames(dataset)[ncol(dataset)] <- 'y'
dataset$y <- ifelse(dataset$class=='test',NA,dataset$y)

#Data Description
data.summary <- cbind(
  variable=colnames(dataset)[-1:-2],
  type=apply(dataset[,-1:-2],2,function(x){
    x <- sum(as.numeric(x))
    x==ceiling(x)
  }),
  t(apply(dataset[,-1:-2],2,function(x){
    c(all.nclass=length(unique(x)),
      all.entropy=-sum((table(x)/length(x))*log(table(x)/length(x))))
  })),
  t(apply(filter(dataset,class=='train')[,-1:-2],2,function(x){
    c(train.nclass=length(unique(x)),
      train.entropy=-sum((table(x)/length(x))*log(table(x)/length(x))))
  }))
) %>% as.data.table %>%
  mutate(type=ifelse(is.na(type),'Categorical',ifelse(type,'Categorical','Continues')))

#Rough Feature Selection, Filter those features with limited variation
variable2test <- data.summary %>% 
  filter(train.nclass>1&all.entropy>.1&train.entropy>0.1)
data.summary <- filter(data.summary,variable%in%variable2test$variable)
dataset2 <- dataset[,colnames(dataset)%in%data.summary$variable]

#Convert Categorical variables to Boolean
temp <- lapply(1:nrow(data.summary),function(i){
  if(data.summary$type[i]=='Categorical'){
    code <- (paste0(data.summary$variable[i],":",unique(dataset2[,i])))
    x <- outer(dataset2[,i],unique(dataset2[,i]),'==')+0
    colnames(x) <- code
    x
  } else {
    x <- cbind(as.numeric(dataset2[,i]))
    colnames(x) <- data.summary$variable[i]
    x
  }
})
names(temp) <- data.summary$variable
temp <- temp[-length(temp)]

#Feature Selection
dataset2 <- do.call(cbind,temp)
train <- dataset2[dataset$class=='train',,drop=F]
test <- dataset2[dataset$class=='test',,drop=F]
test <- test[,!colnames(test)%in%names(which(apply(train,2,var)==0))]
train <- train[,!colnames(train)%in%names(which(apply(train,2,var)==0))]
Y <- dataset$y[dataset$class=='train']
colnames(train) <- colnames(test) <- NULL
data.pca <- pca(rbind(train,test))
modelfile <- data.pca$score[,1:1:which(data.pca$prop>=0.8)[1],drop=F]
train2 <- as.data.frame(modelfile[dataset$class=='train',,drop=F])
test2 <- as.data.frame(modelfile[dataset$class=='test',,drop=F])

#Random Prediction
benchmark <- sapply(1:1000,function(i){mean(sample(Y)==Y)})
mean(benchmark)
hist(benchmark)

#KNN
model.knn <- knn(train,test,as.factor(Y),k=3,prob=T)
model.knn2 <- knn(train2,test2,as.factor(Y),k=3,prob=T)

#Multi Logistic Regression
model.logistic <- sapply(1:length(unique(Y)),function(i){
  modelfilei <- data.frame(train2,Y=(Y==unique(Y)[i])+0)
  modeli <- glm(Y~.,data=modelfilei,family='binomial')
  c(predict(modeli,type='response'),
    predict(modeli,newdata=test2,type='response'))
})
mean(c(Y,rep(NA,nrow(test)))==unique(Y)[
  apply(model.logistic,1,which.max)],na.rm=T)

#LDA
model.lda <- MASS::lda(y~.,data=data.frame(train2,y=Y))
mean(predict(model.lda)$class==Y)


