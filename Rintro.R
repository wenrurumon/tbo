# Agenda of R Intriduction
# 1. GUI and Hello World
# 2. Data Type
# 3. Functional Programming
# 4. condition and loop
# 5. linear regression
# 6. data table, dplyr and segmentation
#

#GUI
#Syntax and Console
#Ctrl L, Ctrl Enter
print("Hello World")
"Hello World"
1+1
pi^120
cos(20)
1;2;3;4;5

#Asign a variable
x <- 10; print(x)
20 -> 20; print(x)
x = 30; print(x)

#Data Type
#Numerical Vector
1;2;3
1:3
c(1,2,3)
rep(c(1,2,3),2)
age <- rep(c(1,2,3),each=2)

#Char
'dog'; 'cat'
c('dog','cat')
animal <- rep(c('dog','cat'),3)

#Matrix
test <- matrix(1:6,ncol=2,nrow=3); test
t(test)
#combine by col or row
cbind(test,test+1)
rbind(test,test+1)
cbind(animal=animal,age=age)
cbind(animal,age)

#List and data frame
list(animal=animal,age=age)
data.frame(animal=animal,age=age)

#Functional Programming
sum(1,2)
max(1,2,3,4,5)
sum2 <- function(a,b){
  return(a+b)
}
sum2(1,2)

#Condition and Loops
if(2>1){print('2>1')}else{print('1>2')}
ifelse(2>1,'2>1','1>2')

rlt.sum = 0
for(i in 1:10){
  rlt.sum = rlt.sum + i
}
print(rlt.sum)

rlt.sum = 0
i = 0
while(i<10){
  i = i+1
  rlt.sum = rlt.sum + i
}
rlt.sum

#Lieanr Regression
x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)
x4 = rnorm(100)
error = rnorm(100)/10
y = x1+2*x2+3*x3+error
(x.lm <- lm(y~x1+x2+x3+x4))
names(x.lm)

x.lm$coefficients
x.lm$residuals
mean(abs(x.lm$residuals)/y) #model mape

summary(x.lm)
x.lm2 <- lm(y~-1+x1+x2+x3)
summary(x.lm2)

#dplyr and segmentation

data(iris)
data <- iris
library(dplyr)
iris %>% group_by(Species) %>% summarise(mean(Sepal.Length))
iris %>% group_by(Species) %>% summarise(m1=mean(Sepal.Length),m2=mean(Sepal.Width),
                                         m3=mean(Petal.Length),m4=mean(Petal.Width))
filter(iris,Species!='setosa') %>% group_by(Species) %>% summarise(mean(Sepal.Length))

#Build a list for marketing mix

tv = abs(rnorm(100))
otv = abs(rnorm(100))
tpr = abs(rnorm(100))
base = abs(rnorm(100))
error = rnorm(100)/10
data <- cbind(1,tv,otv,tpr,base); head(data)
y = 3+1*tv + 2*tpr+2*otv+3*news+4*base+error
(x.lm <- lm(y~tv+otv+tpr+base))
x.lm$coefficients[]
x.lm$coefficients[2]

#Calculate the fit chart
y.fit <- 0
for(i in 1:length(x.lm$coefficients)){
  y.fit <- y.fit + x.lm$coefficients[i] * data[,i]
}
plot.ts(y.fit,col=2); lines(y)

#Calculate the contribution
contribution <- c()
for(i in 1:length(x.lm$coefficients)){
  contribution[i] <- sum(x.lm$coefficients[i] * data[,i])/sum(y)
}

#generate result
rlt <- list()
rlt$fitchart = cbind(raw=y,fit=y.fit)
rlt$coef = x.lm$coefficients
rlt$contribution = contribution
