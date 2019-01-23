
rm(list=ls())
library(data.table)
library(dplyr)
# setwd('/Users/wenrurumon/Downloads/data')
setwd("e:/fenglin/data")
files <- dir(pattern='temp')
files <- lapply(files,function(x){fread(x,encoding='UTF-8') %>% select(-V1)})

exe <- files[[1]]
fdata <- files[[2]]
idx <- files[[3]]
mdata <- files[[4]]
rate <- files[[5]]
rate <- mutate(rate,rate=((hour2-hour1)>=48)+1)

qpca <- function(A,rank=0){
  # A <- scale(A)
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}

###########################

base <- unique(select(fdata,pid,hours,orate))
fdata2 <- select(fdata,-pid)
fdata2.mean <- colMeans(fdata2)
fdata2.sd <- apply(fdata2,2,sd)
fdata2 <- qpca(scale(fdata2),14)$Z
for(i in 1:ncol(fdata2)){
  fdata2[,i] <- (fdata2[,i] * fdata2.sd[i]) + fdata2.mean[i]
  print(cor(fdata2[,i],fdata[,i+1,with=F]))
}
fdata <- as.data.frame(fdata)
colnames(fdata2) <- colnames(fdata)[-1]
fdata[,6:21] <- fdata2[,5:20]
fdata <- as.data.table(fdata)

sel1 <- merge(unique(select(idx,pid,hours)),
              select(base,pid,thours=hours),by=c('pid')) %>% mutate(
                idx = floor(hours/thours*100), check = (idx>100|idx<0)
              ) %>% filter(!check) %>% select(-hours,-thours,-check)
sel1 <- unique(paste(sel1$pid,sel1$idx))
sel2 <- merge(unique(select(exe,pid,hour1,hour2)),
              select(base,pid,thours=hours),by=c('pid')) %>% mutate(
                idx1 = floor(hour1/thours*100),idx2=ceiling(hour2/thours*100),
                idx3 = round((hour1+hour2)/2/thours*100,0),
                check = (idx2>100|idx1<0)
              )
sel2 <- unique(c(paste(sel2$pid,sel2$idx1),
                 paste(sel2$pid,sel2$idx2),
                 paste(sel2$pid,sel2$idx3)))
sel1 <- unique(c(sel1,sel2))
sel2 <- paste(fdata$pid,rep(0:100,length=nrow(fdata)))

########################

base <- unique(select(fdata,pid,orate,hours))
base %>% group_by(orate) %>% summarise(n(),mean(hours))
set.seed(12345); train <- sample(1:nrow(mdata))
train <- lapply(unique(cut(1:nrow(mdata),10)),function(i){
  train[cut(1:nrow(mdata),10)==i]
})
fdata.rate <- fdata$trate
fdata2 <- scale(select(fdata,-pid,-trate))
data2.x <- (data2.qpca <- qpca(fdata2,12))$X
colnames(data2.x) <- paste0('CS',1:ncol(data2.x))

data2.t <- fdata$trate
fdata2.x <- data.table(data2.x,trate=data2.t)
mdata2.x <- fdata2.x[sel2%in%sel1,]

model <- sapply(1:10,function(i){
  print(i)
  traindf <- as.data.frame(mdata2.x[train[[i]],])
  testdf <- as.data.frame(mdata2.x[-train[[i]],])
  # modeli <- glm(trate~.,data=traindf)
  # predi <- predict(modeli,newdata=as.data.frame(mdata.x),type='response')
  modeli <- MASS::lda(trate~.,data=traindf)
  predi <- predict(modeli,newdata=fdata2.x)$class
  print(table(predi=predi,trate=fdata2.x$trate))
  as.numeric(predi)-1
})
model <- rowMeans(model10 <- model)

# model10[!model10%in%c(0,2)] <- 1
# test <- rowMeans(model10)
# test[!test%in%c(0,2)] <- 1
#
# test <- table(predict=round(model,0)[sel2%in%sel1],actual=fdata.rate[sel2%in%sel1]);test;sum(diag(test))/sum(test)
# test <- table(predict=predict(MASS::lda(fdata.rate[sel2%in%sel1]~model10[sel2%in%sel1,]))$class,actual=fdata.rate[sel2%in%sel1]);test;sum(diag(test))/sum(test)
# test <- table(predict=round(model,0),actual=fdata.rate);test;sum(diag(test))/sum(test)
# model10.rate <- data.frame(model10,rate=fdata.rate)
# test <- predict(MASS::lda(rate~.,data=model10.rate[sel2%in%sel1,]),newdata=model10.rate)$class
# test <- table(predict=test,actual=fdata.rate);test;sum(diag(test))/sum(test)
# model10.rate <- data.frame(x=model,rate=fdata$rate)
# test <- predict(MASS::lda(rate~.,data=model10.rate[sel2%in%sel1,]),newdata=model10.rate)$class
# table(test[sel2%in%sel1],fdata.rate[sel2%in%sel1])
# test <- table(predict=test,actual=fdata.rate);test;sum(diag(test))/sum(test)
# fdata <- mutate(fdata,rate=as.numeric(test)-1)

test <- model
test[!test%in%c(0,2)] <- 1
table(predict=test[sel2%in%sel1],actual=fdata$trate[sel2%in%sel1])
table(predict=test,actual=fdata$trate)
fdata <- mutate(fdata,mrate=test)
fdata <- mutate(fdata,rate=ifelse(mrate>trate,mrate,trate))

(fdata %>% group_by(pid) %>% summarise(orate=mean(orate),rate=max(rate),trate=max(trate))) %>% group_by(
  orate,rate
) %>% summarise(n())

test <- apply(((fdata %>% group_by(pid) %>% summarise(orate=mean(orate),rate=max(rate),trate=max(trate))) %>% select(
  -pid
)),2,function(x){as.numeric(table(x))})
test / colSums(test)

table(predict=fdata$rate,raw=fdata$trate)

########################
# HMM
########################

data_hmm <- as.data.table(data2.x) %>% mutate(rate=fdata$orate)
test <-  data_hmm %>% group_by(pid=fdata$pid) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12),
  orate = mean(rate)
)
data_hmm <- data_hmm %>% group_by(pid=fdata$pid) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12),
  orate = (mean(rate)>0)+0
)

####

test <- test %>% as.data.frame()
rownames(test) <- test$pid
test <- select(test,-pid)
table(predict(MASS::lda(orate~.,data=test))$class,test$orate)

####

cbind(pvalue=apply(select(data_hmm,-pid,-orate),2,function(x){t.test(x~data_hmm$orate)$p.value}),
      thred=colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means})))

data_hmm.thred <- colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means}))
data_hmm <- as.data.frame(data2.x)
for(i in 1:ncol(data2.x)){data_hmm[,i] <- (data_hmm[,i] >= data_hmm.thred[i])+0}
data_hmm <- data.table(data_hmm,rate=fdata$rate,pid=fdata$pid)
data_hmm <- mutate(data_hmm,lrate=lag(rate,d1efault=0),lpid=lag(pid,default='0'))

# HMM input data
# data_hmm_back <- data_hmm
# data_hmm <- data_hmm_back
data_hmm <- filter(data_hmm,fdata$orate>0)
emisspr_ori <- data_hmm %>% group_by(rate) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12)
)
transpro <- filter(data_hmm,lpid==pid) %>% group_by(lrate,rate) %>% summarise(n=n())
startpro <- table(fdata$orate)/sum(table(fdata$orate))
list(round(emisspr_ori,2),transpro,startpro)

#########################
#Make it to daily
#########################

#按天给出状态与分数
data2day <- data.table(select(fdata,pid,hours,orate,trate),t=rep(0:100,length=nrow(data2.x)),data2.x
) %>% filter(orate>0) %>% mutate(
  day = floor(hours / 6 / 100*t)
) %>% group_by(pid,orate,day) %>% summarise(
  trate = max(trate),
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12)
) %>% as.data.frame()
for(i in 5:ncol(data2day)){data2day[,i] <- (data2day[,i] > data_hmm.thred[i-4])+0}

#不同状态下的单指标概率
summary2day <- data2day %>% group_by(trate) %>% summarise(
  n = n(),
  CS1=sum(CS1),CS2=sum(CS2),CS3=sum(CS3),CS4=sum(CS4),CS5=sum(CS5),CS6=sum(CS6),
  CS7=sum(CS7),CS8=sum(CS8),CS9=sum(CS9),CS10=sum(CS10),CS11=sum(CS11),CS12=sum(CS12)
)
summary2day <- rbind(as.data.frame(summary2day),colSums(summary2day)) %>% mutate(
  CS1 = CS1/n, CS2 = CS2/n, CS3 = CS3/n, CS4 = CS4/n, CS5 = CS5/n, CS6 = CS6/n, 
  CS7 = CS7/n, CS8 = CS8/n, CS9 = CS9/n, CS10 = CS10/n, CS11 = CS11/n, CS12 = CS12/n 
)

#不同状态下的多源指标
data2day <- as.data.table(data2day) %>% mutate(obs = paste(CS1,CS2,CS3,CS4,CS5,CS6,CS7,CS8,CS9,CS10,CS11,CS12),
                                               lpid=lag(pid,default='0'), lrate=lag(trate,default=0), check=(lpid==pid))
tag.obs <- unique(data2day$obs)
data2day <- data2day %>% mutate(obs = match(obs,tag.obs))

prob.hid2obs <- data2day %>% group_by(trate,obs) %>% summarise(n=n())
prob.hid2hid <- data2day %>% filter(check) %>% group_by(lrate,trate) %>% summarise(n=n())
prob.orihid <- data2day %>% filter(day==1) %>% group_by(trate) %>% summarise(n=n())

#主成分分析

# test <- (data2.qpca <- qpca(fdata2,12))$X
# write.csv(data2.qpca$Y,'25var_2_12cs.csv',row.names=F)

########################
# Processing
########################

# rm(list=ls())
library(data.table)
library(dplyr)
library(HMM)
# setwd('/Users/wenrurumon/Downloads/data')
# load('4hmm.rda')

state <- paste(unique(data2day$trate))
Symbols <- paste(unique(data2day$obs))
startprob <- prob.orihid$n

transpro <- do.call(cbind,lapply(0:2,function(i){
  filter(prob.hid2hid,lrate==i)$n
})); transpro
transpro <- transpro / rowSums(transpro)

emisspr <- do.call(rbind,lapply(0:2,function(i){
  x <- filter(prob.hid2obs,trate==i)
  x <- x$n[match(Symbols,x$obs)]
  x[is.na(x)] <- 0
  x/sum(x)
}))
system.time(hmm <- initHMM(States = state,Symbols = Symbols,startProbs = startprob,transProbs = transpro,
                           emissionProbs = emisspr))
pids <- unique(data2day$pid)

########################
# Modeling
########################

system.time(
  pred <- lapply(1:length(pids),function(j){
    # if(j%%1000==1){print(j)}
    oberi <- paste(filter(data2day,pid==pids[j])$obs)
    ratei <- paste(filter(data2day,pid==pids[j])$trate)
    data.table(pid=pids[j],trate=as.numeric(ratei),V1=as.numeric(viterbi(hmm,oberi)))
  })
)
itv_hmm <- do.call(rbind,pred)
print(mean(itv_hmm[,ncol(itv_hmm),with=F]==itv_hmm$trate))
data2day2 <- data2day %>% mutate(trate = itv_hmm$V1, lrate = lag(itv_hmm$V1,default=0))

prob.hid2obs2 <- data2day2 %>% group_by(trate,obs) %>% summarise(n=n())
prob.hid2hid2 <- data2day2 %>% filter(check) %>% group_by(lrate,trate) %>% summarise(n=n())
prob.orihid2 <- data2day2 %>% filter(day==1) %>% group_by(trate) %>% summarise(n=n())
state2 <- paste(unique(data2day2$trate))
Symbols2 <- paste(unique(data2day2$obs))
startprob2 <- prob.orihid2$n

transpro2 <- do.call(cbind,lapply(0:2,function(i){
  filter(prob.hid2hid2,lrate==i)$n
})); transpro2
transpro2 <- transpro2 / rowSums(transpro2)

emisspr2 <- do.call(rbind,lapply(0:2,function(i){
  x <- filter(prob.hid2obs2,trate==i)
  x <- x$n[match(Symbols,x$obs)]
  x[is.na(x)] <- 0
  x/sum(x)
}))

########################

out <- data.table((as.data.table(data2day2) %>% select(-obs,-lpid,-lrate,-check,-trate)),ori_rate=data2day$trate,exp_rate=itv_hmm$V1)
write.csv(out,'daily_final.out3',row.names=F)
write.csv(prob.hid2hid,'prob_hid2hid.out3',row.names=F)
write.csv(prob.hid2hid2,'prob_hid2hid2.out3',row.names=F)
write.csv(prob.hid2obs,'prob_hid2obs.out3',row.names=F)
write.csv(prob.hid2obs2,'prob_hid2obs2.out3',row.names=F)
write.csv(prob.orihid,'prob_orihid.out3',row.names=F)
write.csv(prob.orihid2,'prob_orihid2.out3',row.names=F)

out <- lapply(dir(pattern='out'),fread)
names(out) <- dir(pattern='out')

#########################

plot.ts(x <- filter(out,pid=='04770BDB6414A34743244C6D20A43A70')$exp_rate)
ret <- function(x,ret=0.3){
  x.ori <- x
  for(i in 2:length(x)){
    x[i] <- x[i-1] * ret + x[i]
  }
  x
}
ret2 <- function(x,ret2=0.3){
  x1 <- x
  x2 <- ret(x[length(x):1],ret2)[length(x):1]+ret(x,ret2)
  # x2 * max(x1)/max(x2)
  x2 <- x2 * sum(x1)/sum(x2)
  sel <- rowMeans(cbind(lag(x1),x1,lead(x1)),na.rm=T)==x1
  x2[sel] <- x1[sel]
  x2
}

i <- 0
#15
pred_area <- t(sapply(pids,function(pidi){
  print(i<<-i+1)
  x <- filter(out,pid==pidi)$exp_rate
  x <- (x==0)*0.505 + (x==1)*0.261 + (x==2) * 0.114
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  f.y <- ret2(x)
  if(length(f.y)<101){
    f.x <- filter(out,pid==pidi)$day * 6
    f.x[length(f.x)] <- x.hour/100
  } else {
    f.x <- 0:(length(f.y)) * (x.hour/(length(f.y))) / 100
    f.x <- f.x[-length(f.x)]
  }
  f <- splinefun(f.x,f.y)
  f <- f((0:x.hour)*length(x)/x.hour)
  # plot((1:x.hour)*length(x)/x.hour,f,type='l',col=2)
  # lines(1:length(x),ret2(x))
  c(orate=x.rate,hour=x.hour/100,area=sum(f))
}))
pred_area <- data.table(pid=pids,pred_area)

head(base)
base_area <- base %>% filter(orate==0) %>% select(pid,orate,hour=hours) %>% mutate(
  orate = 0, hour = floor(hour*100)/100, area = hour * 0.505*100
)

area <- rbind(as.data.table(base_area),as.data.table(pred_area)) %>% mutate(
  avg_area = area/hour
)

write.csv(transpro2,'transpro2.out3',row.names=F)
write.csv(transpro,'transpro1.out3',row.names=F)
write.csv(emisspr2,'emisspr2.out3',row.names=F)
write.csv(emisspr,'emisspr.out3',row.names=F)
write.csv(cbind(tag.obs),'tag_obs.out3',row.names=F)

#######################
# 
# filter(fdata,pid=='03E0A279EDAA8EFF524B096510E19C90')
# filter(rate,pid=='03E18129A221095A84DF35C3287A452A')

i <- 0
area2 <- function(pidi){
  # print(i<<-i+1)
  # print(pidi)
  x <- filter(out,pid==pidi)$exp_rate
  x <- (x==0)*0.505 + (x==1)*0.261 + (x==2) * 0.114
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  x.int <- filter(out,pid==pidi)$day
  f.x <- x.int*6
  f.y <- ret2(x)
  # plot(f.x,f.y,type='l')
  f <- splinefun(f.x,f.y)
  f <- f(s <- (0:ceiling(x.hour/100)))
  f <- ifelse(f>0.505,0.505,ifelse(f<0.114,0.114,f))
  # lines(s,f,type='l',col=2)
  f
}
area2 <- lapply(pids,area2)

area2_2hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/2),each=2)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))
area2_6hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/6),each=6)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))
area2_24hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/24),each=24)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))
write.csv(cbind(pid=pids,area=area2_2hour),'area_2hour.out3',row.names=F)
write.csv(cbind(pid=pids,area=area2_6hour),'area_6hour.out3',row.names=F)
write.csv(cbind(pid=pids,area=area2_24hour),'area_24hour.out3',row.names=F)

status2 <- function(pidi){
  # print(i<<-i+1)
  # print(pidi)
  x <- filter(out,pid==pidi)$exp_rate
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  x.int <- filter(out,pid==pidi)$day
  f.x <- x.int*6
  f.y <- ret2(x)
  # plot(f.x,f.y,type='l')
  f <- splinefun(f.x,f.y)
  f <- f(s <- (0:ceiling(x.hour/100)))
  f <- ifelse(f>2,2,ifelse(f<0,0,f))
  # lines(s,f,type='l',col=2)
  f
}
status2 <- lapply(pids,status2)
status2_2hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/2),each=2)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))
status2_6hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/6),each=6)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))
status2_24hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/24),each=24)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))
write.csv(cbind(pid=pids,status=status2_2hour),'status_2hour.out3',row.names=F)
write.csv(cbind(pid=pids,status=status2_6hour),'status_6hour.out3',row.names=F)
write.csv(cbind(pid=pids,status=status2_24hour),'status_24hour.out3',row.names=F)

# plot.ts(filter(out,pid=='803B7A658FF91E58945DD8D8A7068F1A')$exp_rate)
# plot.ts(status2[pids=='803B7A658FF91E58945DD8D8A7068F1A'][[1]])
# 
# 
# filter(rate,pid=='00CAB3F161BE056E0C59B547D1A12558')
# filter(base,pid=='00CAB3F161BE056E0C59B547D1A12558')
# filter(out,pid=='00CAB3F161BE056E0C59B547D1A12558')$ori_rate

