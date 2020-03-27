
rm(list=ls())
setwd("/Users/wenrurumon/Documents/mindshare/yinlu")
library(data.table)
library(dplyr)
raw <- read.csv('datafile.csv')
raw$MONTH <- sapply(strsplit(paste(raw$MONTH),'/'),function(x){
  as.numeric(x[1])*100+as.numeric(x[2])
})
raw$year <- floor(raw$MONTH/100)

##############################################
# Module
##############################################

check <- function(x,by=raw$MONTH,fun=plot.ts,col=1,iftext=TRUE){
  x <- tapply(x,by,sum)
  fun(as.numeric(x),col=col)
  if(iftext){(text(1:length(x),as.numeric(x),names(x)))}
  return(x)
}
ret <- function(x,ret=0.3){
  rlt <- x
  for(i in 2:length(x)){
    rlt[i] <- rlt[i] + rlt[i-1]*ret
  }
  rlt
}

##############################################
# Yinyu
##############################################

map <- 'Var	Spending	ROI	Pie	Ret
Yinlu_BBZ.vol	NA	NA	NA	0.000000
Yinlu_BBZ.avp	NA	NA	NA	0.000000
Yinlu_BBZ.wd	NA	NA	NA	0.000000
Jishi_total.vol	NA	NA	NA	0.000000
Wahaha_total.vol	NA	NA	NA	0.000000
BBZ_total.grp	30728419.629000	1.500000	0.007901	0.300000
HZD_total.grp	77331196.863419	0.200000	0.002651	0.300000
BBZ_OTV.igrp	20484008.019835	1.500000	0.005267	0.300000
HZD_OTV.igrp	32005424.408000	0.200000	0.001097	0.300000
HZD_Digital_dispaly.imp	3836721.000000	0.200000	0.000132	0.300000
BBZ_ooh.mspd	855000.000000	1.500000	0.000220	0.300000
ZYZ_total.grp	7710004.340775	0.100000	0.000132	0.300000
ZYZ_OTV.igrp	13237914.969744	0.100000	0.000227	0.300000
ZYZ_Digital_display.imp	770000.000000	0.100000	0.000013	0.300000
JPZ_APP.imp	14653369.567873	0.050000	0.000126	0.300000
BBZ_DM.discount	90980566.520000	1.600000	0.024954	0.000000
BBZ_paid.display	257618349.266000	1.600000	0.070660	0.000000
BBZ_consumer.promo	61849014.920402	2.000000	0.021205	0.000000
BBZ_tonglu.promo	126757913.730000	1.800000	0.039113	0.300000
BBZ_gxgc.promo	16303501.000000	2.000000	0.005590	0.000000
HZD_OTT.imp	4269979.000000	0.200000	0.000146	0.300000
ZYZ_OTT.imp	1620518.000000	0.100000	0.000028	0.300000
ZYZ_OTVZC.imp	7719300.000000	0.100000	0.000132	0.300000'
map <- do.call(rbind,strsplit(strsplit(map,'\n')[[1]],'\t'))

mfile <- select(raw
                ,Province
                ,Yinlu_BBZ.vol
                ,Yinlu_BBZ.avp
                ,Yinlu_BBZ.wd
                ,Jishi_total.vol
                ,Wahaha_total.vol
                ,BBZ_total.grp
                ,HZD_total.grp
                ,BBZ_OTV.igrp
                ,HZD_OTV.igrp
                ,HZD_Digital_dispaly.imp
                ,BBZ_ooh.mspd
                ,ZYZ_total.grp
                ,ZYZ_OTV.igrp
                ,ZYZ_Digital_display.imp
                ,JPZ_APP.imp
                ,BBZ_DM.discount
                ,BBZ_paid.display
                ,BBZ_consumer.promo
                ,BBZ_tonglu.promo
                ,BBZ_gxgc.promo
                ,HZD_OTT.imp
                ,ZYZ_OTT.imp
                ,ZYZ_OTVZC.imp)
hh <- rep(tapply(mfile$Jishi_total.vol,mfile$Province,mean),each=36)/100
mfile$Yinlu_BBZ.vol <- mfile$Yinlu_BBZ.vol/hh
mfile$Jishi_total.vol <- mfile$Jishi_total.vol/hh
mfile$Wahaha_total.vol <- mfile$Wahaha_total.vol/hh
mfile <- lapply(unique(mfile$Province),function(p){
  x <- filter(mfile,Province==p) %>% select(-Province)
  for(i in 1:ncol(x)){
    x[,i] <- ret(x[,i],as.numeric(map[grep(colnames(x)[i],map[,1]),5]))
  }
  x
})
mfile <- do.call(rbind,mfile)
model.coef <- apply(mfile,2,function(x){sum(x*hh)})/apply(mfile,2,function(x){sum(x*hh)})[1]
model.coef <- as.numeric(map[match(names(model.coef),map[,1]),4])/model.coef
for(i in 1:length(model.coef)){
  mfile[,i] <- mfile[,i] * ifelse(is.na(model.coef),1,model.coef)[i]
}
mfile1 <- mfile[,is.na(model.coef)]
mfile2 <- mfile[,!is.na(model.coef)]
dummy <- (outer(raw$Province,unique(raw$Province),'=='))+0
colnames(dummy) <- unique(raw$Province)
dummy2 <- (outer(substr(raw$MONTH,5,6),unique(substr(raw$MONTH,5,6)),'=='))+0
colnames(dummy2) <- unique(substr(raw$MONTH,5,6))
dummy <- cbind(dummy,dummy2[,-12])
mfile1 <- cbind(mfile1,dummy)
res <- mfile1$Yinlu_BBZ.vol - rowSums(mfile2)
model <- lm(res~as.matrix(mfile1[,-1])-1)
plot.ts(as.numeric(tapply((rowSums(mfile2)+res)*hh,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=2)

#Optimization 1
b0 <- rep(1,ncol(mfile2))
loss <- function(b){
  l1 <- sum((mfile1$Yinlu_BBZ.vol - colSums(t(mfile2)*b) - predict(model))^2)/sum((mfile1$Yinlu_BBZ.vol - colSums(t(mfile2)*b0) - predict(model))^2)
  l2 <- mean((b-b0)^2)
  l1+l2
}
b1 <- optim(b0,loss,control=list(maxit=10000))[[1]]
for(i in 1:ncol(mfile2)){mfile2[,i] <- mfile2[,i] * b1[i]}
res <- mfile1$Yinlu_BBZ.vol - rowSums(mfile2)
model <- lm(res~as.matrix(mfile1[,-1])-1)
# plot.ts(as.numeric(tapply((rowSums(mfile2)+res)*hh,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=4)

#Optimization 2
res <- res - predict(model)
mfile2.coef <- apply(1+apply(mfile2 / rowSums(mfile2) * res,2,function(x){
  tapply(x,rep(rep(1:3,each=12),24),sum)
})/apply(mfile2,2,function(x){
  tapply(x,rep(rep(1:3,each=12),24),sum)
}),2,function(x){
  return(x)# rep(rep(x,each=12),3)
})
b0 <- ifelse(is.na(mfile2.coef),1,mfile2.coef)
b0 <- as.vector(b0)
loss <- function(b){
  b2 <- matrix(b,nrow=3)
  b2 <- apply(b2,2,function(x){rep(rep(x,each=12),3)})
  l1 <- sum((mfile2*b2)^2)/sum((mfile2*b0)^2)
  l2 <- sum((b-b0)^2)
  l1+l2
}
system.time(b1 <- optim(b0,loss,control=list(maxit=10000))[[1]])
b1 <- apply(matrix(b1,nrow=3),2,function(x){rep(rep(x,each=12),3)})
mfile2 <- mfile2 * b1
# mfile2.coef <- apply(1+apply(mfile2 / rowSums(mfile2) * res,2,function(x){
#   tapply(x,rep(rep(1:3,each=12),24),sum)
# })/apply(mfile2,2,function(x){
#   tapply(x,rep(rep(1:3,each=12),24),sum)
# }),2,function(x){
#   rep(rep(x,each=12),3)
# })
# mfile2 <- ((mfile2.coef-1)/2+1) * mfile2
mfile2[is.na(mfile2)] <- 0
res <- mfile1$Yinlu_BBZ.vol - rowSums(mfile2)
mfile1 <- as.matrix(mfile1[,-1])
model <- lm(res~mfile1-1)
plot.ts(as.numeric(tapply(raw$Yinlu_BBZ.vol,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=2)

decomp <- apply(cbind(t(t(mfile1) * coef(model)),mfile2),2,function(x){
  tapply(x*hh,raw$MONTH,sum)
})
decomp <- data.frame(decomp,
                actual=as.numeric(tapply(raw$Yinlu_BBZ.vol,raw$MONTH,sum)),
                predict=rowSums(decomp))
plot.ts(decomp$actual); lines(decomp$predict,col=2)
write.csv(data.frame(month=unique(raw$MONTH),decomp),'decomp1.csv')
write.csv(apply(decomp,2,function(x){
  tapply(x,rep(1:3,each=12),sum)
}),'summary1.csv')

##############################################
# Yinyu HZD
##############################################

map <- 'Var	Spending	ROI	Pie	Ret
Yinlu_HZD.vol	NA	NA	NA	0.000000
Yinlu_HZD.avp	NA	NA	NA	0.000000
Yinlu_HZD.wd	NA	NA	NA	0.000000
Jishi_total.vol	NA	NA	NA	0.000000
Yinlu_BBZ.vol	NA	NA	NA	0.000000
Wahaha_total.vol	NA	NA	NA	0.000000
BBZ_total.grp	30728419.629000	0.200000	0.001877	0.300000
HZD_total.grp	77331196.863419	1.500000	0.035429	0.300000
BBZ_OTV.igrp	20484008.019835	0.200000	0.001251	0.300000
HZD_OTV.igrp	32005424.408000	1.500000	0.014663	0.300000
HZD_Digital_dispaly.imp	3836721.000000	1.500000	0.001758	0.300000
BBZ_ooh.mspd	855000.000000	0.200000	0.000052	0.300000
ZYZ_total.grp	7710004.340775	0.100000	0.000235	0.300000
ZYZ_OTV.igrp	13237914.969744	0.100000	0.000404	0.300000
ZYZ_Digital_display.imp	770000.000000	0.100000	0.000024	0.300000
JPZ_APP.imp	14653369.567873	0.050000	0.000224	0.300000
HZD_DM.discount	63046156.230000	1.600000	0.030810	0.000000
HZD_paid.display	207596079.272000	1.600000	0.101450	0.000000
HZD_consumer.promo	73899599.233989	2.000000	0.045143	0.300000
HZD_tonglu.promo	40386904.070000	1.800000	0.022204	0.300000
HZD_gxgc.promo	9575484.000000	2.000000	0.005849	0.000000
HZD_OTT.imp	4269979.000000	1.500000	0.001956	0.300000
ZYZ_OTT.imp	1620518.000000	0.100000	0.000049	0.300000
ZYZ_OTVZC.imp	7719300.000000	0.100000	0.000236	0.300000'
map <- do.call(rbind,strsplit(strsplit(map,'\n')[[1]],'\t'))

mfile <- select(raw
                ,Province
                ,Yinlu_HZD.vol
                ,Yinlu_HZD.avp
                ,Yinlu_HZD.wd
                ,Yinlu_BBZ.vol
                ,Jishi_total.vol
                ,Wahaha_total.vol
                ,BBZ_total.grp
                ,HZD_total.grp
                ,BBZ_OTV.igrp
                ,HZD_OTV.igrp
                ,HZD_Digital_dispaly.imp
                ,BBZ_ooh.mspd
                ,ZYZ_total.grp
                ,ZYZ_OTV.igrp
                ,ZYZ_Digital_display.imp
                ,JPZ_APP.imp
                ,HZD_DM.discount
                ,HZD_paid.display
                ,HZD_consumer.promo
                ,HZD_tonglu.promo
                ,HZD_gxgc.promo
                ,HZD_OTT.imp
                ,ZYZ_OTT.imp
                ,ZYZ_OTVZC.imp)
hh <- rep(tapply(mfile$Jishi_total.vol,mfile$Province,mean),each=36)/100
mfile$Yinlu_HZD.vol <- mfile$Yinlu_HZD.vol/hh
mfile$Yinlu_BBZ.vol <- mfile$Yinlu_BBZ.vol/hh
mfile$Jishi_total.vol <- mfile$Jishi_total.vol/hh
mfile$Wahaha_total.vol <- mfile$Wahaha_total.vol/hh
options(warn=1)
mfile <- lapply(unique(mfile$Province),function(p){
  x <- filter(mfile,Province==p) %>% select(-Province)
  for(i in 1:ncol(x)){
    # print(i)
    x[,i] <- ret(x[,i],as.numeric(map[grep(colnames(x)[i],map[,1]),5]))
  }
  x
})
mfile <- do.call(rbind,mfile)
model.coef <- apply(mfile,2,function(x){sum(x*hh)})/apply(mfile,2,function(x){sum(x*hh)})[1]
model.coef <- as.numeric(map[match(names(model.coef),map[,1]),4])/model.coef
for(i in 1:length(model.coef)){
  mfile[,i] <- mfile[,i] * ifelse(is.na(model.coef),1,model.coef)[i]
}
mfile1 <- mfile[,is.na(model.coef)]
mfile2 <- mfile[,!is.na(model.coef)]
dummy <- (outer(raw$Province,unique(raw$Province),'=='))+0
colnames(dummy) <- unique(raw$Province)
dummy2 <- (outer(substr(raw$MONTH,5,6),unique(substr(raw$MONTH,5,6)),'=='))+0
colnames(dummy2) <- unique(substr(raw$MONTH,5,6))
dummy <- cbind(dummy,dummy2[,-12])
mfile1 <- cbind(mfile1,dummy)
res <- mfile1$Yinlu_HZD.vol - rowSums(mfile2)
model <- lm(res~as.matrix(mfile1[,-1])-1)
plot.ts(as.numeric(tapply((rowSums(mfile2)+res)*hh,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=2)

#Optimization 1
b0 <- rep(1,ncol(mfile2))
loss <- function(b){
  l1 <- sum((mfile1$Yinlu_HZD.vol - colSums(t(mfile2)*b) - predict(model))^2)/sum((mfile1$Yinlu_HZD.vol - colSums(t(mfile2)*b0) - predict(model))^2)
  l2 <- mean((b-b0)^2)
  l1+l2
}
b1 <- optim(b0,loss,control=list(maxit=10000))[[1]]
for(i in 1:ncol(mfile2)){mfile2[,i] <- mfile2[,i] * b1[i]}
res <- mfile1$Yinlu_HZD.vol - rowSums(mfile2)
model <- lm(res~as.matrix(mfile1[,-1])-1)
plot.ts(as.numeric(tapply((rowSums(mfile2)+res)*hh,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=2)

#Optimization 2
res <- res - predict(model)
mfile2.coef <- apply(1+apply(mfile2 / rowSums(mfile2) * res,2,function(x){
  tapply(x,rep(rep(1:3,each=12),24),sum)
})/apply(mfile2,2,function(x){
  tapply(x,rep(rep(1:3,each=12),24),sum)
}),2,function(x){
  return(x)# rep(rep(x,each=12),3)
})
b0 <- ifelse(is.na(mfile2.coef),1,mfile2.coef)
b0 <- as.vector(b0)
loss <- function(b){
  b2 <- matrix(b,nrow=3)
  b2 <- apply(b2,2,function(x){rep(rep(x,each=12),3)})
  l1 <- sum((mfile2*b2)^2)/sum((mfile2*b0)^2)
  l2 <- sum((b-b0)^2)
  l1+l2
}
system.time(b1 <- optim(b0,loss,control=list(maxit=10000))[[1]])
b1 <- apply(matrix(b1,nrow=3),2,function(x){rep(rep(x,each=12),3)})
mfile2 <- mfile2 * b1
# mfile2 <- ((mfile2.coef-1)+1) * mfile2
mfile2[is.na(mfile2)] <- 0
res <- mfile1$Yinlu_HZD.vol - rowSums(mfile2)
mfile1 <- as.matrix(mfile1[,-1])
model <- lm(res~mfile1-1)
plot.ts(as.numeric(tapply(raw$Yinlu_HZD.vol,raw$MONTH,sum)))
lines(as.numeric(tapply((predict(model)+rowSums(mfile2))*hh,raw$MONTH,sum)),col=2)

decomp <- apply(cbind(t(t(mfile1) * coef(model)),mfile2),2,function(x){
  tapply(x*hh,raw$MONTH,sum)
})
decomp <- data.frame(decomp,
                     actual=as.numeric(tapply(raw$Yinlu_HZD.vol,raw$MONTH,sum)),
                     predict=rowSums(decomp))
plot.ts(decomp$actual); lines(decomp$predict,col=2)
write.csv(data.frame(month=unique(raw$MONTH),decomp),'decomp2.csv')
write.csv(apply(decomp,2,function(x){
  tapply(x,rep(1:3,each=12),sum)
}),'summary2.csv')


