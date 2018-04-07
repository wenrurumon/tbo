rm(list=ls())
setwd("/Users/wenrurumon/Documents/GSK/model")
library(openxlsx)
f <- function(x,i){
  x <- try(read.xlsx(x,i))
  print(i)
  if(class(x)=="try-error"){
    print('close')
    return(NA)
  }else{
    x
  }
}
databtb <- lapply(1:13,function(i){
  f(dir(pattern='_model_data')[1],i)
})
datavtr <- lapply(1:13,function(i){
  f(dir(pattern='_model_data')[2],i)
})
xbtb <- lapply(databtb,function(x){
  s <- which(is.na(x[,1]))
  map <- t(x[c(s,max(s)+1),])
  coln <- x[max(s)+1,]
  colnames(x) <- coln
  data <- x[-c(s,s+1),-1:-2]
  data <- apply(data,2,as.numeric)
  list(map=map,data=data)
})
xvtr <- lapply(datavtr,function(x){
  s <- which(is.na(x[,1]))
  map <- t(x[c(s,max(s)+1),])
  coln <- x[max(s)+1,]
  colnames(x) <- coln
  data <- x[-c(s,s+1),-1:-2]
  data <- apply(data,2,as.numeric)
  list(map=map,data=data)
})
names(xbtb) <- getSheetNames(dir(pattern='_model_data')[1])
names(xvtr) <- getSheetNames(dir(pattern='_model_data')[2])

keyvtr <- datavtr[[1]][,1:2]
keyvtr <- keyvtr[!is.na(keyvtr[,1]),][-1,]
xvtr <- lapply(xvtr,function(x){
  map <- x$map
  data <- x$data[as.numeric(keyvtr[,2])>42339,]
  list(map=map,data=data)
})
keyvtr <- keyvtr[as.numeric(keyvtr[,2])>42339,]

keybtb <- databtb[[1]][,1:2]
keybtb <- keybtb[!is.na(keybtb[,1]),][-1,]
xbtb <- lapply(xbtb,function(x){
  map <- x$map
  data <- x$data[as.numeric(keybtb[,2])>=42005,]
  list(map=map,data=data)
})
keybtb <- keybtb[as.numeric(keybtb[,2])>=42005,]

#################
# Voltaren
# 313313 as total shipment value
#################

tm <- function(x,m=m.dum){
  as.numeric(tapply(x,m,sum))
}
ret <- function(x,d=c.dum,r=0.3){
  for(i in 2:length(x)){
    if(c.dum[i]==c.dum[i-1]){
      x[i] <- x[i-1] * r + x[i]  
    }
  }
  x
}
lag <- function(x,d=c.dum,l=3){
  x2 <- rep(0,length=length(x))
  for(i in (l+1):length(x)){
    if(c.dum[i]==c.dum[i-l]){
      x2[i] <- x[i-l]
    }
  }
  return(x2)
}

#Process sales data
c.dum <- keyvtr[,1]
m.dum <- rep(1:24,length=length(c.dum))
m.dum2 <- rep(1:12,length=length(c.dum))
x <- xvtr$VTR_sales
ve.vols <- x$data[,grepl('vol',x$map[-1:-2,4])&grepl('EMULGEL',x$map[-1:-2,3]),drop=F]
ve.vol <- as.matrix(ve.vols) %*% matrix(c(20,50,0),nrow=3)
ve.vals <- (x$data[,grepl('val',x$map[-1:-2,4])&grepl('EMULGEL',x$map[-1:-2,3]),drop=F])
cat.val <- x$data[,19]
cat.vol <- x$data[,20]
ve.val <- rowSums(ve.vals[,1:2])
ve.avp <- as.numeric(ve.val/ve.vol)
vt.vols <- x$data[,grepl('vol',x$map[-1:-2,4])&grepl('TAB EC',x$map[-1:-2,3]),drop=F]
vt.vol <- as.matrix(vt.vols) %*% matrix(c(10,20,30),nrow=3) * 25
ve.nd <- apply(x$data[,grepl('ND',colnames(x$data))],1,max,na.rm=T)
ve.wd <- apply(x$data[,grepl('WD',colnames(x$data))],1,max,na.rm=T)
hos.vol <- ret(lag(x$data[,21],2),c.dum,0.3)
vtcat.val <- x$data[,23]
vt.nd <- x$data[,24]
vt.wd <- x$data[,25]

#Process ATL
vtr.grp <- ret(rowSums(xvtr$tv$data),c.dum,0.3)
vtr.otvimp <- ret(rowSums(xvtr$otv$data[,grepl('impression',colnames(xvtr$otv$data))]),c.dum,0.3)
vtr.dgt <- ret( rowSums(xvtr$digital$data[,grepl('Impression',colnames(xvtr$digital$data))]),c.dum,0.3)
vtr.sem <- ret(rowSums(xvtr$SEM$data[,grepl('Clicks',colnames(xvtr$SEM$data))]),c.dum,0.3)
vtr.social <- ret(rowSums(xvtr$social$data[,grepl('Impression',colnames(xvtr$social$data))]),c.dum,0.3)
vtr.ooh <- ret(xvtr$ooh$data[,2],c.dum,.3)
vtr.reminder <- rowSums(xvtr$`display(POSM)`$data[,grepl('reminder',colnames(xvtr$`display(POSM)`$data))])
vtr.posm <- rowSums(xvtr$`display(POSM)`$data[,grepl('posm',colnames(xvtr$`display(POSM)`$data))])
vtr.instore <- rowSums(xvtr$KASC$data[,grepl('店内促销_store',colnames(xvtr$KASC$data))])
vtr.onshelf <- rowSums(xvtr$KASC$data[,grepl('上架_store',colnames(xvtr$KASC$data))])
vtr.app <- rowSums(xvtr$VTR_APP$data[,grepl('Scanned',colnames(xvtr$VTR_APP$data))])
vtr.yxt <- (xvtr$VTR_traning$data)[,1]

#################################
#Model1
#################################

summary(xlm <- lm(ve.vol~
                  +cbind(ve.wd,ve.avp,cat.val,hos.vol)
                  +cbind(vtr.grp,vtr.otvimp,vtr.dgt,vtr.sem,vtr.social,vtr.ooh)
                  +cbind(vtr.posm,vtr.reminder,vtr.instore,vtr.onshelf,vtr.app,vtr.yxt)
                  +c.dum-1
))
plot.ts(as.numeric(tapply(ve.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm),m.dum,sum)),col=2)

# write.csv(cbind(coef(xlm)[5:16],t(apply(
#   cbind(vtr.grp,vtr.otvimp,vtr.dgt,vtr.sem,
#         vtr.social,vtr.ooh,vtr.posm,vtr.reminder,
#         vtr.instore,vtr.onshelf,vtr.app,vtr.yxt),2,function(x){
#   tapply(x,rep(rep(1:2,each=12),length=length(c.dum)),sum)
# })) * as.numeric(coef(xlm)[5:16]))
# ,'test.csv')

hold <- cbind(vtr.grp,vtr.otvimp,vtr.dgt,vtr.sem,
        vtr.social,vtr.ooh,vtr.posm,vtr.reminder,
        vtr.instore,vtr.onshelf,vtr.app,vtr.yxt) %*% 
cbind(c( 0.2914201317057840000000 	,
   0.0000150162939176248000  	,
 0.0000000089284785637539 	,
 0.0012758970784773200000 	,
 0.0000042150892574549800 	,
 0.0001324009099213440000 	,
 0.0052948749327489900000 	,
 0.0446907658040851000000 	,
 0.0230145303946794000000 	,
 0.0210683519962408000000 	,
 0.0007867979365923090000 	,
 0.1510931127686600000000 	)) + cat.val*  4.143e-01

ve.vol2 <- ve.vol - hold
summary(xlm <- lm(ve.vol2~
                  +cbind(ve.avp,hos.vol,ve.wd)
                  +c.dum+paste(m.dum2)-1
))
plot.ts(as.numeric(tapply(ve.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

#################################
#Model2
151270/sum(vt.vol)
#################################

summary(xlm <- lm(vt.vol~ vtcat.val + vt.nd + vt.wd
                  +cbind(vtr.grp,vtr.otvimp,vtr.social,vtr.ooh)
                  +c.dum-1
))
plot.ts(as.numeric(tapply(vt.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm),m.dum,sum)),col=2)

0.01406046 * 9.353e-01* tapply(vtr.grp,rep(rep(1:2,each=12),length=length(c.dum)),sum)
0.01406046 * 3.031e-05* tapply(vtr.otvimp,rep(rep(1:2,each=12),length=length(c.dum)),sum)
0.01406046 * 2.793e-06* tapply(vtr.social ,rep(rep(1:2,each=12),length=length(c.dum)),sum)
0.01406046 * 8.652e-03* tapply(vtr.ooh ,rep(rep(1:2,each=12),length=length(c.dum)),sum)

# cbind(vtr.grp, vtr.otvimp, vtr.social, vtr.ooh)vtr.grp     9.353e-01  6.386e-01   1.464 0.143574    
# cbind(vtr.grp, vtr.otvimp, vtr.social, vtr.ooh)vtr.otvimp  3.031e-05  3.197e-05   0.948 0.343560    
# cbind(vtr.grp, vtr.otvimp, vtr.social, vtr.ooh)vtr.social  2.793e-06  1.342e-05   0.208 0.835229    
# cbind(vtr.grp, vtr.otvimp, vtr.social, vtr.ooh)vtr.ooh     8.652e-03  2.691e-03   3.216 0.001370 ** 

####################################################################################
####################################################################################

#################
# Bactroban
#################

#Process sales data
c.dum <- keybtb[,1]
m.dum <- rep(1:36,length=length(c.dum))
m.dum2 <- rep(1:12,length=length(c.dum))
btb.sales <- xbtb$BTB_sales$data[,grep('BAC',colnames(xbtb$BTB_sales$data))]
ww.nd <- btb.sales[,1]
ww.vol <- btb.sales[,2]
ww.val <- btb.sales[,3]
ww.wd <- btb.sales[,4]
o10.nd <- btb.sales[,5]
o10.vol <- btb.sales[,6]
o10.val <- btb.sales[,7]
o10.wd <- btb.sales[,8]
o5.nd <- btb.sales[,9]
o5.vol <- btb.sales[,10]
o5.val <- btb.sales[,11]
o5.wd <- btb.sales[,12]
ww.avp <- ww.val/ww.vol; o10.avp <- o10.val/o10.vol; o5.avp <- o5.val/o5.vol
ww.avp <- ifelse(is.na(ww.avp),0,ww.avp)
o10.avp <- ifelse(is.na(o10.avp),0,o10.avp)
o5.avp <- ifelse(is.na(o5.avp),0,o5.avp)
hos.val <- lag(rowSums(xbtb$Hospital$data[,grep('BAC',colnames(xbtb$Hospital$data))[c(3,6)]]),1)
hh <- paste(c.dum,rep(rep(1:3,each=12),length=length(c.dum)))
ohh <- rep(tapply(o10.vol+o5.vol,hh,sum),each=12)
whh <- rep(tapply(ww.vol,hh,sum),each=12)

#Process ATL
o.grp <- ret(rowSums(xbtb$`TV and SPONSORSHIP`$data[,grep('BTB',colnames(xbtb$`TV and SPONSORSHIP`$data))]),c.dum,0.3)
ww.grp <- ret(rowSums(xbtb$`TV and SPONSORSHIP`$data[,grep('WW',colnames(xbtb$`TV and SPONSORSHIP`$data))]),c.dum,0.3)
ar.grp <- ret(xbtb$`TV and SPONSORSHIP`$data[,1],c.dum,0.3)
o.igrp <- ret(rowSums(xbtb$otv$data[,grepl('oint',colnames(xbtb$otv$data))&grepl('GRP',colnames(xbtb$otv$data))]),c.dum,0.3)
w.igrp <- ret(rowSums(xbtb$otv$data[,grepl('WW',colnames(xbtb$otv$data))&grepl('GRP',colnames(xbtb$otv$data))]),c.dum,0.3)
x <- xbtb$`digital(Banner)`$data[,grepl('Impressions',colnames(xbtb$`digital(Banner)`$data))]
w.nmimp <- ret(rowSums(x[,c(10,13)]),c.dum,0.3)
btb.nmimp <- ret(rowSums(x[,14:15]),c.dum,0.3)
o.bimp <- ret(rowSums(x[,(!grepl('OTV',colnames(x)))&(grepl('Oint',colnames(x)))]),c.dum,0.3)
w.bimp <- ret(rowSums(x[,(!grepl('OTV',colnames(x)))&(grepl('ww',colnames(x)))]),c.dum,0.3)
x <- xbtb$`digital（Social）`$data[,grep('Impression',colnames(xbtb$`digital（Social）`$data))]
x[rep(rep(1:3,each=12),length=length(c.dum))==1,] <- 0
o.social <- ret(rowSums(x[,grepl('OINT_Weibo',colnames(x)),drop=F]),c.dum,0.3)
ww.social <- ret(rowSums(x[,grepl('WW',colnames(x)),drop=F]),c.dum,0.3)
btb.social <- ret(rowSums(x[,grepl('BOTH',colnames(x)),drop=F]),c.dum,0.3)
x <- xbtb$`digital(SEM)`$data[,grepl('Click',colnames(xbtb$`digital(SEM)`$data))]
o.sem <- ret(rowSums(x[,grepl('软膏',colnames(x)),drop=F]),c.dum,0.3)
ww.sem <- ret(rowSums(x[,grepl('喷雾',colnames(x)),drop=F]),c.dum,0.3)
btb.sem <- ret(rowSums(x[,grepl('Master',colnames(x)),drop=F]),c.dum,0.3)
x <- xbtb$`Display(POSM)`$data[,grepl('item',colnames(xbtb$`Display(POSM)`$data))]
o.2dp <- rowSums(x[,grepl('Oint_Display_二次',colnames(x)),drop=F])
ww.2dp <- rowSums(x[,grepl('WW_Display_二次',colnames(x))])
o.rmd <- lag(ret(rowSums(x[,grepl('Oint_Display_礼品',colnames(x)),drop=F]),c.dum,0.3),c.dum,1)
ww.rmd <- lag(ret(rowSums(x[,grepl('WW_Display_礼品',colnames(x)),drop=F]),c.dum,0.3),c.dum,1)
o10.app <- xbtb$APP$data[,2]
o5.app <- xbtb$APP$data[,3]
o.yxt <- xbtb$Traning$data[,1]
ww.yxt <- xbtb$Traning$data[,3]

#Oint 10
hold <- 
  o.grp * 2.722e-02 + 
  ww.grp * 0.01523 +
  ar.grp * 8.845e-03 + 
  o.igrp * 0.03721 + 
  w.igrp * 0.01848 + 
  o.bimp * 4.250e-09 +
  w.bimp * 3.101e-09 + 
  btb.nmimp * 1.102e-07 +
  w.nmimp * 5.884e-08 + 
  o.social * 1.686e-06 +
  ww.social * 5.790e-07 + 
  btb.social * 1.458e-06 + 
  o.sem * 4.064e-05 +
  btb.sem * 1.730e-05 +
  o.2dp * 1.701e-04 +
  o.rmd * 2.793e-03 + 
  o10.app * 1.677e-03 +
  o.yxt * 3.827e-03
o10.vol2 <- o10.vol - hold

summary(xlm <- lm(o10.vol2~
                    cbind(o10.nd,o10.avp)
                  +paste(m.dum2)+c.dum-1
                    ))
plot.ts(as.numeric(tapply(o10.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

#Oint5

hold <- 
  o.grp * 0.03196187 + 
  ar.grp*0.014109359 + 
  hos.val*8.992597e-07 + 
  o.igrp * 0.0470193 + 
  o.bimp * 4.503158e-09 +
  btb.nmimp * 2.736293e-08 + 
  o.social * 4.247822e-07 + 
  o.sem * 5.241951e-05 + 
  btb.sem * 3.697118e-06 + 
  o.2dp* 1.495691e-04 +
  o.rmd * 8.532941e-05 + 
  o5.app * 6.628955e-06 + 
  o.yxt * 2.591524e-03 
  
o5.vol2 <- o5.vol - hold
summary(xlm <- lm(o5.vol2~
                    cbind(o5.wd,o5.avp,o10.vol,o10.wd)
                  # +cbind(o5.app,o.yxt)
                  +paste(m.dum2)+c.dum-1
                    ))
coef(xlm)[1:10]
plot.ts(as.numeric(tapply(o5.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

#WW

sum(ww.yxt*0.0007247435   )/sum(ww.vol)
hold <- ww.grp*0.025998+
  ar.grp*0.01237664+
  w.igrp*0.006299749+
  w.bimp*1.911974e-09+
  w.nmimp*1.408838e-08+
  btb.nmimp * 3.894083e-09+
  ww.social*2.402012e-07+
  btb.social * 1.286117e-07+
  ww.sem*6.315932e-06+
  ww.2dp*3.699074e-04+
  ww.rmd*3.909537e-04+
  ww.yxt*0.0007247435
  
ww.vol2 <- ww.vol - hold
summary(xlm <- lm(ww.vol2~
                  cbind(ww.nd,ww.wd)
                  +paste(m.dum2)+c.dum-1))
plot.ts(as.numeric(tapply(ww.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

