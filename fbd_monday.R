
rm(list=ls())
setwd('E:\\gsk\\fbd_flx\\model')
# raw <- openxlsx::read.xlsx('mmix_fbd.xlsx',1)[,-1]
raw <- as.data.frame(data.table::fread('mmix_fbd_withsku.csv'))[,-1]
raw2 <- as.data.frame(data.table::fread('mmix_fbd_sku2.csv'))[,-1]
pcat1.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('GeneralPainRelief',colnames(raw2),value=T),value=T))])
pcat2.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('MuscularPainRelief',colnames(raw2),value=T),value=T))])
pcat3.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('100_Pain',colnames(raw2),value=T),value=T))])
pcat.val <- rowSums(pcat1.val)+rowSums(pcat2.val)+rowSums(pcat3.val)
tcm.val <- rowSums(cbind(pcat1.val,pcat2.val,pcat3.val)[,grep('Tcm',colnames(cbind(pcat1.val,pcat2.val,pcat3.val)))])
getvar('OTC')

#############################
# Function
#############################

getvar <- function(varname,value=T,rowsum=T,database=raw){
  sel <- grep(varname,colnames(database))
  if(value){
    x <- (colnames(database)[sel])
    return(x)
  } else {
    x <- (database[,sel,drop=F])
  }
  if(rowsum){
    return(rowSums(x))
  } else {
    return(x)
  }
}
mt <- function(x){
  as.numeric(
    tapply(x,
           rep(1:30,length=810),
           sum)
  )
}
plot.mt <- function(x){
  plot.ts(mt(x))
}
ret <- function(x,ret=0.3){
  x <- sapply(unique(raw$City),function(city){
    x[raw$City==city]
  })
  for(i in 2:nrow(x)){
    x[i,] <- x[i-1,]*ret + x[i,]
  }
  unlist(apply(x,2,list))
}
mc <- function(x){
  x.mc <- tapply(x,raw$City,mean)
  x/x.mc[match(raw$City,names(x.mc))]
}
check <- function(x,y,ifplot=F){
  out <- c(tapply(x,ydum,sum),total=sum(x))/c(tapply(y,ydum,sum),sum(y))
  if(ifplot){plot.model(mt(y),mt(x))}
  return(out)
}
lag <- function(x,l=1){
  x <- sapply(unique(raw$City),function(city){
    x[raw$City==city]
  })
  x <- apply(x,2,function(xi){
    c(rep(0,l),xi)
  })[1:nrow(x),]
  unlist(apply(x,2,list))
}
plot.model <- function(x,y,minval=0){
  maxval <- max(c(x,y))
  plot.ts(rep(c(minval,maxval),length=length(x)),col=0)
  lines(x)
  lines(y,col=2)
}
compare <- function(x){
  t1 <- rep(c(rep(1:2,each=12),rep(3,each=12)),length=810)
  t2 <- rep(c(rep(0,6),rep(4:5,each=12)),length=810)
  c(tapply(x,t1,sum),tapply(x,t2,sum)[-1])
}

#############################
# Processing
#############################

#Dummy
ydum <- do.call(rbind,strsplit(paste(raw$Month),'/'))[,1]
cdum <- raw$City
mdum <- paste0('M',rep(rep(1:12,length=30),length=810))
y1 <- as.numeric(ydum=='2016')
y2 <- as.numeric(ydum=='2017')
y3 <- as.numeric(ydum=='2018')

#sales data
fbd.val <- getvar('FENBID_VAL.sales|FENBID_FAST_VAL.sales',F,T)
f400.val <- getvar('400MG_x_24_VAL.sales',F,T)
foth.val <- fbd.val - f400.val
f400.ul <- f400.val * 348315.7339/sum(f400.val)
foth.ul <- foth.val * 1082580.493/sum(foth.val)
fbd.ul <- f400.ul + foth.ul
compare(fbd.val)
compare(fbd.ul)

fbd_sku.vol <- raw[,match(grep('PACK.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.vol <- rowSums(as.matrix(fbd_sku.vol) %*% cbind(as.numeric(sapply(strsplit(grep('PACK.sales',getvar('FENBID')[-1:-8],value=T),'_'),function(x){x[length(x)-1]}))))
compare(fbd.vol)
fbd_sku.pack <- raw[,match(grep('PACK.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.pack <- rowSums(fbd_sku.pack)
compare(fbd.pack)
fbd_sku.val <- raw[,match(grep('VAL.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.val <- rowSums(fbd_sku.val); plot.mt(fbd.val)
compare(fbd.val)
fbd_sku.wd <- raw[,match(grep('WD.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.wd <- apply(fbd_sku.wd,1,max)
fbd_sku.nd <- raw[,match(grep('ND.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.nd <- apply(fbd_sku.nd,1,max)
fbd.avp <- fbd.val/fbd.vol

fbd400.val <- getvar('400MG_x_24_VAL',F,T)
fbd400.vol <- getvar('400MG_x_24_PACK',F,T) * 24
fbd400.wd <- apply(getvar('400MG_x_24_WD',F,F),1,max)
fbd400.nd <- apply(getvar('400MG_x_24_ND',F,F),1,max)
fbd400.eqhh <- mc(fbd400.vol)
fbd300.val <- fbd.val - fbd400.val
fbd300.vol <- fbd.vol - fbd400.vol
fbd300.avp <- fbd300.val / fbd300.vol
fbd400.avp <- fbd400.val / fbd400.vol


#ATL
fbd.tv <- ret(getvar('GRP.tv',F,F)[,1],0.3)
fbd.tv1 <- ret(getvar('GRP.tv',F,F)[,1]*y1,0.3)
fbd.tv2 <- ret(getvar('GRP.tv',F,F)[,1]*y2,0.3)
fbd.tv3 <- ret(getvar('GRP.tv',F,F)[,1]*y3,0.3)


fbd.otv <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T),0.3)
fbd.semclick <- ret(getvar('Clicks.sem',F,T),0.3)
fbd.semimp <- ret(getvar('Impressions.sem',F,T),0.3)
fbd.digital <- ret(rowSums(getvar('Impression.digital',F,F)[,-5]),0.3)
fbd.digital_social <- ret(getvar('Impression.digital',F,F)[,5],0.3)
fbd.social <- ret(getvar('Impressions.social|Impressions_.social',F,T),0.3)
# fbd.social[fbd.social==59597928] <- sum(c(1474320,112301))
# fbd.social <- ret(fbd.social,0.3)

#BTL
fbd.posm <- getvar('hejiyujidanpinshuliang.posm',F,T)
fbd.posm1 <- rep(1:30,27)%in%(5:12)
fbd.rmd <- getvar('hejiyujidanpinshuliang.rmd',F,T)
fbd.edu <- getvar('shijirenshu.edu',F,T)
fbd.app <- getvar('saomaliang.app',F,T)


#############################
# Modeling
#############################

# reprocessing
y.vol <- fbd.vol
hh <- y.vol / mc(y.vol)
y.vol2 <- y.vol / hh
holdout <-   cbind(
  intercept = rep(0,810)
  ,pcat.val = pcat.val*6.39825e-05
  ,fbd400.vol = mc(fbd400.vol)*0.144583642
  ,fbd.tv=fbd.tv1*9.826528e-05*2
  +fbd.tv2*9.826528e-05*2
  +fbd.tv3*9.826528e-05*2
  ,fbd.otv=fbd.otv*5.890516e-04*0.7
  ,fbd.sem=fbd.semclick*4.911940e-06/3
  ,fbd.digital=fbd.digital*6.815926e-12+fbd.digital_social*4.007322e-11*4
  ,fbd.social = fbd.social*4.007322e-11*4
  ,fbd.posm = fbd.posm1*0.0164719+fbd.posm*5.052831e-06/2
  ,fbd.rmd = fbd.rmd*7.381199e-06
  ,fbd.app = fbd.app*1.007945e-06
  ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
)
y.vol3 <- y.vol2 - rowSums(holdout)

# Model
xlm <- lm(y.vol3 ~ -1
          + cdum
          +paste(cdum,mdum)
          +mc(fbd400.wd)
          # +y1+y2+y3
          # +fbd.tv1+fbd.tv2+fbd.tv3
)
tail(coef(summary(xlm)))
plot.model(mt(y.vol),mt((predict(xlm)+rowSums(holdout))*hh),min(mt(y.vol))*0.5)

# Calc
check((mc((fbd400.wd)^1/3)*0.5292653+y1*0.09944523+y2*0.29267172+y3*0.40809198)*hh,fbd400.vol)
check(fbd400.eqhh*0.183083943*hh,fbd.vol)
check(fbd400.wd*0.0009729*hh,fbd400.vol)
check(fbd.tv*9.826528e-05*2*hh,fbd.vol)
check(fbd.otv*5.890516e-04*0.7*hh,fbd.vol)
check(fbd.semclick*4.911940e-06/3*hh,fbd.vol)
check(fbd.digital*6.815926e-12*hh,fbd.vol)
check(fbd.social*4.007322e-11*4*hh,fbd.vol)
check(fbd.edu*9.573258e-04*0.02013463*hh,fbd.vol)
check(fbd.posm1*0.0164719*hh,fbd.vol)
check(pcat.val*6.39825e-05*hh,fbd.vol)

# Tech Review
y.raw <- mt(y.vol)
y.pred <- mt((predict(xlm)+rowSums(holdout))*hh)
plot.model(y.raw,y.pred)
summary(abs(y.pred/y.raw-1),2)

