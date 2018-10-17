
rm(list=ls())
setwd('E:\\gsk\\fbd_flx\\model')
# raw <- openxlsx::read.xlsx('mmix_fbd.xlsx',1)[,-1]
raw <- data.table::fread('mmix_fbd_withsku.csv')
raw <- as.data.frame(raw)[,-1]

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
y1 <- ydum=='2016'
y2 <- ydum=='2017'
y3 <- ydum=='2018'

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
hh <- fbd.vol / mc(fbd.vol)

#ATL
fbd.tv <- ret(getvar('GRP.tv',F,F)[,1],0.3)
fbd.otv <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T),0.3)
fbd.semclick <- ret(getvar('Clicks.sem',F,T),0.3)
fbd.semimp <- ret(getvar('Impressions.sem',F,T),0.3)
fbd.digital <- ret(getvar('Impression.digital',F,T),0.3)
fbd.social <- getvar('Impressions.social|Impressions_.social',F,F)[,2]
fbd.social[fbd.social==59597928] <- sum(c(1474320,112301))
fbd.social <- ret(fbd.social,0.3)

#BTL
fbd.posm <- getvar('hejiyujidanpinshuliang.posm',F,T)
fbd.rmd <- getvar('hejiyujidanpinshuliang.rmd',F,T)
fbd.edu <- getvar('shijirenshu.edu',F,T)
fbd.app <- getvar('saomaliang.app',F,T)

#############################
# Modeling
#############################

# reprocessing
y.vol <- fbd.vol
y.vol2 <- y.vol / hh
holdout <-   cbind(
  intercept = rep(0,810)
  ,fbd.tv = fbd.tv*9.826528e-05*2
)
y.vol3 <- y.vol2 - rowSums(holdout)

# Model
xlm <- lm(y.vol3 ~ -1
          + paste(cdum,mdum)
          + fbd.wd + fbd.avp
          # + mc(fbd300.vol)
)
tail(coef(summary(xlm)))
plot.model(mt(y.vol),mt((predict(xlm)+rowSums(holdout))*hh),min(mt(y.vol))*0.7)

# Calc
check(fbd400.eqhh*0.183083943*hh,fbd.vol)
check(fbd400.wd*0.0009729*hh,fbd400.vol)
check(fbd.tv*9.826528e-05*2*hh,fbd.vol)

# Tech Review
y.raw <- mt(y.vol)
y.pred <- mt((predict(xlm)+rowSums(holdout))*hh)
plot.model(y.raw,y.pred)
summary(abs(y.pred/y.raw-1),2)

#############################
# Decomp
#############################

decomp <- cbind(
  fbd.wd = mc(fbd.wd)*0.44999799
  ,fbd400.wd = mc(fbd400.wd)*-0.15238009
  ,fbd.avp = fbd.avp*-1.22536989
  ,fbd400.vol = fbd400.eqhh*0.120199506
  ,fbd.tv = fbd.tv*9.826528e-05*2
  ,fbd.otv = fbd.otv*5.890516e-04*0.7
  ,fbd.sem = fbd.semclick*4.911940e-06/3
  ,fbd.digital = fbd.digital*6.815926e-12
  ,fbd.social = fbd.social*4.007322e-9*4
  ,fbd.posm = fbd.posm*5.052831e-06/2
  ,fbd.rmd = fbd.rmd*7.381199e-06
  ,fbd.app = fbd.app*1.007945e-06
  ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
)
decomp <- apply(decomp,2,function(x){
  mt(x*hh)
})
y.res <- mt(y.vol) - rowSums(decomp)
y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
decomp <- cbind(decomp,season = y.season)
decomp <- data.frame(decomp,predict=rowSums(decomp),actual=mt(y.vol),value=mt(fbd.val))
rownames(decomp) <- unique(raw$Month)
plot.model(decomp$actual,decomp$predict,min(decomp$predict)*0.5)

decomp <- rbind(decomp,
                apply(decomp,2,function(x){
                  out1 <- tapply(x,rep(1:3,each=12)[1:30],sum)
                  out2 <- tapply(x,c(rep(0,6),rep(4:5,each=12)),sum)
                  c(out1,out2[-1])
                })
)

# write.csv(t(decomp),'decomp_fbd.csv')
