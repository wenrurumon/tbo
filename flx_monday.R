
rm(list=ls())
setwd('/Users/wenrurumon/Downloads')
raw <- openxlsx::read.xlsx('mmix_flx.xlsx',1)[,-1]

#############################
# Function
#############################

# getvar('FLIXONASE_AQUA_VAL',F)

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

#############################
# Processing
#############################

#sales data
flx.val <- getvar('FLIXONASE_AQUA_VAL',F)
flx.vol <- getvar('FLIXONASE_AQUA_PACK',F)
flx.avp <- flx.val / flx.vol
flx.wd <- mc(getvar('FLIXONASE_AQUA_WD',F))
flx.nd <- mc(getvar('FLIXONASE_AQUA_ND',F))
cat.val <- getvar('VAL.sales',F,T)
hh <- rep(tapply(cat.val,raw$City,mean),each=30)
mdum <- paste0('M',rep(rep(1:12,length=30),27))
cdum <- raw$City
ydum <- do.call(rbind,strsplit(raw$Month,'/'))[,1]
y1 <- ydum=='2016'
hos.val <- getvar('hos_Value',F,T)
comp.val <- getvar('RHINOCORT_AQUA_VAL|CLARITYNE_VAL.sales|NEW_XISIMIN_VAL.sales|NASONEX_VAL.sales',F)
comp.validx <- lm(comp.val~mdum)$residual
bcn.val <- getvar('BECONASE')
plot.mt(getvar('NEW_XISIMIN_VAL',F))

#atl
flx.tv <- ret(getvar('GRP.tv',F,T),0.4)
flx.otv <- ret(getvar('GRP.otv',F,T),0.4)
flx.totv <- ret(getvar('Mobile_iGRP.targetotv|PC_iGRP.targetotv',F,T),0.3)
flx.sem <- ret(getvar('Clicks.sem',F,T),0.3)
flx.semimp <- ret(getvar('Impressions.sem',F,T),0.3)
flx.digital <- ret(getvar('Liulanliang.digital',F,T),0.3)
flx.social <- ret(getvar('Read.social',F,T),0.3)

#btl
flx.app <- (getvar('saomaliang.app',F,T))
flx.edu <- ret(getvar('shijirenshu.edu',F,T),0.5)
flx.posm <- ret(getvar('posm',F,T),0.5)
flx.rmd <- ret(getvar('hejiyujidanpinshuliang.rmd',F,T),0.5)
flx.rmd_csm <- ret(getvar('mianxiangxiaofeizhe',F,T),0.3)
flx.rmd_store <- ret(getvar('mianxiangdianyuan',F,T) +
                       getvar('mianxiangmendian',F,T) +
                       getvar('xinxingqiqiu',F,T),0.5)

#############################
# Modeling
#############################

# reprocessing

flx.vol2 <- flx.vol / hh
cheat1 <- rep(1:30,27) %in% 16:17
cheat2 <- rep(1:30,27) %in% c(13:15)
holdout <-   cbind(
  flx.tv = flx.tv  * 5.169e-07 * 1.315773,
  flx.otv = flx.otv * 6.462e-07 * 0.4674315,
  flx.totv = flx.totv * 4.204e-06 * 0.4674315,
  flx.digital = flx.digital * 8.177e-14 * 3.066865,
  flx.sem = flx.semimp * 3.24e-09 * 0.1614686,
  flx.social = flx.social * 2.093e-10 * 0.1243447,
  flx.app = flx.app  *  1.102e-07,
  flx.posm = flx.posm * 1.370e-07 * 0.15,
  flx.rmd = flx.rmd * 2.869e-08,
  flx.edu = flx.edu * 1.679e-07 * 0.1239381,
  cheat1 = cheat1 * 0.0001814190,
  cheat2 = cheat2 * -0.0001814190 *54/81,
  flx.wd = flx.wd * 0.0016429148
)
flx.vol3 <- flx.vol2 - rowSums(holdout)

# Model

xlm <- lm(flx.vol3 ~ -1 
          # + cdum + mdum
          + paste0(cdum,mdum)
          # + flx.wd 
)
tail(coef(summary(xlm)))

# Tech Review
plot.model(
  y.raw <- mt(flx.vol),
  y.pred <- mt((predict(xlm) + rowSums(holdout)) * hh),
  minval = min(y.pred)/2
)
summary(abs(y.pred/y.raw-1))
summary(lm(y.raw~-1+y.pred))$r.square

# Calc
colSums(holdout*hh)/sum(y.pred)
check(flx.tv  * 5.169e-07 * 1.315773 * hh,flx.vol)
check(flx.otv * 6.462e-07 * 0.4674315 * hh,flx.vol)
check(flx.totv * 4.204e-06 * 0.4674315 * hh,flx.vol)
check(flx.semimp * 3.24e-09 * hh,flx.vol)
check(flx.digital * 8.177e-14 * hh,flx.vol)
check(flx.app  *  1.102e-07 * hh,flx.vol)
check(flx.rmd_csm * 9.562e-08 * hh,flx.vol)
check(flx.rmd_store * 1.740e-07 * hh,flx.vol)
check(flx.posm * 1.370e-07 * 0.15 * hh,flx.vol)
check(flx.rmd * 2.869e-08 * hh,flx.vol)
check(flx.social * 2.093e-10 * hh, flx.vol)
check(flx.edu * 1.679e-07 * hh, flx.vol)
check(cheat1 * 0.0001814190 * hh, flx.vol)

# Validate

#############################
# Decomp
#############################

decomp <- cbind(
  flx.tv = flx.tv  * 5.169e-07 * 1.315773,
  flx.otv = flx.otv * 6.462e-07 * 0.4674315,
  flx.totv = flx.totv * 4.204e-06 * 0.4674315,
  flx.digital = flx.digital * 8.177e-14 * 3.066865,
  flx.sem = flx.semimp * 3.24e-09 * 0.1614686,
  flx.social = flx.social * 2.093e-10 * 0.1243447,
  flx.app = flx.app  *  1.102e-07,
  flx.posm = flx.posm * 1.370e-07 * 0.15,
  flx.rmd = flx.rmd * 2.869e-08,
  flx.edu = flx.edu * 1.679e-07 * 0.1239381,
  cheat = cheat1 * 0.0001814190 + cheat2 * -0.0001814190 *54/81,
  flx.wd = flx.wd * 0.0016429148
) * hh
decomp <- apply(decomp,2,mt)
y.res <- mt(flx.vol) - rowSums(decomp)
y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
decomp <- cbind(decomp,season = y.season)
plot.model(mt(flx.vol),rowSums(decomp),min(mt(flx.vol)))

apply(decomp,2,function(x){
  out1 <- tapply(x,rep(1:3,each=12)[1:30],sum)
  out2 <- tapply(x,c(rep(0,6),rep(4:5,each=12)),sum)
  c(out1,out2[-1])
})
