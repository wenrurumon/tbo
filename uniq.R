
############################ Load Data and Library######################################################
########################################################################################################

setwd("C:/Users/zhu2/Documents/mindshare/uniq")
library(sqldf)
library(data.table)
library(dplyr)

rm(list=ls())
load('data.Rdata')
load('rlt_list.rda')
sales$cate <- paste(sales$yr_ssn,sales$cate)

retention <- function(x,rate=0.75,code){
  for(i in 2:length(x)){
    if(code[i]==code[i-1]){
      x[i] <- x[i-1]*rate + x[i]
    }
  }
  return(x)
}
meancenter <- function(x,code){
  grpmean <- tapply(x,code,mean)
  abs(grpmean[match(code,names(grpmean))])
}
getdummy <- function(x){
  rlt <- outer(x,unique(x),'==')+0
  colnames(rlt) <- unique(x)
  return(rlt[,-ncol(rlt)])
}
model <- function(beta,Y,X,betacons){
  beta1 <- ifelse(betacons*beta<0,0,beta)
  beta2 <- optim(beta1,function(b){
    b <- ifelse(b * betacons<0, -b, b)
    sum((Y-b%*%t(X))^2)
  })
  beta2 <-  ifelse(beta2[[1]] * betacons<0, -beta2[[1]], beta2[[1]])
  beta2
}

aggplot <- function(x,l,main=NULL){
  x <- as.numeric(tapply(x,l,sum))
  plot.ts(x,main=main)
}
agglines<- function(x,l,col=2){
  x <- as.numeric(tapply(x,l,sum))
  lines(x,col=col)
}
callcode <- function(text){
  eval(parse(text=text))
}
modelfile <- function(cati){
  cati <- paste(cati)
  print(cati)
  tsales <- filter(sales,cate==cati,Qty>0,week<=20170000)
  tcode <- paste0(tsales$week,tsales$city,tsales$cate,tsales$yr_ssn)
  tsales <- mutate(tsales,
                   year=paste0('year',substr(week,1,4)),
                   month=substr(week,5,6),
                   aphh=Amnt/hh,qphh=Qty/hh,
                   Amnt=ifelse(Amnt<0,0,Amnt),Qty=ifelse(Qty<0,0,Qty),Disc=ifelse(Disc<0,0,Disc),
                   Disc_Ratio=ifelse(Amnt+Disc<=0,0,Disc/(Amnt+Disc)),
                   pris=ifelse(Qty==0,0,-Amnt/Qty),
                   pr=FRCN_DE_imp_pr+FRCN_SOLW_imp_pr+jogger_imp_pr+
                     kaws_imp_pr+shanghai_lemaire_imp_pr+shanghai_magic_imp_pr+
                     shanghai_reopen_imp_pr+victoria_imp_pr,
                   pr.ret = retention(pr,0.75,tcode),
                   digital=digital_pr
                   +digital_all_fw
                   +digital_ut
                   +digital_all_ss
                   +digital_jeans
                   +digital_kids
                   +digital_branding
                   +digital_sports
                   +digital_festival
                   +digital_polo
                   +digital_jogger_and_jeans
                   +digital_ut_and_linen
                   +digital_airism
                   +digital_jogger
                   +digital_knit
                   +digital_uld
                   +digital_ht
                   +digital_down,
                   digital.ret = retention(digital,rate=0.75,code=tcode
                   ),otv=otv_uld
                   +otv_ht
                   +otv_ut
                   +otv_jeans
                   +otv_down_and_uld_and_wool,
                   otv.ret = retention(otv,0.75,tcode
                   ),tv=tv_uld
                   +tv_ht
                   +tv_ut
                   +tv_airism
                   +tv_jeans
                   +tv_down_and_uld_and_wool,
                   tv.ret = retention(tv,0.75,tcode
                   ),search=search_ht
                   +search_uld_and_down,
                   search.ret = retention(search,0.75,tcode
                   ),ooh=ooh_branding
                   +ooh_flannel
                   +ooh_ht
                   +ooh_jeans
                   +ooh_jogger_and_jeans
                   +ooh_knit
                   +ooh_linen
                   +ooh_sport_and_flannel
                   +ooh_sports
                   +ooh_uld
                   +ooh_ut
                   +ooh_ut_and_linen,
                   ooh.ret = retention(ooh,rate=0.75,code=tcode
                   ),magazine=magazine_jeans
                   +magazine_knit
                   +magazine_uld
                   +magazine_kids
                   +magazine_pr
                   +magazine_linen
                   +magazine_polo,
                   magazine.ret = retention(magazine,rate=0.75,code=tcode
                   ),
                   storecount=LFL_countStr+new_countStr+others_countStr,
                   discphh=Disc/hh)
  return(tsales)
}

#################################################################################################
############################ Model by Model######################################################
#################################################################################################

pie5 <- c(	0.07550624	,
           0.000180903	,
           0.002114839	,
           0.003225364	,
           2.9646E-05	,
           0.017969077	,
           0.001440313	)


cats <- unique(sales$cate)
cats <- cats[-grep('FW',cats)]
rlt2 <- list()

###########################
# Manual Modeling
###########################


#Setup
i <- 10
header=strsplit('coef	tperiod	year2014	year2015	year2016','\t')[[1]]

#filter data period
data <- modelfile(cats[[i]])
aggplot(data$Qty,data$week)
# data <- filter(data,week>=20150000)

rlti <- rlt_list
rlti <- (rlt_list[[which(names(rlt_list)==cats[[i]])]][[1]][,c(9,13),drop=F])
holdout <- cbind(1,as.matrix(data[,match(rownames(rlti),colnames(data))]))

#Round1 Residual Model with adjustment from conspie
x.lm1 <- lm(qphh~city+month+avg_temp+storecount,data=data)
aggplot(data$Qty,data$week);agglines(predict(x.lm1)*data$hh,data$week)
fit1 <- as.numeric(predict(x.lm1))
res1 <- as.numeric(x.lm1$residuals)
res.lm <- lm(res1~holdout-1)
res.coef <- coef(res.lm)
res.coef[-1] <- ifelse(res.coef[-1]<0,0,res.coef[-1])
cons.coef <- c(res.coef[1:4],pie5/(colSums((holdout*data$hh)[data$week>=20160000,-1:-4])/sum(data$Qty[data$week>=2016])))
cons.coef[-1] <- ifelse(cons.coef[-1]<0,0,cons.coef[-1])

x.coef <- (cons.coef+res.coef)/2
res.driven <- (outer(data$hh,x.coef,"*") * holdout)[,-1]
if(length(unique(data$year))==1){
  output <- cbind(coef=x.coef[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round(((apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
} else {
  output <- cbind(coef=x.coef[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round((t(apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
}

#Check discount value percentage
Discount_percentage <- sum(as.numeric(data$Disc))/sum(as.numeric(data$Amnt))
if(output[3,ncol(output)]>Discount_percentage * 0.8){
  x.coef[4] <- Discount_percentage * 0.8 / output[3,ncol(output)] * x.coef[4]
}
res.driven <- (outer(data$hh,x.coef,"*") * holdout)[,-1]
if(length(unique(data$year))==1){
  output <- cbind(coef=x.coef[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round(((apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
} else {
  output <- cbind(coef=x.coef[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round((t(apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
}

#Output for the first round
write.csv(output,'clipboard')
y.holdout <- rowSums(res.driven[,-1])/data$hh

#Again optim the coef and set the threshold range as [1/2,2]
data <- mutate(data,qphh2=qphh-y.holdout)
x.lm2 <- lm(data$qphh2~city+month+avg_temp,data=data)
fit2 <- as.numeric(predict(x.lm2))
res2 <- data$qphh - fit2
x.coef2 <- model(x.coef,Y=res2,X=holdout,betacons=c(0,rep(1,ncol(holdout)-1)))

x.coef2[-1:-4] <- ifelse(x.coef2[-1:-4]/x.coef[-1:-4]>2,x.coef[-1:-4]*2,x.coef2[-1:-4])
x.coef2[-1:-4] <- ifelse(x.coef2[-1:-4]/x.coef[-1:-4]<1/2,x.coef[-1:-4]/2,x.coef2[-1:-4])
res.driven <- (outer(data$hh,x.coef2,"*") * holdout)[,-1]

if((colSums(res.driven)/sum(data$Qty))[3]>Discount_percentage * 0.8){
  x.coef2[4] <- Discount_percentage * 0.8 / output[3,ncol(output)] * x.coef[4]
  res.driven <- (outer(data$hh,x.coef2,"*") * holdout)[,-1]
}

if(length(unique(data$year))==1){
  output <- cbind(coef=x.coef2[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round(((apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
  colnames(output)[3] <- 'year2016'
} else {
  output <- cbind(coef=x.coef2[-1],
                  tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                  round((t(apply(res.driven,2,function(x){
                    tapply(x,data$year,sum)
                  })/
                    as.numeric(tapply(data$Qty,data$year,sum)))),10))
}
rownames(output) <- paste(cats[[i]],rownames(output),sep=" ")
output <- output[,match(header,colnames(output))]
colnames(output) <- header
write.csv(output,'clipboard')

#An overfloating fitchart
y.holdout <- rowSums(res.driven[,-1:-3])/data$hh
data <- mutate(data,qphh3=qphh-y.holdout)
fitf <- predict(lm(qphh3~city+month+avg_temp+storecount+pris+Disc_Ratio,data=data))
aggplot(data$Qty,data$week,main=cats[[i]])
agglines((fitf + y.holdout)*data$hh,data$week)
output
print(sum(as.numeric(data$Disc))/sum(as.numeric(data$Amnt))) #check discount contribution

