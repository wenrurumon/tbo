
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

#modelbymodel.R

write.csv(as.matrix(sales %>% group_by(cate,substr(week,1,4)) %>% summarise(sum(as.numeric(Qty)),sum(as.numeric(Amnt)))),'clipboard')

######################################
#The model
######################################

model_cati <- function(i,filteryear=0){
  
  #Setup
  
  header=strsplit('coef	tperiod	year2014_driven	year2015_driven	year2016_driven	year2014_pie	year2015_pie	year2016_pie','\t')[[1]]
  
  #filter data period
  data <- modelfile(cats[[i]])
  # aggplot(data$Qty,data$week)
  data <- filter(data,week>=(filteryear*10000))
  
  #Step 1 remove the seasonality impact on the response variable
  x.lm1 <- lm(qphh~city+month,data=data)
  # aggplot(data$Qty,data$week);agglines(predict(x.lm1)*data$hh,data$week)
  fit1 <- as.numeric(predict(x.lm1))
  res1 <- as.numeric(x.lm1$residuals)
  
  #Variables in the residual model
  holdout <- as.matrix(cbind(intecept=1,select(data,storecount,new_countStr,sku_count,pris,
                                               Disc_Ratio,
                                               digital.ret,pr.ret,ooh.ret,magazine.ret,tv.ret,otv.ret,search.ret)))
  res.lm <- lm(res1~holdout-1)
  res.coef <- coef(res.lm)
  
  #Align the contribution to the total level expectation
  res.coef[-1] <- ifelse(res.coef[-1]<0,0,res.coef[-1])
  cons.coef <- c(res.coef[1:6],
                 pie5/(colSums((holdout*data$hh)[data$week>=20160000,-1:-6])/sum(data$Qty[data$week>=2016])
                 ))
  cons.coef[-1] <- ifelse(cons.coef[-1]<0,0,cons.coef[-1])
  
  x.coef <- (cons.coef+res.coef)/2
  x.coef <- ifelse(is.infinite(x.coef),0,x.coef)
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
  
    #Check contribution for base variables and promotion
  Discount_percentage <- sum(as.numeric(data$Disc))/sum(as.numeric(data$Amnt))
  if(output[5,2]>Discount_percentage * 0.6){
    x.coef[6] <- Discount_percentage * 0.6 / output[5,2] * x.coef[6]
  }
  x.coef[2:5] <- ifelse(abs(output[1:4,ncol(output)])>1,x.coef[2:5]/abs(output[1:4,ncol(output)]),x.coef[2:5])
  
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
  # write.csv(output,'clipboard')
  y.holdout <- rowSums(res.driven[,-1])/data$hh
  
  #remove the seasonality with holdout excluded
  data <- mutate(data,qphh2=qphh-y.holdout)
  x.lm2 <- lm(data$qphh2~city+month,data=data)
  fit2 <- as.numeric(predict(x.lm2))
  res2 <- data$qphh - fit2
  
  #Optim the coeficient and control the difference between [1/2,2]
  x.coef2 <- model(x.coef,Y=res2,X=holdout,betacons=c(0,rep(1,ncol(holdout)-1)))
  x.coef2[-1:-6] <- ifelse(x.coef2[-1:-6]/x.coef[-1:-6]>2,x.coef[-1:-6]*2,x.coef2[-1:-6])
  x.coef2[-1:-6] <- ifelse(x.coef2[-1:-6]/x.coef[-1:-6]<1/2,x.coef[-1:-6]/2,x.coef2[-1:-6])
  
  #Check contribution for base variables and promotion
  res.driven <- (outer(data$hh,x.coef2,"*") * holdout)[,-1]
  if((colSums(res.driven)/sum(data$Qty))[5]>Discount_percentage * 0.6){
    x.coef2[6] <- Discount_percentage * 0.6 / (colSums(res.driven)/sum(data$Qty))[5] * x.coef2[6]
  }
  x.coef2[2:5] <- ifelse(abs((colSums(res.driven)/sum(data$Qty))[1:4])>1,x.coef2[2:5]/abs((colSums(res.driven)/sum(data$Qty))[1:4]),x.coef2[2:5])
  res.driven <- (outer(data$hh,x.coef2,"*") * holdout)[,-1]
  
  #output
  if(length(unique(data$year))==1){
    output <- cbind(coef=x.coef2[-1],
                    tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                    (apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })),
                    round(((apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })/
                      as.numeric(tapply(data$Qty,data$year,sum)))),10))
    colnames(output)[3:4] <- c('year2016_driven','year2016_pie')
  } else {
    output <- cbind(coef=x.coef2[-1],
                    tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                    t(apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })),
                    round((t(apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })/
                      as.numeric(tapply(data$Qty,data$year,sum)))),10))
    cname <- colnames(output)[grep('year',colnames(output))]
    cname[1:(length(cname)/2)] <- paste0(cname[1:(length(cname)/2)],'_driven')
    cname[-1:-(length(cname)/2)] <- paste0(cname[-1:-(length(cname)/2)],'_pie')
    colnames(output)[grep('year',colnames(output))] <- cname
  }
  rownames(output) <- paste(cats[[i]],rownames(output),sep=" ")
  output <- output[,match(header,colnames(output))]
  colnames(output) <- header
  # write.csv(output,'clipboard')
  
  #Input 20% max discounted value
  y.holdout <- rowSums(res.driven)/data$hh
  data <- mutate(data,qphh3=qphh-y.holdout)
  x.lm3 <- lm(qphh3~city+month+avg_temp+discphh,data=data)
  tprpie_overfit <- sum(coef(x.lm3)[grep('discphh',names(coef(x.lm3)))]*data$discphh*data$hh)/sum(data$Qty)
  tpr_maxup <- output[5,2]/3
  coef_tpr2 <- ifelse(tprpie_overfit>tpr_maxup,
                      coef(x.lm3)[grep('discphh',names(coef(x.lm3)))]/tprpie_overfit*tpr_maxup,
                      coef(x.lm3)[grep('discphh',names(coef(x.lm3)))])
  x.coef3 <- c(x.coef2,disc=coef_tpr2)
  res.driven <- cbind(res.driven,disc=coef_tpr2*data$discphh*data$hh)
  
  #Input avg_temp with dummies
  # y.holdout <- rowSums(res.driven)/data$hh
  # data <- mutate(data,qphh3=qphh-y.holdout)
  # x.lm3 <- lm(qphh3~city+month+avg_temp,data=data)
  # coef_temp <- coef(x.lm3)[length(coef(x.lm3))]
  # x.coef3 <- c(x.coef3,coef_temp)
  # res.driven <- cbind(res.driven,avg_temp=coef_temp*data$avg_temp*data$hh)
  
  #Input avg_temp without dummies
  y.holdout <- rowSums(res.driven)/data$hh
  data <- mutate(data,qphh3=qphh-y.holdout)
  x.lm3 <- lm(qphh3~city+month,data=data)
  coef_temp <- coef(lm(x.lm3$residuals~data$avg_temp))[2]
  x.coef3 <- c(x.coef3,coef_temp)
  res.driven <- cbind(res.driven,avg_temp=coef_temp*data$avg_temp*data$hh)
  
  #Output Finalized
  if(length(unique(data$year))==1){
    output <- cbind(coef=x.coef3[-1],
                    tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                    (apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })),
                    round(((apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })/
                      as.numeric(tapply(data$Qty,data$year,sum)))),10))
    colnames(output)[3:4] <- c('year2016_driven','year2016_pie')
  } else {
    output <- cbind(coef=x.coef3[-1],
                    tperiod=round((colSums(res.driven)/sum(data$Qty)),10),
                    t(apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })),
                    round((t(apply(res.driven,2,function(x){
                      tapply(x,data$year,sum)
                    })/
                      as.numeric(tapply(data$Qty,data$year,sum)))),10))
    cname <- colnames(output)[grep('year',colnames(output))]
    cname[1:(length(cname)/2)] <- paste0(cname[1:(length(cname)/2)],'_driven')
    cname[-1:-(length(cname)/2)] <- paste0(cname[-1:-(length(cname)/2)],'_pie')
    colnames(output)[grep('year',colnames(output))] <- cname
  }
  rownames(output) <- paste(cats[[i]],rownames(output),sep=" ")
  output <- output[,match(header,colnames(output))]
  colnames(output) <- header
  output <- output[c(1,2,3,4,14,5,13,6:12),]
  
  #decomp finalized
  dummy <- (predict(x.lm3)-coef_temp*data$avg_temp) * data$hh
  decomp <- cbind(dummy=dummy,res.driven)
  fit3 <- as.numeric(tapply(rowSums(decomp),data$week,sum))
  raw <- as.numeric(tapply(data$Qty,data$week,sum))
  decomp <- cbind(vol=data$Qty,val=data$Amnt,predvol=rowSums(decomp),decomp)
  weeklydecomp <- apply(decomp,2,function(x){
    tapply(x,data$week,sum)
  })
  rownames(weeklydecomp) <- paste(cats[[i]],rownames(weeklydecomp),sep="_")
  monthcode <- tapply(data$month,data$week,function(x){mean(as.numeric(x))})
  plot.ts(raw,main=cats[[i]]); lines(fit3,col=2)
  print(output)
  write.csv(output,'clipboard')

  list(
    output=output,decomp=decomp,weeklydecomp=weeklydecomp,
    rsq=summary(lm(raw~fit3))$r.square,
    tmape=summary(abs(fit3-raw)/raw),
    pmape=summary((abs(fit3-raw)/raw)[monthcode>=3&monthcode<=8])
  )
}

######################################
# Modeling
######################################

par(mfrow=c(3,4))
rlt <- list(
  model_cati(1),
  model_cati(2,2015),
  model_cati(3,2016),
  model_cati(4,2015),
  model_cati(5),
  model_cati(6,2016),
  model_cati(7,2015),
  model_cati(8),
  model_cati(9),
  model_cati(10)
)  

#output calc
write.csv(do.call(rbind,lapply(rlt,function(x){x[[1]]})),'clipboard')
#output fit summary
fit_summary <- t(sapply(rlt,function(x){do.call(c,x[4:6])}))
rownames(fit_summary) <- cats
write.csv(fit_summary,'clipboard')
#output decomp
write.csv(do.call(rbind,lapply(rlt,function(x){x$weeklydecomp})),'temp.csv')
