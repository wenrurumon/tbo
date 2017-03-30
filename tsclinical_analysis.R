rm(list=ls())

#############################
#Create Dummy Variables

dummy <- as.data.frame(matrix(0,100,12))
colnames(dummy) <- strsplit(
  "patient,claim,date,zhubing1,zhubing2,zhubing3,fubing1,fubing2,fubing3,fubing4,gender,bp",',')[[1]]
dummy[,1] <- sample(1:60,size=100,T)
dummy[,2] <- 1:100
dummy[,3] <- sample(20,size=100,T)
dummy[,4] <- sample(1:3,size=100,T)
dummy[,5:6] <- t(sapply(1:100,function(x){
  out <- sample(c(0,0,0:3),2)
  out[out==dummy[x,4]] <- 0
  out
}))
dummy[,7] <- sample(1:4,size=100,T) 
dummy[,8:10] <- t(sapply(1:100,function(x){
  out <- sample(c(0,0,0,0,0:4),3)
  out[out==dummy[x,7]] <- 0
  out
}))
dummy[,11] <- sample(1:2,100,T)
dummy[,12] <- runif(100,0.5,0.8)

#############################
#Recode specific variable

dummy$bp <- floor(dummy$bp/0.2)

#############################
#Generate matrix description

code <- lapply(4:ncol(dummy),function(i){
  data.frame(row=paste('sample',dummy$patient,sep=":"),
             col=paste(colnames(dummy)[i],dummy[,i],sep=":"),
             date=dummy$date)
})
code <- do.call(rbind,code)
code$col <- gsub("zhubing1|zhubing2|zhubing3","zhubing",code$col)
code$col <- gsub("fubing1|fubing2|fubing3|fubing4","fubing",code$col)

code <- code %>% dplyr::group_by(row,col) %>% dplyr::summarise(mindate=min(date),maxdate=max(date))
rowid <- unique(code$row)
colid <- unique(code$col)

#############################
#Get sparse matrix

rlt <- slam::simple_triplet_matrix(match(code$row,rowid),match(code$col,colid),code$maxdate)
rlt <- as.matrix(rlt); dimnames(rlt) <- list(rowid,colid)

#############################
#Modeling
#To model Zhubing1

y <- rlt[,colnames(rlt)=='zhubing:1']
date_not_related_variable <- grep('gender',colnames(rlt))
x1 <- rlt[,date_not_related_variable,drop=F]
x2 <- rlt[,-date_not_related_variable,drop=F]
x2 <- x2-y; 
