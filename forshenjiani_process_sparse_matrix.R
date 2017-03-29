rm(list=ls())

#load data
setwd('C:\\Users\\WenluluSens\\Downloads')
library(data.table)
library(dplyr)
raw <- read.csv('towenwen.csv')
x <- select(raw,CI_ID,EACT_CD,BETOS_CD_CAT,PRI_DIAG_CD_CAT)

#description of the matrix
code <- lapply(2:ncol(x),function(i){
  cbind(row=paste(x$CI_ID),col=paste(colnames(x)[i],x[,i],sep=":"))
})
code <- unique(do.call(rbind,code))

#generate dimnames of the final matrix
rowid <- unique(code[,1])
colid <- unique(code[,2])
row2tag <- match(code[,1],rowid)
col2tag <- match(code[,2],colid)
code2 <- cbind(row=row2tag,col=col2tag)

#generate sparse matrix
library(slam)
rlt <- as.matrix(slam::simple_triplet_matrix(row2tag,col2tag,rep(1,length(row2tag))))
dimnames(rlt) <- list(rowid,colid)

#Validate

test <- sapply(rowid,function(id){
  all(all.equal(sort(code[code[,1]==id,2]),sort(names(which(rlt[rownames(rlt)==id,]>0))))==TRUE)
})
all(test)
