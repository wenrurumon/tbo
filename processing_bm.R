
##########################
#初始化设置
##########################

rm(list=ls()) #清理内存
library(rjson) #导入RJSON的代码库
setwd('C:/Users/WenluluSens/Documents/qianji/beimi/data') #设置工作文件夹

tryapply <- function(x,fun,fun2=lapply){
  fun3 <- function(x){
    rlt <- try(fun(x))
    if(class(rlt)=="try-error"){
      return(NULL)
    } else {
      return(rlt)
    }
  }
  rlt <- fun2(x,fun3)
  rlt[!sapply(rlt,is.null)]
}
lcode<- function(x){
  for(i in 1:length(x)){
    if(is.null(x[[i]])){next}
    x[[i]] <- cbind(i,x[[i]])
  }
  return(do.call(rbind,x))
}
#这句先别管

##########################
#读取原始数据
##########################

fs <- dir() #dir函数用于读取当前文件夹下的所有文件文件名，赋值为fs
json2r <- function(x){
  x <- readLines(x,encoding='UTF-8')
  fromJSON(x)
} 
#创造了一个函数，给定文件名x，第一步读取x中的所有内容，第二步json格式的内容改为Rlist格式

rawdata <- tryapply(fs,json2r)

#########################
#数据overview
#########################

credit <- sapply(rawdata,function(x){
  ifelse(is.null(x$credit$zmxyScore),0,x$credit$zmxyScore)
})
basic <- cbind(t(sapply(rawdata,function(x){x$basic})),credit)
call <- lcode(lapply(rawdata,function(x){do.call(rbind,x[[2]][[3]])}))
record <- lcode(lapply(rawdata,function(x){do.call(rbind,x$existingCustomer)}))
beh <- lcode(lapply(rawdata,function(x){do.call(rbind,x$behavior)}))
contacts <- lcode(lapply(rawdata,function(x){do.call(rbind,x$contacts)}))
apps <- sapply(rawdata,function(x){paste(unique(paste(unlist(x$apps))),collapse=';')})
setwd('..')
setwd('sample')

write.csv(basic,'basic.csv')
write.csv(call,'call.csv')
write.csv(record,'record.csv')
write.csv(beh,'beh.csv')
write.csv(contacts,'contacts.csv')
write.csv(apps,'apps.csv')
