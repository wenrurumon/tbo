
rm(list=ls()) #程序环境初始化

################################
#导入数据部分
################################

setwd("/zfsauton/project/highmark/aska/training_rita/rawdata") #设置环境地址
library(data.table) #加载用于快速导入数据用的程序包
library(dplyr) #加载用于segmentation用的程序包

raw <- fread('0.txt',integer64 = 'numeric') #导入数据'0.txt'赋值为raw
#raw2 <- lapply(dir(),fread) #导入环境地址下的所有文件赋值为raw2

colnames(raw) <- c('cellphone','feedback_code','time','content','send_status','client') #对表头进行重命名

################################
#Segmentation
################################

seg_by_cellphone_test <- raw %>% group_by(cellphone) %>% summarise(n=n(),nclient=n_distinct(client)) 
#group by手机号计算n为手机号出现的次数，nclient为手机号被不同客户触达的次数
seg_by_cellphone <- filter(raw,send_status!='FAIL') %>% group_by(cellphone) %>% summarise(n=n(),nclient=n_distinct(client)) 
#只保留所有发送状态不为fail的记录，然后group by手机号计算n为手机号出现的次数，nclient为手机号被不同客户触达的次数
seg_by_cellphone <- arrange(seg_by_cellphone,desc(nclient)) #根据nclient从大到小对结果排序

filter(raw,cellphone==15117985463) #查看手机号为15117985463的所有记录
candidates <- filter(seg_by_cellphone,nclient>1)$cellphone #选取所有被大于一个客户触达的手机号，命名为vector
filter(raw,cellphone%in%candidates) #查看所有手机号包含在candidates中的所有记录
