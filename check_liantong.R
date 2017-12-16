rm(list=ls())
setwd('C:\\Users\\admin\\Documents\\liantong\\demo')
library(data.table)
library(dplyr)

setwd('shanghai9')
f <- dir(pattern='csv')
x <- lapply(f,fread)
names(x) <- f
data <- x

#######################################################

x <- data[[1]]
x <- dplyr::select(x,homeid,date_month,agegroup,gender,pop_num)
x.gender <- x %>% group_by(gender) %>% summarise(n = sum(as.numeric(pop_num)))
x.age <- x %>% group_by(agegroup) %>% summarise(n = sum(as.numeric(pop_num)))
x.home <- x %>% group_by(homeid) %>% summarise(n = sum(as.numeric(pop_num)))
head(x)
x1 <- list(x.gender,x.age,x.home)
lapply(x1,head)
sum(x.home$n)

########################################################

x <- data[[2]]
x <- dplyr::select(x,homeid,jobid,consumeid,pop_num,date_month,city)
x.home <- x %>% group_by(homeid) %>% summarise(n = sum(as.numeric(pop_num)))
x.job <- x %>% group_by(jobid) %>% summarise(n = sum(as.numeric(pop_num)))
x.con <- x %>% group_by(consumeid) %>% summarise(n = sum(as.numeric(pop_num)))
head(x)
x2 <- list(x.home,x.job,x.con)
lapply(x2,head)
sum(x.home$n)

#########################

check_home <- data.table(x1[[3]],n2=x2[[1]]$n)
check_home
cor(check_home[,2:3])

#########################

x <- data[[3]]
x <- dplyr::select(x,date_hour,grid_id,pop_num)
x.hour <- x %>% group_by(date_hour) %>% summarise(n=sum(as.numeric(pop_num)),n2=n_distinct(grid_id))
as.data.table(x.hour)
x.hour %>% group_by(substr(date_hour,1,8)) %>% summarise(n=sum(n),n2=sum(n2))

#########################

x <- data[[4]][,-1]
x <- select(x,-objectid,-city,-create_time,-update_time,-date_month)
s <- function(...){sum(as.numeric(...))}
x.home <- x %>% group_by(homeid) %>% 
  summarise(h1=s(h08),h2=s(h08_21),h3=s(h21_23),h4=s(h23_24),h5=s(h24))
x.home <- mutate(x.home,n3=h1+h2+h3+h4+h5)
sum(x.home$n3)
check_home <- data.table(check_home,n3=x.home$n3)

######################


x <- data[[2]]
x <- dplyr::select(x,homeid,jobid,consumeid,pop_num,date_month,city)
x.home <- x %>% group_by(homeid) %>% summarise(n = sum(as.numeric(pop_num)))
x.job <- x %>% group_by(jobid) %>% summarise(n = sum(as.numeric(pop_num)))
x.con <- x %>% group_by(consumeid) %>% summarise(n = sum(as.numeric(pop_num)))
