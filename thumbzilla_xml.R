library(XML)
setwd('E:/spider')
value <- function(x){sapply(x,xmlValue)}

test <- lapply(1:20,function(i){

  print(paste(i,Sys.time()))
  
  url <- paste0('http://www.thumbzilla.com/newest?page=',i)
  html <- readLines(url)
  html <- htmlParse(html)
  
  movies <- getNodeSet(html,'//*[@id="content"]//*[@class="js-thumb"]')
  title <- getNodeSet(movies[[1]],'//*[@class="title"]')
  duration <- getNodeSet(movies[[1]],'//*[@class="duration"]')
  rating <- getNodeSet(movies[[1]],'//*[@class="hoverInfo videos"]')
  rating <- t(sapply(value(rating),function(x){
    x <- strsplit(x,'\n')[[1]][2:3]
    x <- gsub('\t','',x)
  }))
  colnames(rating) <- c('views','rating')
  rownames(rating) <- NULL
  href <- sapply(movies,function(x) xmlGetAttr(x,'href'))
  slidei <- data.frame(title=value(title),duration=value(duration),rating,href)
  
  urls <- paste0('http://www.thumbzilla.com',slidei$href)
  info <- t(sapply(urls,function(urli){
    htmli <- htmlParse(readLines(urli))
    x <- gsub('\t|Expand','',value(getNodeSet(htmli,'//*[@id="infoSection"]/section/div')))
    x <- gsub('\n',' ',x)
    x <- substr(x,3,nchar(x))
    x <- strsplit(x,'  ')[[1]]
    star <- grep('Stars:',x,value=T)
    cate <- grep('Categories:',x,value=T)
    tag <- grep('Tags:',x,value=T)
    x <- c(star=star,cate=cate,tag=tag)
    x <- x[match(c('star','cate','tag'),names(x))]
    substr(x,regexpr(':',x)+2,nchar(x))
  }))
  dimnames(info) <- list(NULL,c('star','cate','tag'))
  slidei <- data.frame(slidei,info)
  slidei
})
print(Sys.time())

temp <- test[[1]]
for ( i in test[-1]){
  temp <- rbind(temp,i)
}
write.csv(temp,paste0('thbzl_new_',format(Sys.time(),'%Y%m%d'),'.csv'),row.names=FALSE)
