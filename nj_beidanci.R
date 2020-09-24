
rm(list=ls())
library(rjson)
library(rvest)
library(data.table)
library(dplyr)

#https://www.koolearn.com/dict/fenlei_2_0_1.html

getbook <- function(x,y){paste0('https://www.koolearn.com/dict/tag_',x,'_',1:y,'.html')}
nj1 <- getbook(423,3)
nj2 <- getbook(424,2)
nj3 <- getbook(425,2)
nj4 <- getbook(426,2)
nj5 <- getbook(427,2)
nj6 <- getbook(428,2)
nj7 <- getbook(449,2)
nj8 <- getbook(450,2)
nj9 <- getbook(451,2)
nj10 <- getbook(452,2)
nj11 <- getbook(453,2)
nj12 <- getbook(454,2)
sj1 <- getbook(366,3)
lj1 <- getbook(382,2)
zs1 <- getbook(331,9)
zb1 <- getbook(332,4)

getjson <- function(urls){
  wordlist <- lapply(urls,function(urli){
    print(urli)
    x <- read_html(urli)
    x <- x %>% html_nodes('.word-wrap .word')
    words <- x %>% html_text()
    hrefs <- x %>% html_attr('href')
    x <- data.table(word=words,href=paste0('https://www.koolearn.com',hrefs))
    x
  })
  wordlist <- do.call(rbind,wordlist)
  test <- lapply(1:nrow(wordlist),function(i){
    x <- read_html(wordlist$href[i])
    wi <- wordlist$word[i]
    pns <- x %>% html_nodes('.word-title+ .content-box .details-content') %>% html_text()
    pns <- gsub(' |\n','',pns)
    prop <- x %>% html_nodes('.prop') %>% html_text()
    p <- x %>% html_nodes('.clearfix p') %>% html_text()
    list(wi,pns,prop,p)
  })
  test
}

test <- lapply(
  list(nj1=nj1,nj2=nj2,nj3=nj3,nj4=nj4,nj5=nj5,nj6=nj6,nj7=nj7,nj8=nj8,nj9=nj9,nj10=nj10,nj11=nj11,nj12=nj12,
       sj1=sj1,lj1=lj1,zs1=zs1,zb1=zb1)
  ,getjson)
write(toJSON(test),'test.json')
