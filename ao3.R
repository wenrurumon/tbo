
rm(list=ls())
raw <- '肖战迫不及待的解开了王一博的衬衫 王一博也很主动的解开了肖战的纽扣
肖战一路从嘴唇吻到了胸膛
“待会你轻点”王一博娇滴滴的说着
“我会的”肖战说
王一博被人吻的气喘吁吁 把肖战推开后喘了几次
肖战让他喘了几口后便又吻了上去
王一博动了动膝盖表示了他的不满
“别急嘛”肖战说
王一博把手勾上了肖战的脖子 腿环在了肖战的腰上
加深了这个吻
肖战又一路从嘴唇吻到了脖子吻到了胸膛
王一博的那根早已高高立起 肖战舔了舔小王一博 下一秒就将他含在了嘴里 轻轻吮吸着头部 王一博受不了便将精液喷出 肖战把白稠吐到王一博的后穴上 后来便将两根手指插了进去 下体感到一阵疼痛的王一博浑身一抖
“肖战…”
“我会轻点的”
这个后穴多年没被开阔 一博疼的抓紧了床单 肖战抓住了一博的手 安慰了他
多年没被开阔 后穴被插入的他当然有点不习惯 肖战意识到了 便吻了吻他
扩张完毕 肖战把他的性器送入一博的后穴
感到一阵酥麻的一博把腰绷紧 仰起头来呻吟了一声
“嗯啊~”肖战每当送入一次 他就呻吟一次
“嗯！你轻点…啊~明天…我…还…要…上…班…嗯啊”王一博的话被撞击变得断断续续
“嗯！不要…禽兽…嗯！使劲！啊！嗯！嗯！嗯！”
“叫的小声点 静静听到了怎么办”肖战说
“没事的 他每次睡觉都睡得很熟…快点…继续”
肖战听到一博这么说当然是继续了
“嗯！啊！嗯！”王一博明显有点不满意 把自己的腰往上送了送
“嗯！再…使…劲 嗯！嗯！”
王一博小腹微微隆起 显然是肖战已经送得最深了
“不要这么深 肚子疼 嗯！”王一博对着肖战说道
肖战听到了王一博的请求 把那根出来了一点点
“怎么不继续了？”王一博疑惑的问
“你的腰不疼么？”肖战反问
“不疼 那个要等明天”王一博眨了眨眼睛
“明天腰疼 下不了床别怪我 你自己要的”
“嗯呢”说完 王一博勾住了肖战的脖子 又吻了上去'

id <- c('id1','id2')
raw <- gsub('肖战','id1',raw)
raw <- gsub('王一博|一博','id2',raw)
raw <- gsub('“','value_start',raw)
raw <- gsub('”','value_end',raw)

x <- strsplit(raw,'value_start')[[1]]
x <- lapply(x,function(x){
  x <- unlist(strsplit(x,'value_end'))
  if(length(x)==1){
    x <- list('',x)
  } else {
    list(x[1],x[2])
  }
  names(x) <- c('say','bk')
  x
}) %>% unlist

x <- x[!x%in%c('','\n')]
say <- which(names(x)=='say')
bk <- which(names(x)!='say')

#########

names(x)[say] <- sapply(say,function(i){
  bki <- bk-i
  names(which.min(nchar(c(sapply(id,function(idi){
    x <- strsplit(x[bk][(which(bki==max(bki[bki<0])))],idi)[[1]]
    x[length(x)]
  }),
  sapply(id,function(idi){
    x <- strsplit(x[bk][(which(bki==min(bki[bki>0])))],idi)[[1]]
    x[1]
  })))))
})

x <- strsplit(x,'\n') %>% unlist
names(x)[grepl('bk',names(x))] <- 'bk'
x <- x[x!='']

cbind(key=names(x),value=x)
