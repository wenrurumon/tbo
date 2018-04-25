rm(list=ls())

g <- 
'Gene_order	Gene_name	SNP_order	Disease	Pvalue
27	2::LINC01123	1	Diabetes	0
53	2::ZBTB45P1	1	Diabetes	0
20	2::IGKV6D-21	1	AD	0
27	2::LINC01123	1	AD	0
53	2::ZBTB45P1	1	AD	0
218	5::GTF2H2C	1	Diabetes	0
218	5::GTF2H2C	2	Diabetes	0
218	5::GTF2H2C	1	AD	0
218	5::GTF2H2C	2	AD	0
230	5::NAIP	1	Diabetes	0
239	5::RN7SL9P	1	Diabetes	0
247	5::RP11-497H16.5	1	Diabetes	0
248	5::RP11-497H16.6	2	Diabetes	0
256	5::SERF1B	1	Diabetes	0
256	5::SERF1B	2	Diabetes	0
260	5::SMN2	2	Diabetes	0
260	5::SMN2	3	Diabetes	0
230	5::NAIP	1	AD	0
239	5::RN7SL9P	1	AD	0
247	5::RP11-497H16.5	1	AD	0
248	5::RP11-497H16.6	2	AD	0
256	5::SERF1B	1	AD	0
256	5::SERF1B	2	AD	0
256	5::SERF1B	3	AD	0
260	5::SMN2	1	AD	0
260	5::SMN2	2	AD	0
260	5::SMN2	3	AD	0
471	7::PHBP6	2	Diabetes	0
505	8::ABC12-47043100G14.2	2	AD	0
541	8::RANP9	2	AD	0
591	9::RN7SL763P	1	AD	0
593	9::RNU6-156P	1	AD	0
609	9::RP11-216M21.1	1	Diabetes	0
604	9::RP11-15J10.1	2	AD	0
614	9::RP11-318K12.2	1	AD	0
643	9::ZNF658B	1	AD	0
685	12::RP11-359J14.2	3	Diabetes	0
694	12::RP11-972P1.8	1	AD	0
824	15::HERC2P9	2	AD	0
867	16::AC009133.21	1	AD	0
886	16::MIR3179-2	1	AD	0
942	17::AC132872.2	3	Diabetes	0
929	16::ZG16	1	AD	0
942	17::AC132872.2	3	AD	0
977	17::KYNUP3	2	Diabetes	0
985	17::RN7SL606P	1	AD	0
1020	17::TBC1D3P1	1	AD	0
1020	17::TBC1D3P1	2	AD	0
1032	18::RNU6-702P	3	AD	0
1105	19::MIR1470	1	Diabetes	0
1105	19::MIR1470	2	Diabetes	0
1105	19::MIR1470	3	Diabetes	0
1051	10::RP11-508M1.7	1	AD	0
1151	1::AL021920.1	1	AD	0
1158	1::AL590452.1	1	AD	0
1183	1::EIF1AXP1	1	AD	0
1187	1::FAM231B	1	AD	0
1228	1::PFN1P6	1	Diabetes	0
1231	1::PRAMEF11	1	AD	0
1294	1::RP5-845O24.8	1	AD	0
1346	21::RNU6-614P	1	AD	0
1464	2::AC108479.1	1	Diabetes	0
1467	2::AC114730.3	3	Diabetes	0'
g <- do.call(rbind,(strsplit(strsplit(g,'\n')[[1]],'\t')))[,2]
g <- unique(g)[-1]

setwd('/Users/wenrurumon/Documents/uthealth/data')
load('cutdata.rda')
library(data.table)
library(reshape2)
library(cca)

pid <- rownames(data.expr[[1]])
did <- rownames(data.dis[[1]])
data.d2 <- lapply(data.dis,function(x){
  x[match(pid,did),,drop=F]
})

# names.snp <- gsub(':','',substr(names(data.snp),3,nchar(names(data.snp))))
names.snp <- names(data.snp)
snp <- data.snp[names.snp%in%g]

test <- t(sapply(snp,function(x){
  sapply(data.d2,function(y){
    ccap(x,y)
  })
}))
colnames(test) <- sapply(data.dis,colnames)
test <- melt(test)
library(dplyr)

filter(test,value<0.05)
