library(rJava)
library(DBI)
library(RJDBC)
library(XML)
library(memoise)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rvest)
library(RColorBrewer)
library(data.table)
library(reshape)

getwd()
setwd("C:\\Users\User\project180728")
KoNLP::useSejongDic()
dSeoulNew <- readLines("seoul_new.txt");
class(dSeoulNew)
dSeoulNew <- sapply(dSeoulNew, extractNoun, USE.NAMES = T)  
#명사추출함.
class(dSeoulNew)
dSeoulNew <- gsub("\\d+","",dSeoulNew)
head(dSeoulNew)
dSeoulNew <- unlist(dSeoulNew)
dSeoulNew <- unlist(dSeoulNew)
write(dSeoulNew, "seoul_new2.txt") ###공백 제거된, 1차 정제된 메모 파일 생성.
dSeoulNew <- read.table("seoul_new2.txt")
head(dSeoulNew)



class(dSeoulNew)
dSeoulNew <- gsub("\\d+","",dSeoulNew)   ##\\d: decimal(숫자뜻함)을 없앤다. 분석대상X
head(dSeoulNew)



place <- sapply(txt,KoNLP::extractNoun,USE.NAMES = F);place

temp <- unlist(place)
place <- Filter(function(x) (nchar(x)>=3),txt)

place <- gsub("한반도","", place);place
#write(unlist(place))
test <- brewer.pal(9,place)
wordcloud(names(table(place)),
          freq = table(place),
          scale = c(5,1),
          ro.per = 0.25,
          min.freq = 1,
          random.order = F,
          random.color = T,
          colors = palette)
# 
# x = file("seoul.txt")
# readLines(x,n=4)
# 
# y = read.table('https://terms.naver.com/entry.nhn?docId=574440&cid=46618&categoryId=46618',sep=' ')
# y[1:10,]
