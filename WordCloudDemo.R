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
library(stringr)
############################################################
## 문제 1. 서울시 응답소 페이지 분석.
############################################################

getwd()
list.files()
useSejongDic()
target <- readLines("seoul_new.txt") #####나중에는 여기만 바꾸면 돼요.
target <- gsub("\\d+","", target)  ## 숫자를 먼저 제거함.



target <- sapply(target, extractNoun,USE.NAMES = F)
target <- unlist(target)
myGsub <- readLines("myGsub.txt")

###의미없는 값 제거하기
i <- 1  #myGsub에 length만큼 돌아라. 그 안에 있는 단어들 겹치는 내용 없애버림/
for (i in 1:length(myGsub)){
  target <- gsub(myGsub[i],"",target)
}

target <- Filter(function(x){!nchar(x)==1},target)  # '~한', '~는' 이런거
target <- Filter(function(x){nchar(x)<=10},target) # 10글자 이상은 의미없다고 판단

head(sort(target,decreasing=T),20) ##헤드를 통해서 20개 단어만, 역순정렬하여 봄.
write(unlist(target),"Seoul_new2.txt")   ## 공백 제거
target <- read.table("seoul_new2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##공백이 제거된 상태에서, 

#### 의미없는 단어다 싶으며는, myGsub에다가 붙여넣어서, 제거해줘야해~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##단어 최소 반복수 조건 
                     random.order = F, rot.per = 0.25, ##예뻐보이려구 각도25%준거임
                     scale=c(5,1), colors = pal) #스케일 글자크기. 빈도수에 띠라 5~1



############################################################
## 문제 3. 서울여행지지 분석.
############################################################

getwd()
list.files()
useSejongDic()
target <- readLines("seoul_go.txt") #####나중에는 여기만 바꾸면 돼요.
target <- gsub("\\d+","", target)  ## 숫자를 먼저 제거함.

target <- sapply(target, extractNoun,USE.NAMES = F)
target <- unlist(target)
myGsub <- readLines("myGsub.txt")

###의미없는 값 제거하기
i <- 1  #myGsub에 length만큼 돌아라. 그 안에 있는 단어들 겹치는 내용 없애버림/
for (i in 1:length(myGsub)){
  target <- gsub(myGsub[i],"",target)
}

target <- Filter(function(x){!nchar(x)==1},target)  # '~한', '~는' 이런거
target <- Filter(function(x){nchar(x)<=10},target) # 10글자 이상은 의미없다고 판단

head(sort(target,decreasing=T),20) ##헤드를 통해서 20개 단어만, 역순정렬하여 봄.
write(unlist(target),"seoul_go2.txt")   ## 공백 제거
target <- read.table("seoul_go2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##공백이 제거된 상태에서, 

#### 의미없는 단어다 싶으며는, myGsub에다가 붙여넣어서, 제거해줘야해~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##단어 최소 반복수 조건 
                     random.order = F, rot.per = 0.25, ##예뻐보이려구 각도25%준거임
                     scale=c(6,1), colors = pal) #스케일 글자크기. 빈도수에 띠라 5~1

