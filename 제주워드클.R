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
## 문제 2. 제주 여행코스 분석.
############################################################

getwd()
list.files()
useSejongDic()
target <- readLines("jeju.txt") #####나중에는 여기만 바꾸면 돼요.
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
write(unlist(target),"jeju2.txt")   ## 공백 제거
target <- read.table("jeju2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##공백이 제거된 상태에서, 

#### 의미없는 단어다 싶으며는, myGsub에다가 붙여넣어서, 제거해줘야해~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##단어 최소 반복수 조건 
                     random.order = F, rot.per = 0.25, ##예뻐보이려구 각도25%준거임
                     scale=c(5,1), colors = pal) #스케일 글자크기. 빈도수에 띠라 5~1


## 이제는, 제목을 붙이겠다.
legend(0,0, "제주도 추천 여행코스 분석", cex=0.8, fill= NA, border = NA,
       bg = "white", text.col = "red", text.font = 2, box.col="red")


######################################################################
getwd()
list.files()
useSejongDic()
target <- readLines("jeju.txt") #####나중에는 여기만 바꾸면 돼요.
target <- gsub("\\d+","", target)  ## 숫자를 먼저 제거함.

target <- sapply(target, extractNoun,USE.NAMES = F)
target <- unlist(target)
myGsub <- readLines("제주도여행코스gsub.txt")

###의미없는 값 제거하기
i <- 1  #myGsub에 length만큼 돌아라. 그 안에 있는 단어들 겹치는 내용 없애버림/
for (i in 1:length(myGsub)){
  target <- gsub(myGsub[i],"",target)
}

target <- Filter(function(x){!nchar(x)==1},target)  # '~한', '~는' 이런거
target <- Filter(function(x){nchar(x)<=10},target) # 10글자 이상은 의미없다고 판단

head(sort(target,decreasing=T),20) ##헤드를 통해서 20개 단어만, 역순정렬하여 봄.
write(unlist(target),"jeju2.txt")   ## 공백 제거
target <- read.table("jeju2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##공백이 제거된 상태에서, 

#### 의미없는 단어다 싶으며는, myGsub에다가 붙여넣어서, 제거해줘야해~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##단어 최소 반복수 조건 
                     random.order = F, rot.per = 0.25, ##예뻐보이려구 각도25%준거임
                     scale=c(5,1), colors = pal) #스케일 글자크기. 빈도수에 띠라 5~1


## 이제는, 제목을 붙이겠다.
legend(0,0, "제주도 추천 여행코스 분석", cex=0.8, fill= NA, border = NA,
       bg = "white", text.col = "red", text.font = 2, box.col="red")



## 최빈 값 TOP40
top <- head(sort(target,decreasing = T),40)
top
top10 <- c("중문","주상절리","성산","천지연폭포","한라","산방",
           "섭지코지","송악산","신창","오설록")
top10count <- c(13,12,10,9,9,8,6,6,7,5);top10count

## 파이차트
plot.new
pie(top10count,radius=0.7,init.angle=90, col=rainbow(length(top10count)), label=top10)
legend(-1.2,1.05,top10, cex=0.7, fill=rainbow(length(top10count)))


## 산포도 그래프
plot(1:10,top10count, ylim = c(1,15), axes=F,
     xlab ="장소", ylab = "빈도", main = "제주관광")  
axis(1, 1:10, lab = c("중문","주상절리","성산","천지연폭포","한라","산방",
                      "섭지코지","송악산","신창","오설록"))
axis(2, ylim = c(0,15))

title(main="제주관광",col.main ="red", font.main=4) #제목, 제목색, 글자크기
title(xlab = "장소", col.lab = "black")
title(ylab="빈도",col.lab="green")


## 히스토그램
plot.new
hist(top10count, freq = T,ylim = c(1,15), 
     xlab ="장소", ylab = "빈도", main = "제주관광", axes=F)  
axis(1, 1:10, lab = c("중문","주상절리","성산","천지연폭포","한라","산방",
                      "섭지코지","송악산","신창","오설록"))
axis(2, ylim = c(0,15))

title(main="제주관광",col.main ="red", font.main=4) #제목, 제목색, 글자크기
title(xlab = "장소", col.lab = "black")
title(ylab="빈도",col.lab="green")


##막대그래프##
plot.new
barplot(top10count, horiz = F, col=rainbow(length(top10)))
axis(1, 1:10, lab = c("중문","주상절리","성산","천지연폭포","한라","산방",
                      "섭지코지","송악산","신창","오설록"))
axis(2, ylim = c(0,15))
legend(10,13,top10, cex=0.7, fill=rainbow(length(top10count)))


##꺾은선 차트##
plot.new
plot(1:10,top10count,type="o",col="red", ylim = c(0,15), 
     xlab ="장소", ylab = "빈도", main = "제주관광",
     axes = F, ann = F) ##axes F면 축을 숨김. ann F면 축의 이름 정하지 않음

axis(1, 1:10, lab = c("중문","주상절리","성산","천지연폭포","한라","산방",
                     "섭지코지","송악산","신창","오설록"))
axis(2, ylim = c(0,15))

title(main="제주관광",col.main ="red", font.main=4) #제목, 제목색, 글자크기
title(xlab = "장소", col.lab = "black")
title(ylab="빈도",col.lab="green")





#그래프를 배치해서 보여주기
plot.new
par(mfrow = c(1,1))
plot(top10count,type="o")
plot(top10count,type="s")
plot(top10count,type="l")
axis(1, 1:10, lab = c("중문","주상절리","성산","천지연폭포","한라","산방",
                      "섭지코지","송악산","신창","오설록"))
axis(2, ylim = c(0,15))

title(main="제주관광",col.main ="red", fon.main=4) #제목, 제목색, 글자크기
titile(xlab = "장소", col.lab = "black")
title(ylab="빈도",col.lab="green")
