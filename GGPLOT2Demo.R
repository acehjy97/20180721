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
## ggplot# ggplot2 문법 구성 기본요소
# 데이터 프레임 data.frame
# 색상, 크기 같은 기하학적 요소 aes
# 점, 선, 모양 같은 기하학적 요소 geoms
# 통계적 처리방법 stats
# aes 에서 사용할 스케일 (scale)
## ggplot(df, aes(x=val, y=val))+geom  : 여기서 +가 dplyr에서의 %>% 역할.

getwd()
list.files()
tkorean <-read.table("학생별국어성적.txt", header = T, sep=",") 
##dplyr랑 같죠 방식이
ggplot2::ggplot(tkorean, aes(x = 이름, y = 점수)) + geom_point()


tkorean <-read.table("학생별국어성적.txt", header = T, sep=",") 
## + 앞까지는 똑같저.
ggplot2::ggplot(tkorean, aes(x = 이름, y = 점수)) + geom_bar(
  stat = "identity", fill="orange",color="dark green"
) + theme( axis.text.x = element_text(
  angle = 25, hjust = 1, vjust=1, color ="dark green", size=8
  )
)
 

###########################EXCEL 파일################################
tscores <- read.csv("학생별과목별성적_국영수.csv", header = T);tscores
tscores <- arrange(tscores,이름,과목);tscores
install.packages("plyr")
library(plyr)
tscores <- ddply(tscores, "이름", transform, 누적합계 =cumsum(점수), 
                 label = cumsum(점수)-0.5*점수)  ;tscores

ggplot2::ggplot(tscores, aes(x=이름, y=점수, fill=과목)) + 
  geom_bar(stat="identity") + 
  geom_text(
    aes(y=label, label=paste(점수,"점")),color="black",size=4) +
  guides(fill=guide_legend(reverse=T)
         ) +
  theme(axis.text.x = element_text(
    angle=25, hjust=1, vjust=1, color="black", size=8
  ))


##########NEWFILE t
list.files()
tAllScores <- read.table("학생별전체성적.txt" ,header=T,sep=",");
tAllScores[,c("이름","영어")]   ##Change the form into the form we know.
