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
## ���� 1. ����� ����� ������ �м�.
############################################################

getwd()
list.files()
useSejongDic()
target <- readLines("seoul_new.txt") #####���߿��� ���⸸ �ٲٸ� �ſ�.
target <- gsub("\\d+","", target)  ## ���ڸ� ���� ������.



target <- sapply(target, extractNoun,USE.NAMES = F)
target <- unlist(target)
myGsub <- readLines("myGsub.txt")

###�ǹ̾��� �� �����ϱ�
i <- 1  #myGsub�� length��ŭ ���ƶ�. �� �ȿ� �ִ� �ܾ�� ��ġ�� ���� ���ֹ���/
for (i in 1:length(myGsub)){
  target <- gsub(myGsub[i],"",target)
}

target <- Filter(function(x){!nchar(x)==1},target)  # '~��', '~��' �̷���
target <- Filter(function(x){nchar(x)<=10},target) # 10���� �̻��� �ǹ̾��ٰ� �Ǵ�

head(sort(target,decreasing=T),20) ##��带 ���ؼ� 20�� �ܾ, ���������Ͽ� ��.
write(unlist(target),"Seoul_new2.txt")   ## ���� ����
target <- read.table("seoul_new2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##������ ���ŵ� ���¿���, 

#### �ǹ̾��� �ܾ�� �������, myGsub���ٰ� �ٿ��־, �����������~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##�ܾ� �ּ� �ݺ��� ���� 
                     random.order = F, rot.per = 0.25, ##�������̷��� ����25%�ذ���
                     scale=c(5,1), colors = pal) #������ ����ũ��. �󵵼��� ��� 5~1



############################################################
## ���� 3. ���￩������ �м�.
############################################################

getwd()
list.files()
useSejongDic()
target <- readLines("seoul_go.txt") #####���߿��� ���⸸ �ٲٸ� �ſ�.
target <- gsub("\\d+","", target)  ## ���ڸ� ���� ������.

target <- sapply(target, extractNoun,USE.NAMES = F)
target <- unlist(target)
myGsub <- readLines("myGsub.txt")

###�ǹ̾��� �� �����ϱ�
i <- 1  #myGsub�� length��ŭ ���ƶ�. �� �ȿ� �ִ� �ܾ�� ��ġ�� ���� ���ֹ���/
for (i in 1:length(myGsub)){
  target <- gsub(myGsub[i],"",target)
}

target <- Filter(function(x){!nchar(x)==1},target)  # '~��', '~��' �̷���
target <- Filter(function(x){nchar(x)<=10},target) # 10���� �̻��� �ǹ̾��ٰ� �Ǵ�

head(sort(target,decreasing=T),20) ##��带 ���ؼ� 20�� �ܾ, ���������Ͽ� ��.
write(unlist(target),"seoul_go2.txt")   ## ���� ����
target <- read.table("seoul_go2.txt")
target <- table(target)
head(sort(target, decreasing=T),20)  ##������ ���ŵ� ���¿���, 

#### �ǹ̾��� �ܾ�� �������, myGsub���ٰ� �ٿ��־, �����������~
pal <- brewer.pal(8,"Dark2")

#set.seed(1234)
wordcloud::wordcloud(names(target), freq=target, min.freq=1, ##�ܾ� �ּ� �ݺ��� ���� 
                     random.order = F, rot.per = 0.25, ##�������̷��� ����25%�ذ���
                     scale=c(6,1), colors = pal) #������ ����ũ��. �󵵼��� ��� 5~1
