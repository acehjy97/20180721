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
## ggplot# ggplot2 ���� ���� �⺻���
# ������ ������ data.frame
# ����, ũ�� ���� �������� ��� aes
# ��, ��, ��� ���� �������� ��� geoms
# ����� ó����� stats
# aes ���� ����� ������ (scale)
## ggplot(df, aes(x=val, y=val))+geom  : ���⼭ +�� dplyr������ %>% ����.

getwd()
list.files()
tkorean <-read.table("�л��������.txt", header = T, sep=",") 
##dplyr�� ���� �����
ggplot2::ggplot(tkorean, aes(x = �̸�, y = ����)) + geom_point()


tkorean <-read.table("�л��������.txt", header = T, sep=",") 
## + �ձ����� �Ȱ���.
ggplot2::ggplot(tkorean, aes(x = �̸�, y = ����)) + geom_bar(
  stat = "identity", fill="orange",color="dark green"
) + theme( axis.text.x = element_text(
  angle = 25, hjust = 1, vjust=1, color ="dark green", size=8
  )
)
 

###########################EXCEL ����################################
tscores <- read.csv("�л������񺰼���_������.csv", header = T);tscores
tscores <- arrange(tscores,�̸�,����);tscores
install.packages("plyr")
library(plyr)
tscores <- ddply(tscores, "�̸�", transform, �����հ� =cumsum(����), 
                 label = cumsum(����)-0.5*����)  ;tscores

ggplot2::ggplot(tscores, aes(x=�̸�, y=����, fill=����)) + 
  geom_bar(stat="identity") + 
  geom_text(
    aes(y=label, label=paste(����,"��")),color="black",size=4) +
  guides(fill=guide_legend(reverse=T)
         ) +
  theme(axis.text.x = element_text(
    angle=25, hjust=1, vjust=1, color="black", size=8
  ))


##########NEWFILE t
list.files()
tAllScores <- read.table("�л�����ü����.txt" ,header=T,sep=",");
tAllScores[,c("�̸�","����")]   ##Change the form into the form we know.