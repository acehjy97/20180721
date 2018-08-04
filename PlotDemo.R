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
library(stringr)

VDemo <- c(1:5)
VDemo
plot(VDemo) ##DEFAULT : X, Y the same.
#plot(x�� ������, y�� ������)
plot(1:3,6:4)

##x�� y�� �Ѱ谪S
plot(1:3,6:4, xlim = c(1,5), ylim = c(1,10)) 

##x�� y�� �� ����, �׷��� ���� �����ؼ� ���.
plot(1:3,6:4, xlim = c(1,5), ylim = c(1,10), 
     xlab ="x�� ��", ylab = "y�ప", main = "TST")    #lab = label

plot.new() #���� �׷��� ����� ���� �׸���
dev.new() # ��â���� ���



##������ ��Ʈ
Vdemo <- rnorm(5,100,5); Vdemo
plot(Vdemo,type="o",col="red", ylim = c(0,200), 
     axes = F, ann = F) ##axes F�� ���� ����. ann F�� ���� �̸� ������ ����

axis(1, 1:5, lab = c("��", "ȭ", "��", "��", "��"))
axis(2, ylim = c(0,100))

title(main="����",col.main ="red", fon.main=4) #����, �����, ����ũ��
titile(xlab = "����", col.lab = "black")
title(ylab="����",col.lab="green")


#�׷����� ��ġ�ؼ� �����ֱ�
plot.new
Vdemo <- rnorm(5,100,5); Vdemo
par(mfrow = c(1,3))
plot(Vdemo,type="o")
plot(Vdemo,type="s")
plot(Vdemo,type="l")

# OPTION TYPE SUMMARY
# p -> point, default
# l -> line(b -> point + line)
# c -> at b, omit the points
# o -> overlap the lines and the points
# h -> vertical graph from the points to the x axes.
# s -> stair shaped graph which rely on the left points y value.
# n - > only the axes, no graph
# mfrow = c(x,y): control the arragement of the graphics
# x= row #, y = col #

plot.new
Vdemo <- rnorm(5,100,5); Vdemo
par(mfrow = c(1,3))
pie(Vdemo)
plot(Vdemo,type="o")
barplot(Vdemo)


## mgp = c(title loc, index_val loc, index_line loc)
par(mfrow = c(1,3))
par(mgp = c(0,1,2))
plot( Vdemo, xlab = "TEST")

par(mgp = c(3,1,0))
plot( Vdemo, xlab = "TEST")

par(mgp = c(3,4,2))
plot( Vdemo, xlab = "TEST")


## OMA OPTION. control the whole empty place in the graph
par(mfrow = c(1,3))
par(oma=c(3,0,0,1))
plot(Vdemo, xlab = "TEST")

par(oma=c(0,2,0,0))
plot(Vdemo, xlab = "TEST")


#���� ������� 3���� ����� ���� 1���� ����� ���� ����.
par(mfrow = c(1,1))
t1 <- c(1,2,3,4)
t2 <- c(5:1)
t3 <- c(3:7)
plot(t1, type = 's', col = 'red', ylim=c(1,5))
par(new=T) ###�ߺ� ���
plot(t2, type = 'o', col = 'blue', ylim = c(1,5))


## ���� �׷��� ������, x�� ����� ylim���� ���Ӱ� ����.
## ����, �Ʒ��� ���� linew()�� ����� ���� ���� �׸��� ��� ����.
plot.new
par(mfrow = c(1,1))
t1 <- c(1,2,3,4)
t2 <- c(5:1)
t3 <- c(3:7)
plot(t1, type = 's', col = 'red', ylim=c(1,5))
lines(t2, type = 'o', col = 'blue', ylim = c(1,5))
lines(t3, type = 'l', col = 'orange', ylim = c(1,5))

legend(3.4,5, c("��","��","��"),cex=0.9,
       col=c("red","blue","orange"),lty=1) #x�� ��ġ #y�� ��ġ #����ũ��


## ���� �׷��� �׸���
# runif(����, ���۰�, ����) : getting random value
Vdemo <- round(runif(5,1,5),1);Vdemo
barplot(Vdemo, horiz = T)


## �׷����� ��� ���
v1 <- c(5:2);v2 <- c(5,3)
v3 <- c("green","yellow")
m1 <- matrix(v1,2,2) #2�� 2���� ��Ʈ����; 
m1
barplot(m1, beside = T, names=v2, col= v3)


## �׷����� ��� ���� ���
plot.new
v1 <- c(5:2); v2 <- c("before","after")
v3 <- c("orange","purple")
v4 <- c(1,0.5,1,0.5)
m1 <- matrix(v1,2,2)
par(oma = v4) ##�׷��� ���� 6��, 9��, 12��, 3�� ����

barplot( m1, beside = T, names = v2, col = v3, horiz = T)


## bar�� �ΰ��� ���� �ռ�
plot.new
v1 <- c(5:2); v2 <- c("before","after")
v3 <- c("orange","purple")
v4 <- c(0,12)
m1 <- matrix(v1,2,2)

barplot(m1, xlim = v4, names = v2, col = v3, horiz = T)


## ���� ���� ����
VBanana <- round(runif(5,100,180),0)
VCherry <- round(runif(5,100,180),0)
VOrange <- round(runif(5,100,180),0)
dFruit <- data.frame(�ٳ���= VBanana, ü��= VCherry, ������ =VOrange)
Vylim <- c(0,200)
Vday <- c("��","ȭ","��","��","��")
dFruit

barplot(as.matrix(dFruit), main="�����Ǹŷ�", beside=T,
        col = rainbow(nrow(dFruit)),ylim = Vylim)
legend(16.3,200, Vday, cex = 0.8, fill=rainbow(nrow(dFruit)))
##������ ������ �ϵ� ������ dFruit. �����ȿ� �ִ� ���� ������ŭ �϶�.


## �ϳ��� �ٿ� ���� ��ġ��
barplot(t(dFruit), main = "�����Ǹŷ�", 
        col=rainbow(nrow(dFruit)), ylim = c(0,550), 
        space = 0.1, cex.axis = 0.8, las = 1,
        names.arg = Vday, cex = 0.8) ### t() ��ġ�Լ� : ��� �� ��ġ ��ȯ
legend(0,550, names(dFruit), cex = 0.8, fill=rainbow(nrow(dFruit)))

## 
Vpeach <- round(runif(5,150,250),0);Vpeach
Vday <- c("��","ȭ","��","��","��")
Vcolor <- c()

for(i in 1:length(Vpeach)){
 if(Vpeach[i] >= 200){
   Vcolor = c(Vcolor, "red")
 } else if(Vpeach[i] >= 180){
   Vcolor = c(Vcolor, "orange")
 } else {
   Vcolor = c(Vcolor, "purple")
 }
}
barplot(Vpeach, main="�������Ǹŷ�", names.arg =Vday, col= Vcolor)


##
VHeight1 <- round(runif(2,150,160))
VHeight2 <- round(runif(3,161,170))
VHeight3 <- round(runif(3,171,180))
VHeight4 <- round(runif(2,181,190))
VHeight <- c(VHeight1, VHeight2, VHeight3, VHeight4);VHeight


## ������׷�
hist(VHeight, breaks=8 ,main="�л� Ű ����")
barplot(VHeight, main="�л� Ű ����")


## ���� ��Ʈ
plot.new
Vpie <- c(10,20,30,40)
pie( Vpie, radius = 1)  
pie(Vpie, radius =1, init.angle=90) ## ���۰����� 90���� ����. 12�ÿ��� ����.


# ����� �󺧸� ���� (�״�� �ɼǸ� �߰��߰�)
plot.new
Vpie <- c(10,20,30,40)
pie(Vpie, radius =1, init.angle=90, col=rainbow(length(Vpie)), label=Vday) 


## 
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("��","ȭ","��","��","��")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie(Vpie, radius =1, init.angle=90, col=rainbow(length(Vpie)), label=Vlab) 
legend(1.6,1,Vday, cex=0.9, fill=rainbow(length(Vpie)))


## 
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("��","ȭ","��","��","��")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie(Vpie, radius =1, init.angle=90, col=c("red","orange","yellow","green", "blue"), label=Vlab) 
legend(1.6,1,Vday, cex=0.9, fill=c("red","orange","yellow","green", "blue"))


###
install.packages("plotrix")
library(plotrix)
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("��","ȭ","��","��","��")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie3D(Vpie, main="3D Pie Chart",col=rainbow(length(Vpie)),cex=0.5, labels=Vlab,
      explode = 0.05)
legend(-0.8,0.85,Vday, cex=0.7, fill=rainbow(length(Vpie)))
