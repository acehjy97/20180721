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
#plot(x축 데이터, y축 데이터)
plot(1:3,6:4)

##x축 y축 한계값S
plot(1:3,6:4, xlim = c(1,5), ylim = c(1,10)) 

##x축 y축 축 제목, 그래프 제목 지정해서 출력.
plot(1:3,6:4, xlim = c(1,5), ylim = c(1,10), 
     xlab ="x축 값", ylab = "y축값", main = "TST")    #lab = label

plot.new() #기존 그래프 지우고 새로 그리기
dev.new() # 새창에서 띄우



##꺾은선 차트
Vdemo <- rnorm(5,100,5); Vdemo
plot(Vdemo,type="o",col="red", ylim = c(0,200), 
     axes = F, ann = F) ##axes F면 축을 숨김. ann F면 축의 이름 정하지 않음

axis(1, 1:5, lab = c("월", "화", "수", "목", "금"))
axis(2, ylim = c(0,100))

title(main="과일",col.main ="red", fon.main=4) #제목, 제목색, 글자크기
titile(xlab = "요일", col.lab = "black")
title(ylab="가격",col.lab="green")


#그래프를 배치해서 보여주기
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


#이전 싧습에서 3개로 출력한 것을 1개로 만들기 위한 예제.
par(mfrow = c(1,1))
t1 <- c(1,2,3,4)
t2 <- c(5:1)
t3 <- c(3:7)
plot(t1, type = 's', col = 'red', ylim=c(1,5))
par(new=T) ###중복 허용
plot(t2, type = 'o', col = 'blue', ylim = c(1,5))


## 새로 그려질 때마다, x축 제목과 ylim값이 새롭게 적용.
## 따라서, 아래와 같이 linew()를 사용해 보다 쉽게 그리는 방법 권장.
plot.new
par(mfrow = c(1,1))
t1 <- c(1,2,3,4)
t2 <- c(5:1)
t3 <- c(3:7)
plot(t1, type = 's', col = 'red', ylim=c(1,5))
lines(t2, type = 'o', col = 'blue', ylim = c(1,5))
lines(t3, type = 'l', col = 'orange', ylim = c(1,5))

legend(3.4,5, c("국","영","수"),cex=0.9,
       col=c("red","blue","orange"),lty=1) #x축 위치 #y축 위치 #글자크기


## 막대 그래프 그리기
# runif(개수, 시작값, 끝값) : getting random value
Vdemo <- round(runif(5,1,5),1);Vdemo
barplot(Vdemo, horiz = T)


## 그룹으로 묶어서 출력
v1 <- c(5:2);v2 <- c(5,3)
v3 <- c("green","yellow")
m1 <- matrix(v1,2,2) #2행 2열의 매트릭스; 
m1
barplot(m1, beside = T, names=v2, col= v3)


## 그룹으로 묶어서 가로 출력
plot.new
v1 <- c(5:2); v2 <- c("before","after")
v3 <- c("orange","purple")
v4 <- c(1,0.5,1,0.5)
m1 <- matrix(v1,2,2)
par(oma = v4) ##그래프 여백 6시, 9시, 12시, 3시 방향

barplot( m1, beside = T, names = v2, col = v3, horiz = T)


## bar에 두개의 값을 합서
plot.new
v1 <- c(5:2); v2 <- c("before","after")
v3 <- c("orange","purple")
v4 <- c(0,12)
m1 <- matrix(v1,2,2)

barplot(m1, xlim = v4, names = v2, col = v3, horiz = T)


## 과일 가격 응용
VBanana <- round(runif(5,100,180),0)
VCherry <- round(runif(5,100,180),0)
VOrange <- round(runif(5,100,180),0)
dFruit <- data.frame(바나나= VBanana, 체리= VCherry, 오렌지 =VOrange)
Vylim <- c(0,200)
Vday <- c("월","화","수","목","금")
dFruit

barplot(as.matrix(dFruit), main="과일판매량", beside=T,
        col = rainbow(nrow(dFruit)),ylim = Vylim)
legend(16.3,200, Vday, cex = 0.8, fill=rainbow(nrow(dFruit)))
##무지개 색으로 하되 개수는 dFruit. 변수안에 있는 값의 개수만큼 하라.


## 하나의 바에 값을 합치기
barplot(t(dFruit), main = "과일판매량", 
        col=rainbow(nrow(dFruit)), ylim = c(0,550), 
        space = 0.1, cex.axis = 0.8, las = 1,
        names.arg = Vday, cex = 0.8) ### t() 전치함수 : 행과 열 위치 전환
legend(0,550, names(dFruit), cex = 0.8, fill=rainbow(nrow(dFruit)))

## 
Vpeach <- round(runif(5,150,250),0);Vpeach
Vday <- c("월","화","수","목","금")
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
barplot(Vpeach, main="복숭아판매량", names.arg =Vday, col= Vcolor)


##
VHeight1 <- round(runif(2,150,160))
VHeight2 <- round(runif(3,161,170))
VHeight3 <- round(runif(3,171,180))
VHeight4 <- round(runif(2,181,190))
VHeight <- c(VHeight1, VHeight2, VHeight3, VHeight4);VHeight


## 히스토그램
hist(VHeight, breaks=8 ,main="학생 키 분포")
barplot(VHeight, main="학생 키 분포")


## 파이 차트
plot.new
Vpie <- c(10,20,30,40)
pie( Vpie, radius = 1)  
pie(Vpie, radius =1, init.angle=90) ## 시작각도를 90도로 지정. 12시에서 시작.


# 색깔과 라벨명 지정 (그대로 옵션만 추가추가)
plot.new
Vpie <- c(10,20,30,40)
pie(Vpie, radius =1, init.angle=90, col=rainbow(length(Vpie)), label=Vday) 


## 
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("월","화","수","목","금")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie(Vpie, radius =1, init.angle=90, col=rainbow(length(Vpie)), label=Vlab) 
legend(1.6,1,Vday, cex=0.9, fill=rainbow(length(Vpie)))


## 
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("월","화","수","목","금")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie(Vpie, radius =1, init.angle=90, col=c("red","orange","yellow","green", "blue"), label=Vlab) 
legend(1.6,1,Vday, cex=0.9, fill=c("red","orange","yellow","green", "blue"))


###
install.packages("plotrix")
library(plotrix)
plot.new
Vpie <- c(10,20,30,40)
Vday <- c("월","화","수","목","금")
Vpct <- round(Vpie/sum(Vpie)*100,1)
Vlab= paste(Vpct, " %")
pie3D(Vpie, main="3D Pie Chart",col=rainbow(length(Vpie)),cex=0.5, labels=Vlab,
      explode = 0.05)
legend(-0.8,0.85,Vday, cex=0.7, fill=rainbow(length(Vpie)))

