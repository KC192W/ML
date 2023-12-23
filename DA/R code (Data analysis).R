######R CODE (W1)######
#chapter 1.3
#Download package
library() 
install.packages("car")
library(car)
install.packages(c("openxlsx", "vcd"))
library(openxlsx)
###test1
install.packages(c("dplyr","lattice"))
library(dplyr)
library(lattice)
###test1

#Load and print data
table<-read.csv("C:/Users/user/Desktop/ML/DA/R_data/Table.csv")
table1<-read_xlsx("C:/Users/user/Desktop/ML/DA/R_data/test.xlsx")
table
table1
####�ɥR(�����bR�إߪ�)
table1 <- data.frame( location = c("�x�_", "����", "����", "�j��","�Q�J��","�X��"),  
                 DF = c("�ꤺ", "�ꤺ", "��~","��~","��~","��~"),  
                 price = c(30, 10, 23, 34,98,23)
                  ) 
####�ɥR(�����bR�إߪ�)
head(table, 5)
tail(table, 3)
t(table)
table[2,]
table[,2]
table[2,5]
subset(table1, table1$location=="����")
subset(table1, table1$DF=="��~" & table1$price>="80")
sample(table$�έp��,size=30,replace=T)
###test2
table[sample(1:nrow(table), size=6, replace=T),]
###test2
table[sample(2:3, size=2, replace=F),]
#Generate data
###test3
r1<-c(16,14,15,13,11,12)
r2<-c(1:6)
r3<-seq(from=2,length=6,by=2)
r4<-rep(r2,2)
r4m<-t(array(r4,dim=c(6,2)))
r4m<-t(matrix(r4,6,2))
M<-rbind(r1,r2,r3,r4m)
M
###test3
###test4
set.seed(3)
x=round(rnorm(100,20,5))
###test4

#Matrix operation
as.matrix(table)
t(as.matrix(table))
as.vector(as.matrix(table$�έp��))
as.vector(as.matrix(table[,2:3]))
###test5
Mw<-c(0.7,0.3)
Msm<-as.matrix(rbind(table$�έp��,table$��P��))
Mw%*%Msm
###test5
###test6
M3X3<-matrix(c(1:9),3,3)
solve(M3X3)
M3X3<-matrix(c(1,0,5,1,2,5,1,3,1),3,3)
invM3X3<-solve(M3X3)
M3X3%*%invM3X3
###test6
###test7
diag(1,3,3)
M3X3%*%diag(3,3,3)
###test7
dim(M3X3)
###test8
apply(M3X3,1,sum)
apply(M3X3,2,prod)
c4<-rowSums(M3X3)
c5<-rowMeans(M3X3)
Grade<-cbind(M3X3,c4,c5)
rownames(Grade)
colnames(Grade)
colnames(Grade)<-c("��1","��2","��3","sum","avg")
rownames(Grade)<-c("�ǥ�1","�ǥ�2","�ǥ�3")
Grade
###test8
det(M3X3)
max(M3X3, na.rm=TRUE)

#Statistical operation
x<-c(0:10,50)
c(mean(x), mean(x,trim = 0.1))
x1<-c(0:10,NA)
c(mean(x1), mean(x1,na.rm=TRUE))
median(x)
sd(x)
var(x)
cov(M3X3)
cor(M3X3)
eigen(M3X3)
eigen(M3X3)$value
eigen(M3X3)$vector

#chapter 1.4
rent<-read.csv("C:/Users/user/Desktop/ML/DA/R_data/rent.csv")
#�@���C�p�����R
table(rent$�Ϋ�)
prop.table(table(rent$�Ϋ�))
tapply(rent$����, list(rent$�Ϋ�), mean)
tapply(rent$����, list(rent$���B�u), sd)
#�G���C�p�����R
table(rent$�Ϋ�,rent$���B�u)
prop.table(table(rent$�Ϋ�,rent$���B�u),margin=1)
tapply(rent$����, list(rent$�Ϋ�,rent$���B�u), sum)

#chapter 1.5
house<-read.csv("C:/Users/user/Desktop/ML/DA/R_data/house.csv")
hist(house$price,col=2, breaks=5)
#BAR CHART
barplot(table(house$season))
barplot(table(house$season,house$district), beside=TRUE)
#BOX CHART
boxplot(house$price, col=2)
#PIE CHART
pie(table(house$district), init.angle = 90)
v<-table(house$district)
label<-paste(names(v),"\n", v, "\n", round(prop.table(v)*100,2)," %")
pie(v, labels=label, init.angle = 90)
#SCATTER PLOT
plot(house$price,house$area,type="p",pch=c(1,7),col=c(2,3),xlab="price",ylab="area",main="house_price vs area")
smoothScatter(house$price,house$area)
coplot(house$area~house$unitPrice|house$season,xlab="area",ylab="unit_price")
cloud(house$price~house$age+house$area|house$season)
