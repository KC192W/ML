######R CODE (W2)######
#chapter 2.3
#Step 1: 線性程度檢視
table<-read.csv("C:/Users/user/Desktop/ML/迴歸分析/R_data/stock.csv")
y<-table$Stock.Index.Price
x1<-table$Interest.Rate
x2<-table$Unempolyment.Rate
plot(x=x1,y=y, col="red") #stock_interest 
plot(x=x2,y=y, col="blue") #stock_Unempolyment

#Step 2: 建立迴歸模型/多元判定係數/迴歸係數檢定
model<-lm(formula=y~x1+x2)
summary(model)

#Step 3:變異數分析線性驗證
summary(aov(y~x1+x2))
#Step 4:殘差的常態性檢定
library(nortest)
library(ggplot2)
model$residual
shapiro.test(model$residual)
pearson.test(model$residual) 
cvm.test(model$residual) 
ks.test(model$residual, "pnorm", mean=mean(model$residual), sd=sd(model$residual))
ad.test(model$residual)
lillie.test(model$residual)
sf.test(model$residual) 
ggplot(data.frame(x=model$residual), aes(sample=model$residual)) + stat_qq() + stat_qq_line()

#Step 5: 殘差的獨立性、同質性、共線性檢定
library(car)
durbinWatsonTest(model)
ncvTest(model)
vif(model)
plot(model) #簡易殘差分析

#-------------------------------------------

#chapter 2.6

#Step 1: 讀檔與類別自變數轉換
table1<-read.csv("C:/Users/user/Desktop/ML/迴歸分析/R_data/admit.csv")
admit<-table1$admit
gre<-table1$gre
gpa<-table1$gpa
rank<-as.factor(table1$rank)

#Step 2:建立羅吉斯迴歸模型與分析
model1<-glm(formula=admit~gre+gpa+rank, family="binomial")
summary(model1)
Odds<-exp(model1$coefficients)

#Step 3:計算預測值
admit.predict<-predict(model1,type="response")
admit.predict
new1<-data.frame(admit="0",gre=380,gpa=3.61,rank="3")
predict(model1, newdata=new1,type="response")
new2<-data.frame(admit="1",gre=800,gpa=4.00,rank="1")
predict(model1, newdata=new2,type="response")

#Step 4:模型準確度驗證
admit.approved<-ifelse(admit.predict>0.5,1,0)
admit.com<-table(table1$admit,admit.approved,dnn=c("REAL","PRED"))
admit.com
accury<-sum(diag(admit.com))/sum(admit.com)
accury







