##################### Load Packages ########################
## libraries Path
.libPaths()
.libPaths("C:/Program Files/R/R-4.3.2/library")
.libPaths()

## Load Packages
install.packages("quantmod") #用來做股價相關分析的套件
library("quantmod")

## Time-Series Data (Stock Market)
# 鴻海股價
getSymbols("2317.TW", src="yahoo", from="2010-01-01", to="2020-12-31") #HonHa
HonHa.TW <- get("2317.TW")
HonHa.TW.na = na.omit(HonHa.TW) #剔除遺失值

#可以利用函數，將日資料轉換成各種時間間隔(週、月、季)資料再畫圖。
#weekly, weekly, quarterly,
HonHa.TW.week <- as.data.frame(to.weekly(HonHa.TW.na))
TS.Form <- ts(HonHa.TW.week[,4], frequency=52, start=c(2010,01))
TS.Decompose <- decompose(TS.Form)
plot(TS.Decompose)

#敘述統計分析
install.packages(c("forecast","tseries"))
library(forecast)#AUTO-ARIMA
library(tseries) #adf kpss test

HonHa.TS <- HonHa.TW.week$HonHa.TW.na.Close

adf.test(HonHa.TS) #單根檢定(檢定時間序列是否定態，H_0:non-stationary)

BoxCox.trans <- BoxCox(HonHa.TS,lambda = "auto") #BoxCox轉換過後的資料，其中-1 <= lambda <= 1。
BoxCox.lambda(HonHa.TS) #計算 lambda 的值，方便最後的資料還原。
adf.test(BoxCox.trans) #再一次單根檢定，結果還是非定態，只好選擇差分轉換。

Diff <- diff(BoxCox.trans, differences = 1) #一次差分
Diff.Trans <- diff(BoxCox(HonHa.TS,lambda = "auto"),differences = 1) #差分有轉換(少見)
adf.test(Diff)  #定態

#建模
auto.arima(Diff ,stepwise = F,trace = T,stationary = T,ic=c("aic"))
fit <- arima(HonHa.TS ,order=c(2,1,0),include.mean=F)
fit


