######R CODE (W1)######
'''
table<-read.csv("C:/Users/user/Desktop/MLhw1.csv")

library(dplyr)

#刪除指定變數空值存於table1
table1=subset(table, table$居住縣市<93 & table$最高教育程度<93)
# 定義字典（鍵是原始名稱，值是新名稱）
aname_dict <- c("1"="台北市", "2"="新北市", "3"="基隆市", "4"="桃園市", "5"="新竹縣", "6"="新竹市", "7"="苗栗縣", "8"="台中市", "9"="彰化縣", "10"="南投縣", "11"="雲林縣", "12"="嘉義縣", "13"="嘉義市", "14"="台南市", "15"="高雄市", "16"="屏東縣", "17"="宜蘭縣", "18"="花蓮縣", "19"="台東縣", "20"="澎湖縣", "21"="金門縣", "22"="連江縣", "23"="大陸地區（含港澳）", "24"="國外", "29"="新竹縣市"
)
bname_dict <- c("1" = "高中", "2" = "高職", "3" = "五專", "4" = "二專", "5" = "技術學院或科大", "6" = "大學", "7" = "碩士", "8" = "博士", "9" = "其他")

# 將最高教育程度欄位轉換為因子
table1 <- table1 %>%
  mutate(居住縣市 = factor(居住縣市, levels = names(aname_dict), labels = aname_dict))
table1 <- table1 %>%
  mutate(最高教育程度 = factor(最高教育程度, levels = names(bname_dict), labels = bname_dict))

#離散變數眾數
離散變數眾數=names(table(table1$最高教育程度))[which.max(table(table1$最高教育程度))]

library(knitr)
#a
d=as.data.frame(table(table1$居住縣市))
colnames(d)<-c("居住縣市","人數")
kable(format(d,justify='right'), border = "solid")
paste("眾數是：", 離散變數眾數)
#b
d=as.data.frame(table(table1$最高教育程度))
colnames(d)<-c("最高教育程度","人數")
kable(format(d,justify='right'), border = "solid")
paste("眾數是：", 離散變數眾數)
'''
table<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")
library(dplyr)

#統計運算
  a="週工時"
  var1=table[[a]]
  table1=subset(table, var1!="")
  var1=table1[[a]]
  k=c(mean(var1),median(var1),sd(var1),var(var1))
  k=round(k,digits = 2)
  paste("週工時：平均",k[1],"中位數",k[2],"標準差",k[3],"變異數",k[4],sep="")

  a="身高"
  b="體重"
  var1=table[[a]]
  var2=table[[b]]
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]
  cov(var1,var2)
  cor(var1,var2)
  #k=as.matrix(rbind(var1,var2))
  #eigen(k)
#眾數
  a="週工時"
  b="最高教育程度"
  var1=table[[a]]
  var2=table[[b]]
#刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]

#一維table-離散變數眾數
  離散變數眾數1=names(table(var1))[which.max(table(var1))]
  離散變數眾數2=names(table(var2))[which.max(table(var2))]
  library(knitr)
  #a
  d=as.data.frame(table(var1))
  colnames(d)<-c(a,"人數")
  kable(format(d,justify='right'), border = "solid")
  paste("眾數是：", 離散變數眾數1)
  #b
  d=as.data.frame(table(var2))
  colnames(d)<-c(b,"人數")
  kable(format(d,justify='right'), border = "solid")
  paste("眾數是：", 離散變數眾數2)
#一維table-離散變數頻率
  a="最高教育程度"
  var1=table[[a]]
  table1=subset(table, var1!="")
  var1=table1[[a]]
  d=as.data.frame(round(prop.table(table(var1)),digits = 3))
  colnames(d)<-c(a,"頻率")
  kable(format(d,justify='right'), border = "solid")
#一維tapply(x,list(factor),function),trim=0.1:
  a="週工時"
  b="小孩數"
  var1=table[[a]]
  var2=table[[b]]
 
  
  #刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]
  d <- as.data.frame(tapply(var1, list(var2),mean,trim=0.1))
  d <-round(d, 2)
  e=paste("平均",a, sep = "")
  colnames(d) <- e
  kable(format(d,justify='right'), digits = 2, border = "solid",caption=paste(b,"對照",e, sep = ""))

#二維table:
  a="大學科系"
  b="小孩數"
  var1=table[[a]]
  var2=table[[b]]
  #刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]
  e=table(var1,var2)
  kable(format(e,justify='right'), border = "solid")
#二維table頻率:
  '''a <- "大學科系"
  b <- "小孩數"
  var1 <- table[[a]]
  var2 <- table[[b]]
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- table1[[a]]
  var2 <- table1[[b]]
  e <- table(var1, var2)
  num_rows <- 9
  j <- data.frame(matrix(NA, nrow = num_rows, ncol = ncol(e)))
  for (i in 1:num_rows) {
    j[i, ] <- prop.table(e[i, ])
  }
  colnames(j) <- colnames(e)
  rownames(j) <- rownames(e)
  j=round(j,3)
  kable(format(j,justify='right'), border = "solid")
  '''
  
  a="大學科系"
  b="小孩數"
  var1=table[[a]]
  var2=table[[b]]
  #刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]
  e=prop.table(table(var1,var2),margin = 1)
  kable(format(e,justify='right'), border = "solid")
  
  
  
#二維tapply:
  a="性別"
  b="大學科系"
  x="時薪估計"
  var1=table[[a]]
  var2=table[[b]]
  var3=table[[x]]

  #刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!=""& var3!="")
  var1=table1[[a]]
  var2=table1[[b]]
  var3=table1[[x]]
  r <- as.data.frame(tapply(var3, list(var1,var2),mean,trim=0.1))
  r <-round(r, 1)
  #kable(format(d,justify='right'), digits = 2, border = "solid",caption=paste(a,"、",b,"對照平均",x, sep = ""))

'''圖例
  legend("topright",                                # 表示在右上角
         pch = 16,                                   # pch代表點的圖案
         col = category_colors,           # col代表顏色 
         legend = unique_categories, # 顏色所對應的名稱
         cex=0.7)
'''
  
#直方圖
  a="性別"
  b="時薪估計"
  var1=table[[a]]
  var2=table[[b]]
  #刪除指定變數空值存於table1
  table1=subset(table, var1!="" & var2!="")
  var1=table1[[a]]
  var2=table1[[b]]
  par(mfrow=c(1,2))
  hist(table(var1,var2)[1,],breaks = 50,main="女性時薪",xlab = "新台幣", ylab = "人數", xlim = c(0, 450), ylim = c(0,450))
  hist(table(var1,var2)[2,],breaks = 50,main="男性時薪",xlab = "新台幣",ylab = "人數", xlim = c(0, 450), ylim = c(0,450))

#長條圖 不同的顏色區別var2的類別
  library(ggplot2)
  library(dplyr)  
  
  a <- "薪水組中值"
  b <- "性別"
  var1 <- table[[a]]
  var2 <- table[[b]]
  
  # 刪除指定變數空值存於 table1
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- table1[[a]]
  var2 <- table1[[b]]
  var2 <- factor(var2)
  data <- data.frame(var1, var2)
  # 使用ggplot2繪製柱狀圖，以var1為x軸，fill為var2，geom為"bar"
  ggplot(data, aes(x = var1, fill = var2)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set1") + # 使用不同的顏色區別var2的類別
    labs(x = a,y = "性別/人數")
#長條圖 依類別分色(彩虹) 
  a <- "薪水組中值"
  b <- "小孩數"
  var1 <- table[[a]]
  var2 <- table[[b]]
  
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- table1[[a]]
  var2 <- table1[[b]]
  var2 <- factor(var2)
  data <- data.frame(var1, var2)
  # 創建一個包含不同顏色的類別-顏色映射
  color_mapping <- data.frame(var2 = unique(data$var2), color = rainbow(length(unique(data$var2))))
  
  # 使用ggplot2繪製柱狀圖，以var1為x軸，fill為var2，geom為"bar"
  ggplot(data, aes(x = var1, fill = var2)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = color_mapping$color)
  
#箱型圖 單色
  a <- "時薪估計"
  b <- "居住縣市"
  var1 <- table[[a]]
  var2 <- table[[b]]
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- table1[[a]]
  var2 <- table1[[b]]
  var2 <- factor(var2)
  data <- data.frame(var1, var2)
  
  # 繪製箱型圖，以var2為x軸，var1為y軸
  ggplot(data, aes(x = var2, y = var1)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    theme_minimal()+  # 使用自訂的顏色和風格
    ylim(0, 2000)+
    labs(x = b,y = a)
  
#箱型圖 多色
  a <- "時薪估計"
  b <- "性別"
  var1 <- table[[a]]
  var2 <- table[[b]]
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- table1[[a]]
  var2 <- table1[[b]]
  var2 <- factor(var2)
  data <- data.frame(var1, var2)
  # 使用ggplot2繪製箱型圖，以var2為x軸，y軸為var1，顏色區分var2的類別
  ggplot(data, aes(x = var2, y = var1, fill = var2)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Set1")+  # 使用不同的顏色區別var2的類別
    ylim(0, 2000)+
    labs(x = b,y = a)
#直方圖 
  a <- "薪水組中值"
  var1 <- table[[a]]
  table1 <- subset(table, var1 != "")
  var1 <- table1[[a]]
  hist(var1,col=2, breaks=17,xlab=a,main=paste(a,"分布",sep=""))
#長條圖 圓餅圖 依類別分色(彩虹)
  a <- "曾在幾家公司工作"
  var1 <- table[[a]]
  table1 <- subset(table, var1 != "")
  var1 <- factor(var1)
  f1 <- prop.table(table(table1[[a]]))
  category_colors <- rainbow(length(levels(var1)))
  unique_categories <- levels(var1)
  # 創建一個 1x2 的畫布，包含兩個子圖
  par(mfrow=c(1,2))
  # 第一個子圖：長條圖
  barplot(f1, main="長條圖", xlab=a, ylab=paste(a,"相對頻率", sep = ""), col = category_colors)
  # 第二個子圖：圓餅圖
  pie(f1, main="圓餅圖", labels=unique_categories, col = category_colors,cex=1)

#散佈圖
  par(mfrow=c(1,1))
  a <- "週工時"
  b <- "時薪估計"
  var1 <- table[[a]]
  var2 <- table[[b]]
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- as.numeric(table1[[a]])
  var2 <- as.numeric(table1[[b]])
  plot(var1, var2, xlab=a, ylab=b,pch = 16,cex=0.7,main=paste(a," vs ",b, sep = ""))
#平滑密度散佈圖 
  a <- "身高"
  b <- "體重"
  var1 <- table[[a]]
  var2 <- table[[b]]
  table1 <- subset(table, var1 != "" & var2 != "")
  var1 <- as.numeric(table1[[a]])
  var2 <- as.numeric(table1[[b]])
  plot(var1, var2, xlab=a, ylab=b,pch = 16,cex=0.7)
  smoothScatter(var1, var2, xlab=a, ylab=b,main=paste(a," vs ",b, sep = ""))
  
#coplot散佈圖
  a="小孩數"
  b="週工時"
  g="薪水組中值"
  var1=table[[a]]
  var2=table[[b]]
  var3=table[[g]]
  table1=subset(table, var1!="" & var2!=""& var3!="")
  var1=table1[[a]]
  var2=table1[[b]]
  var3=table1[[g]]
  var1=as.factor(var1)
  coplot(var3 ~ var2 | var1, xlab = g, ylab = b,cex=0.5)
  title(main = paste(a, "分類條件下，", g, "~", b, "的散佈圖", sep = ""))
  
#cloud散佈圖
  library(lattice)
  a="小孩數"
  b="週工時"
  g="薪水組中值"
  h="曾在幾家公司工作"
  var1=table[[a]]
  var2=table[[b]]
  var3=table[[g]]
  var4=table[[h]]
  table1=subset(table, var1!="" & var2!=""& var3!=""& var4!="")
  var1=table1[[a]]
  var2=table1[[b]]
  var3=table1[[g]]
  var4=table1[[h]]
  var1=as.factor(var1)
  # 自訂圖例
  key = list(
    text = list(
      labels = c(paste(a, "分類條件下", sep = ""), paste("var2 =", b, sep = ""), paste("var3 =", g, sep = ""), paste("var4 =", h, sep = "")),
      cex = 1
    ),
    space = "right"
  )
  print(cloud(var3 ~ var2 + var4 | var1, key = key))
  
