########羅吉斯
  library(ggplot2)
  library(caTools)
  library(dplyr)
  #Step 1: 讀檔與類別自變數轉換
  table1<- read.csv("C:/Users/user/Desktop/Lcl.csv", header = TRUE)
  table1 <- na.omit(table1)
  split <- sample.split(table1, SplitRatio = 0.8)
  train_set <- subset(table1, split == "TRUE")
  test_set <- subset(table1, split == "FALSE")
  y<-train_set$是否快樂
  x1<-train_set$時薪是否高於中位數
  x2<-train_set$是否有小孩
  x3<-train_set$是否滿意工作
  x4<-train_set$是否健康
  x5<-train_set$BMI是否高於中位數
  x6<-train_set$週工時是否高於中位數
  x7<-train_set$是否結過婚
  x8<-train_set$工作是否符合所學
  #Step 2:建立羅吉斯迴歸模型與分析
  model1<-glm(formula=y~x1+x2+x3+x4+x5+x6+x7+x8, family="binomial")
  summary(model1)
  Estimate<-model1$coefficients
  Estimate
  Odds<-exp(model1$coefficients)
  Odds
    #繪製Estimate和Odds長條圖
    variables <- c("(Intercept)", "時薪是否高於中位數:是", "是否有小孩:是", "是否滿意工作:是", "是否健康:是", 
                   "BMI是否高於中位數:是", "週工時是否高於中位數:是", "是否結過婚:是", "工作是否符合所學:是")
    Estimate_data <- data.frame(variables, Estimate)
    par(mar = c(8, 4, 4, 2) + 0.1)
    ggplot(Estimate_data, aes(x = variables, y = Estimate)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Estimate of Variables", y = "Estimate") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    odds_data <- data.frame(variables, Odds)
    par(mar = c(8, 4, 4, 2) + 0.1)
    ggplot(odds_data, aes(x = variables, y = Odds)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Odds of Variables", y = "Odds") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #繪製ROC
    library("pROC")
    train_prob = predict(model1, type = "response")
    par(pty = "s") #指定畫布繪製成正方形的大小
    test_roc = roc(train_set$是否快樂 ~ train_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE) #legacy.axes=TRUE 將 x軸改成 1 - Specificity
  #Step 3:計算預測值
  admit.predict<-predict(model1,type="response")
  #Step 4:模型準確度驗證
    #繪製預測值分布直方圖
      hist(admit.predict, main = "Distribution of admit.predict",
           xlab = "admit.predict", ylab = "Frequency", col = "skyblue", border = "black")
    #繪製閾值vs準確性折線圖
      # 創建一系列的閾值（假設你已經有一些閾值的值）
      thresholds <- seq(0, 1, by = 0.01)
      # 創建一個空的向量來存儲 accuracy 的值
      accuracy_values <- numeric(length(thresholds))
      # 計算每個閾值下的 accuracy
      for (i in seq_along(thresholds)) {
        admit.approved <- ifelse(admit.predict > thresholds[i], 1, 0)
        admit.com <- table(train_set$是否快樂, admit.approved, dnn = c("REAL", "PRED"))
        accuracy_values[i] <- sum(diag(admit.com)) / sum(admit.com)
      }
      # 創建一個資料框
      accuracy_data <- data.frame(thresholds, accuracy_values)
      # 使用 ggplot2 繪製折線圖
      library(ggplot2)
      ggplot(accuracy_data, aes(x = thresholds, y = accuracy_values)) +
        geom_line() +
        labs(title = "Threshold vs Accuracy", x = "Threshold", y = "Accuracy") +
        theme_minimal()

  admit.approved<-ifelse(admit.predict>0.5,1,0)#選取閾值0.5
  admit.com<-table(train_set$是否快樂,admit.approved,dnn=c("REAL","PRED"))
  admit.com
  accury<-sum(diag(admit.com))/sum(admit.com)
  paste("accuracy：",accury)

  #Step 5:計算測試集的準確度
  y_test <- test_set$是否快樂
  x1_test <- test_set$時薪是否高於中位數
  x2_test <- test_set$是否有小孩
  x3_test <- test_set$是否滿意工作
  x4_test <- test_set$是否健康
  x5_test <- test_set$BMI是否高於中位數
  x6_test <- test_set$週工時是否高於中位數
  x7_test <- test_set$是否結過婚
  x8_test <- test_set$工作是否符合所學
  admit.predict_test <- predict(model1, newdata = data.frame(x1=x1_test, x2=x2_test, x3=x3_test, x4=x4_test, x5=x5_test, x6=x6_test, x7=x7_test, x8=x8_test), type = "response")
  admit.approved_test <- ifelse(admit.predict_test > 0.5, 1, 0)
  admit.com_test <- table(test_set$是否快樂, admit.approved_test, dnn = c("REAL", "PRED"))
  admit.com_test
  accuracy_test <- sum(diag(admit.com_test)) / sum(admit.com_test)
  paste("accuracy：",accuracy_test)
    #繪製ROC
    library("pROC")
    test_prob = predict(model1, newdata = data.frame(x1=x1_test, x2=x2_test, x3=x3_test, x4=x4_test, x5=x5_test, x6=x6_test, x7=x7_test, x8=x8_test), type = "response")
    par(pty = "s") #指定畫布繪製成正方形的大小
    test_roc = roc(test_set$是否快樂 ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE) #legacy.axes=TRUE 將 x軸改成 1 - Specificity
  
  

########決策樹
########關聯規則
  library(arules)
  data <- read.csv("C:/Users/user/Desktop/Acl.csv", header = TRUE)
  require(arules)
  rule <- apriori(data, 
                  # min support & confidence, 最小規則長度(lhs+rhs)
                  parameter=list(minlen=3, supp=0.4, conf=0.7),  
                  appearance = list(default="lhs",
                                    rhs=c("是否快樂=是","是否快樂=否") 
                                    # 右手邊顯示的特徵
                  )
  ) 
  inspect(rule)
  
  # 以下舉 lift 為例
  sort.rule <- sort(rule, by="lift")
  inspect(sort.rule)
  #第六個關聯規則(#編號8)「若身分是男性成人船員 => 不會存活」，對比於第五個關聯規則(#編號4)：「若身分是男性船員 => 不會存活」，其實看不到任何有用的資訊！
  #而且，第六個規則的lift <= 第五個規則的lift， 當發生這樣的情況時，我們就可以說：第六個關聯規則是多餘的(redundant)。
  #多餘的關聯法則，會造成分析上的雜訊，因此需要刪除它們
  # 先根據 support 大小排序 rules
  sort.rule <- sort(rule, by="support")
  subset.matrix <- is.subset(x=sort.rule, y=sort.rule)
  #上面的結果要解釋：在X的項目，如果是Y項目的子集(subset)，就會回傳TRUE。
  subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
  # 把這個矩陣的下三角去除，只留上三角的資訊
  subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  # 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
  redundant <- colSums(subset.matrix, na.rm=T) >= 1
  # 移除多餘的規則
  sort.rule <- sort.rule[!redundant]
  sort.rule <- sort(sort.rule, by="confidence")
  sort.rule <- sort(sort.rule, by="support")
  sort.rule <- sort(sort.rule, by="lift")
  inspect(sort.rule)
  
  #視覺化：在R裡面，關聯式規則還提供一個視覺化的套件arulesViz，可以觀察每條規則的三項指標分布情況：
  require(arulesViz)
  plot(sort.rule)
  plot(sort.rule, method="graph")
  plot(sort.rule, method="grouped")
  
  
  plot(sort.rule)
  