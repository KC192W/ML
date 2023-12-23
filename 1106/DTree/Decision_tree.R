
###############################
### 決策樹 (Decision Trees) ###
###############################
rm(list = ls())
#----------------------------------------------------------------------#
#此部分與GAMs的處理方式相同
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose",
                 "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits",
                 "Job", "NumLiable", "Phone", "Foreign", "Credit")
# 讀取資料並指定變數名稱(col.names)
credit <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"),
                     sep = "", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)
# 針對所關心的類別變數進行解碼（原本為非顯性編碼(non-obvious codes)）
# 信用紀錄
creditHistory <- c(A30 = "All Paid", A31 = "All Paid This Bank",
                   A32 = "Up To Date", A33 = "Late Payment",
                   A34 = "Critical Account")
# 用途
purpose <- c(A40 = "car (new)", A41 = "car (used)",
             A42 = "furniture/equipment", A43 = "radio/television",
             A44 = "domestic appliances", A45 = "repairs", A46 = "education",
             A47 = "(vacation - does not exist?)", A48 = "retraining",
             A49 = "business", A410 = "others")
# 目前這份工作的年資
employment <- c(A71 = "unemployed", A72 = "< 1 year", A73 = "1 - 4 years",
                A74 = "4 - 7 years", A75 = ">= 7 years")
# 轉換後取代原本的變數
credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]
# 將信用重新編成好(good)/壞(bad)
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
# 將信用儲存為 factor，並以好(Good)做為基準（預設會以字母順序最前面的類別作為基準）
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))
#----------------------------------------------------------------------#
# 建立決策樹
#install.packages("rpart")
library(rpart)
creditTree <- rpart(Credit ~ CreditAmount + Age + CreditHistory + Employment, data = credit) 
creditTree #以文字的形式呈現樹
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(creditTree, extra = 4) #以圖形的形式呈現樹
# extra: 在節點上顯示額外訊息
# extra = 4: 呈現節點中每類觀測值的機率值（總和為 1）。

##################################
### 提升樹模型 (Boosted Trees) ###
##################################
rm(list = ls())
#----------------------------------------------------------------------#
#此部分與GAMs、決策樹的處理方式相同
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose",
                 "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits",
                 "Job", "NumLiable", "Phone", "Foreign", "Credit")
# 讀取資料並指定變數名稱(col.names)
credit <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"),
                     sep = "", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)
# 針對所關心的類別變數進行解碼（原本為非顯性編碼(non-obvious codes)）
# 信用紀錄
creditHistory <- c(A30 = "All Paid", A31 = "All Paid This Bank",
                   A32 = "Up To Date", A33 = "Late Payment",
                   A34 = "Critical Account")
# 用途
purpose <- c(A40 = "car (new)", A41 = "car (used)",
             A42 = "furniture/equipment", A43 = "radio/television",
             A44 = "domestic appliances", A45 = "repairs", A46 = "education",
             A47 = "(vacation - does not exist?)", A48 = "retraining",
             A49 = "business", A410 = "others")
# 目前這份工作的年資
employment <- c(A71 = "unemployed", A72 = "< 1 year", A73 = "1 - 4 years",
                A74 = "4 - 7 years", A75 = ">= 7 years")
# 轉換後取代原本的變數
credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]
# 將信用重新編成好(good)/壞(bad)
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
# 將信用儲存為 factor，並以好(Good)做為基準（預設會以字母順序最前面的類別作為基準）
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))
#----------------------------------------------------------------------#
library(useful)
# 建立模型的 formula，因為是建立樹模型，所以不需要截距項(-1)
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount - 1
# 使用所有 level 的類別變數（類別變數以 one hot encoding 編碼）
# contrasts(as.factor(credit$Employment)) #dummy variable
# contrasts(as.factor(credit$Employment), FALSE) #one hot encoding
creditX <- build.x(creditFormula, data = credit, contrasts = FALSE)
# contrasts = FALSE: one hot encoding（為每個類別新增一個欄位，用 0/1 表示是否）
# contrasts = TRUE: dummy variable
creditY <- build.y(creditFormula, data = credit)
# 將邏輯向量 (logical vector) 轉換為 [0,1]
# creditY 轉為數值，以Bad當基準(1-1=0)、Good為(2-1=1)
creditY <- as.integer(relevel(creditY, ref = 'Bad')) - 1

#install.packages("xgboost")
library(xgboost)
creditBoost <- xgboost(data = creditX, label = creditY, max.depth = 3,
                       eta = .3, nthread = 4, nrounds = 3,
                       objective = "binary:logistic", eval_metric = "error")
# 建立的模型越多，評估結果越好
creditBoost20 <- xgboost(data = creditX, label = creditY, max.depth = 3,
                         eta = .3, nthread = 4, nrounds = 20,
                         objective = "binary:logistic", eval_metric = "error")
# data: 預測變數矩陣
# label: 反應變數向量
# max.depth: 樹的深度，預設為6。
# eta: 縮放每棵樹的貢獻， 0 < eta < 1，預設為 0.3。
# nthread: 控制平行運算的執行緒(thread)數量。
# nrounds: 決定對資料建模幾次，太高的次數可能會造成過度配適，所以挑選此數需要一些考量。
# objective : 指定訓練的模型類別（二元分類模型）。

# installed.packages("DiagrammeR")
library(DiagrammeR)
# 嘗試將幾顆樹混合在一起
xgb.plot.multi.trees(creditBoost, feature_names = colnames(creditX))
# 繪製變數重要性圖 (variable importance plot)
xgb.plot.importance(xgb.importance(creditBoost, feature_names = colnames(creditX)))
# feature_names: 提供節點名稱

#################################
### 隨機森林 (Random Forests) ###
#################################
rm(list = ls())
#----------------------------------------------------------------------#
#此部分與GAMs、決策樹、提升樹的處理方式相同
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose",
                 "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits",
                 "Job", "NumLiable", "Phone", "Foreign", "Credit")
# 讀取資料並指定變數名稱(col.names)
credit <- read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"),
                     sep = "", header = FALSE, col.names = creditNames, stringsAsFactors = FALSE)
# 針對所關心的類別變數進行解碼（原本為非顯性編碼(non-obvious codes)）
# 信用紀錄
creditHistory <- c(A30 = "All Paid", A31 = "All Paid This Bank",
                   A32 = "Up To Date", A33 = "Late Payment",
                   A34 = "Critical Account")
# 用途
purpose <- c(A40 = "car (new)", A41 = "car (used)",
             A42 = "furniture/equipment", A43 = "radio/television",
             A44 = "domestic appliances", A45 = "repairs", A46 = "education",
             A47 = "(vacation - does not exist?)", A48 = "retraining",
             A49 = "business", A410 = "others")
# 目前這份工作的年資
employment <- c(A71 = "unemployed", A72 = "< 1 year", A73 = "1 - 4 years",
                A74 = "4 - 7 years", A75 = ">= 7 years")
# 轉換後取代原本的變數
credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]
# 將信用重新編成好(good)/壞(bad)
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
# 將信用儲存為 factor，並以好(Good)做為基準（預設會以字母順序最前面的類別作為基準）
credit$Credit <- factor(credit$Credit, levels = c("Good", "Bad"))
#----------------------------------------------------------------------#

# install.packages("randomForest") 無法成功
# Warning in install.packages: package ‘randomForest’ is not available (for R version 4.0.2)
urlPackage <- "https://cran.rstudio.com/bin/windows/contrib/4.0/randomForest_4.6-14.zip"
install.packages(urlPackage, repos = NULL, type = "source")

library(randomForest)
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount - 1
library(useful)
# 使用所有 level 的類別變數（類別變數以 one hot encoding 編碼）
creditX <- build.x(creditFormula, data = credit, contrasts = FALSE)
creditY <- build.y(creditFormula, data = credit)
# 建立隨機森林樹
creditForest <- randomForest(x = creditX, y = creditY)
creditForest

# 建立反應變數矩陣
creditY2 <- as.integer(relevel(creditY, ref = 'Bad')) - 1
# 建立隨機森林
library(xgboost)
boostedForest <- xgboost(data = creditX, label = creditY2, max_depth = 4,
                         num_parallel_tree = 1000,
                         subsample = 0.5, colsample_bytree = 0.5,
                         nrounds = 3, objective = "binary:logistic", eval_metric = "error")
# num_parallel_tree: 平行建立樹的數量
# subsample: 隨機抽樣的橫列，預設為1（設定為 0.5 表示 xgboost 隨機收集一半的資料來種植樹，防止過度配適。）
# colsample_bytree: 隨機抽樣的直行，預設為1（每次建樹可以使用多少比例的features）
# eval_metric = "error": 二分類錯誤率

# installed.packages("DiagrammeR")
# library(DiagrammeR)
xgb.plot.multi.trees(boostedForest, feature_names = colnames(creditX))
# feature_names: 提供節點名稱

