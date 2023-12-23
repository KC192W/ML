
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ellipse)
library(mclust)
library(cluster)
library(ca)
getwd()
data0 <- read.csv("C:/Users/user/Desktop/MLhw1_c.csv", stringsAsFactors = FALSE)
x1 <- "薪水組中值"
x2 <- "週工時"

data0 <- data0[, c(x1, x2)]
data_c <- subset(data0, data0[[x1]] != "" & data0[[x2]] != "")
data_c[[x1]] <- as.numeric(data_c[[x1]])
data_c[[x2]] <- as.numeric(data_c[[x2]])

pca <- princomp(data_c)
loadings <- pca$loadings

# 繪製散點圖和主成分分析結果
library(ggplot2)
graph <- ggplot(data = data_c, aes(x = 薪水組中值, y = 週工時)) +
  geom_point(alpha = 0.3) +
  scale_shape_manual(values = c(46)) +
  stat_ellipse(type = 'norm', level = 0.99, color = 'grey25') +
  geom_abline(intercept = 0, slope = loadings[2, 1] / loadings[1, 1], color = 'grey25', linetype = 2) +
  geom_abline(intercept = 0, slope = loadings[2, 2] / loadings[1, 2], color = 'grey25', linetype = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 90000), ylim = c(0, 200)) +
  theme_bw()

print(graph)
screeplot(pca, main='')

data0 <- read.csv("C:/Users/user/Desktop/MLhw1_c.csv", stringsAsFactors = FALSE)
syms <- c( '時薪估計', '週工時', '身高', '體重', '曾在幾家公司工作')
data0 <- data0[,syms]
data_c <- na.omit(data0)
sp_pca <- princomp(data_c)
par(mar=c(6,3,0,0)+.1, las=2)
screeplot(sp_pca, main='')#陡坡圖

loadings <- sp_pca$loadings[,1:5]
loadings <- as.data.frame(loadings)
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, 'Component', 'Weight', -Symbol)

loadings$Color=loadings$Weight > 0
graph <- ggplot(loadings, aes(x=Symbol, y=Weight, fill=Color)) +
  geom_bar(stat='identity', position='identity', width=.75) + 
  facet_grid(Component ~ ., scales='free_y') +
  guides(fill='none') +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=90, vjust=0.5))
graph


par(mar=c(6,3,0,0)+.1, las=2)
screeplot(sp_pca, main='')
######################################################

data_c <- na.omit(data0)
# 再次確認資料結構
str(data_c)

# 計算相關矩陣
cor_matrix <- cor(data_c, use = "complete.obs")

# 使用 eigen 函數計算特徵值和特徵向量
eigen_result <- eigen(cor_matrix)

# 找出特徵值大於1的位置
selected_components <- eigen_result$values > 1

# 根據所選的主成分數目提取特徵向量
selected_eigenvectors <- eigen_result$vectors[, selected_components]

# 進行主成分分析
pca_result <- as.matrix(data_c) %*% selected_eigenvectors

num_selected_components <- sum(selected_components)
cat("保留的主成分數目:", num_selected_components, "\n")
