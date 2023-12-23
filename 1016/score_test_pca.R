library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ellipse)
library(mclust)
library(cluster)
library(ca)

data <- read.csv("C:/Users/user/Desktop/ML/1016/sp500_data.csv.gz",row.names = 1, header=T)
#data <- read.csv("C:/Users/user/Desktop/ML/1016/score_test.csv", header=T)
pca <- princomp(data)
pca$loadings                 
#PCA視覺化
screeplot(pca, main='')#陡坡圖

loadings <- pca$loadings
data.eigen <- eigen(cov(data))
data.eigen$values

#Kaiser’s rule (Kaiser-Guttman criterion)
data.eigen$values[data.eigen$values >=1] 

data.pca.ve <- data.eigen$values/sum(data.eigen$values) #主成分解釋百分比
data.pca.ve
plot(data.pca.ve,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), 
     type = 'b',
     main = 'Scree plot')
#The variance explained criteria
cumsum(data.eigen$values/sum(data.eigen$values))

pca.loading <- data.eigen$vectors[,1:10] # select the first two principal components
colnames(pca.loading) <- c(paste("PC",1:10,sep="")) # name columns
rownames(pca.loading) <- colnames(data) # name rows
pca.loading

