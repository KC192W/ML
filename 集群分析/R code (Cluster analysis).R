######R CODE (W3)######

#Section 3.4
data<-read.csv("C:/Users/user/Desktop/ML/集群分析/R_data/dog.csv")
dist<-dist(data, method="euclidean") #other method= manhattan 

#AGNES 演算法
dog.hcluster<-hclust(dist, method="centroid") #other method= single, complete, average, ward.D2
plot(dog.hcluster) #繪製樹狀圖
rect.hclust(dog.hcluster, k=3, border="red")    #分3群標示
cut.h.cluster<-cutree(dog.hcluster, k=3)
cbind(data,cut.h.cluster) #分群總表
count.cluster<-table(cut.h.cluster) #每群個數表
plot(count.cluster) # 每群個數長條圖

#DIANA 演算法
#install.packages("cluster")
library(cluster)
dog.diana<-diana(data, metric="euclidean",stand=TRUE)
dog.diana
plot(dog.diana) #繪製樹狀
rect.hclust(dog.diana, k=3, border="red")    #分3群標示
cut.dog.diana<-cutree(dog.diana, k=3)
cbind(data,cut.dog.diana) #分群總表
count.diana<-table(cut.dog.diana) #每群個數表
plot(count.diana) # 每群個數長條圖


#Section 3.5
houseC<-read.csv("C:/Users/user/Desktop/ML/集群分析/R_data/houseC.csv")
explain<-houseC[,2:4]  #資料不含區域

#K-Means 演算法
#install.packages("factoextra")
library(factoextra)
km.house<-kmeans(explain,3,nstart=10)
km.house
cbind(houseC$district,km.house$cluster) #分群總表
count.km.house<-table(km.house$cluster)#每群個數表
plot(count.km.house) # 每群個數長條圖
fviz_cluster(km.house, data=explain, geom=c("point","test"), ellipse.type="norm")

#K-Medoid 演算法
library(cluster)
library(factoextra)
kmedoid.house<-pam(explain,k=3)
kmedoid.house
cbind(houseC$district,kmedoid.house$clustering) #分群總表
count.kmedoid.house<-table(kmedoid.house$clustering)#每群個數表
plot(count.kmedoid.house) # 每群個數長條圖
fviz_cluster(kmedoid.house, data=explain, geom=c("point","test"), ellipse.type="norm")

