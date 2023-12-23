'''data0<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")
x1="曾在幾家公司工作"
x2="週工時"

data0=subset(data0, data0[[x1]]!="" & data0[[x2]]!="")

library(caTools)
set.seed(123)
split = sample.split (data0,SplitRatio =0.1)
train = subset (data0, split == TRUE)
test = subset (data0, split == FALSE)
data=data.frame()
data = data.frame(matrix(NA,nrow = nrow(train), ncol = 2))
colnames(data) <- c(x1, x2)
data[[x1]]<-train[[x1]]
data[[x2]]<-train[[x2]]
x1<-data[[x1]]
x2<-data[[x2]]


dist<-dist(data, method="euclidean") #other method= manhattan 

'''

######R CODE (W3)######

#Section 3.4
data0<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")
x1="週工時"
x2="曾在幾家公司工作"
x3="薪水組中值"

data0=subset(data0, data0[[x1]]!="" & data0[[x2]]!=""& data0[[x3]]!="")

z_scores <- scale(data0[[x3]])
threshold <- 1
# 去除 Z 大於閾值的觀測值
data <- data0[abs(z_scores) <= threshold, ]
data=data[, c(x1,x2,x3)]

library(caTools)
set.seed(123)
split = sample.split (data,SplitRatio =0.01)
train = subset (data, split == TRUE)
test = subset (data, split == FALSE)
data=data.frame()
data = data.frame(matrix(NA,nrow = nrow(train), ncol = 3))
colnames(data) <- c(x1, x2, x3)
data[[x1]]<-train[[x1]]
data[[x2]]<-train[[x2]]
data[[x3]]<-train[[x3]]
x1<-data[[x1]]
x2<-data[[x2]]
x3<-data[[x3]]

dist<-dist(data, method="euclidean") #other method= manhattan 




#AGNES 演算法
wage.hcluster<-hclust(dist, method="centroid") #other method= single, complete, average, ward.D2
plot(wage.hcluster) #繪製樹狀圖
rect.hclust(wage.hcluster, k=4, border="red")    #分3群標示
cut.h.cluster<-cutree(wage.hcluster, k=4)
cbind(data,cut.h.cluster) #分群總表
count.cluster<-table(cut.h.cluster) #每群個數表
plot(count.cluster) # 每群個數長條圖

#DIANA 演算法
#install.packages("cluster")
library(cluster)
wage.diana<-diana(data, metric="euclidean",stand=TRUE)
wage.diana
plot(wage.diana) #繪製樹狀
rect.hclust(wage.diana, k=4, border="red")    #分3群標示

pdf("diana_plot2.pdf")
plot(wage.diana)
dev.off()


cut.wage.diana<-cutree(wage.diana, k=4)
cbind(data,cut.wage.diana) #分群總表
count.diana<-table(cut.wage.diana) #每群個數表
plot(count.diana) # 每群個數長條圖




cut.wage.diana<-cutree(wage.diana, k=4)
cbind(data,cut.wage.diana) #分群總表
count.diana<-table(cut.wage.diana) #每群個數表
plot(count.diana) # 每群個數長條圖


#Section 3.5
'''df0<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")
x1="週工時"
x2="曾在幾家公司工作"
x3="薪水組中值"
df=subset(df0,data0[[x1]]!="" & data0[[x2]]!=""& data0[[x3]]!="")'''

explain<-data[,-3]  #資料不含
x1="週工時"
x2="曾在幾家公司工作"
x3="薪水組中值"
#K-Means 演算法
#install.packages("factoextra")
library(factoextra)
wage<-kmeans(explain,4,nstart=10)
wage
cbind(data[[x3]],wage$cluster) #分群總表
count.wage<-table(wage$cluster)#每群個數表
plot(count.wage) # 每群個數長條圖
fviz_cluster(wage, data=explain, geom=c("point","test"), ellipse.type="norm")

#K-Medoid 演算法
library(cluster)
library(factoextra)
kmedoid.wage<-pam(explain,k=4)
kmedoid.wage
cbind(data[[x3]],kmedoid.wage$clustering) #分群總表
count.kmedoid.wage<-table(kmedoid.wage$clustering)#每群個數表
plot(count.kmedoid.wage) # 每群個數長條圖
fviz_cluster(kmedoid.wage, data=explain, geom=c("point","test"), ellipse.type="norm")

k=cbind(data[[x3]],wage$cluster)
write.csv(k, file = "data.csv", row.names = FALSE)