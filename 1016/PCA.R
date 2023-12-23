
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ellipse)
library(mclust)
library(cluster)
library(ca)
getwd()
sp500_px <- read.csv("C:/Users/user/Desktop/ML/1016/sp500_data.csv",row.names=1)#year變第一列
sp500_sym <- read.csv("C:/Users/user/Desktop/ML/1016/sp500_sectors.csv", stringsAsFactors = FALSE)
#利用princomp來計算主成分分析
oil_px <- sp500_px[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
pca$loadings



#PCA視覺化
loadings <- pca$loadings
#aes=axe;
graph <- ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) +#點狀圖
  scale_shape_manual(values=c(46)) +#變量類型多於6種（小於用linetype）
  #type:椭圆的類型，默認為“t"-多元t分布；"norm"為多元常態分布；"euclid"繪製一個半徑等於level的圆
  #level:	椭圆包含多少數據，默認level=0.95
  stat_ellipse(type='norm', level=.99, color='grey25') +
  #斜線intercept=截距
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1], color='grey25', linetype=2) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2],  color='grey25', linetype=2) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(-3, 3), ylim=c(-3, 3)) +
  theme_bw()
graph

####解釋主成分分析

syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
           'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca <- princomp(top_sp)
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

##########################K-means#######################################
'''oil_px <- pca.loading
pca <- princomp(oil_px)
pca$loadings
loadings <- pca$loadings
#aes=axe;
graph <- ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) +#點狀圖
  scale_shape_manual(values=c(46)) +#變量類型多於6種（小於用linetype）
  #type:椭圆的類型，默認為“t"-多元t分布；"norm"為多元常態分布；"euclid"繪製一個半徑等於level的圆
  #level:	椭圆包含多少數據，默認level=0.95
  stat_ellipse(type='norm', level=.99, color='grey25') +
  #斜線intercept=截距
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1], color='grey25', linetype=2) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2],  color='grey25', linetype=2) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(-3, 3), ylim=c(-3, 3)) +
  theme_bw()
graph
'''


