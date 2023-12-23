install.packages("arules")
library(arules)
data <- read.csv("C:/Users/user/Desktop/Acl.csv", header = TRUE)
#data <- read.csv("C:/Users/user/Desktop/MLhw1_A4.csv")
#data0 <- na.omit(data)
require(arules) # apriori關聯式法則的套件
##apriori演算法需要設定：
##支持度(min support)：「規則」在資料內具有普遍性，也就是這些 A 跟 B 同時出現的機率多少。
##信賴度(min confidence)：「規則」要有一定的信心水準，也就是當購買 A 狀態下，也會購買 B 的條件機率。
# apriori rules with rhs containing "Survived" only
rule <- apriori(data, 
                # min support & confidence, 最小規則長度(lhs+rhs)
                parameter=list(minlen=3, supp=0.4, conf=0.8),  
                appearance = list(default="lhs",
                                  rhs=c("是否快樂=是","是否快樂=否") 
                                  # 右手邊顯示的特徵
                )
) 


inspect(rule)

# 以下舉 lift 為例
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

#看第一個關聯規則：「若身分是成人女性 => 則會存活」，lift=2.3 > 1，表示這個規則相當具有正相關；
#但其 support 卻沒有想像中的高，這也可以理解，因為當時那個年代，船上的女性人數應該遠低於男性人數，因此女性的樣本數較少，造成 support 較低也是合理的。

#第六個關聯規則(#編號8)「若身分是男性成人船員 => 不會存活」，對比於第五個關聯規則(#編號4)：「若身分是男性船員 => 不會存活」，其實看不到任何有用的資訊！
#而且，第六個規則的lift <= 第五個規則的lift， 當發生這樣的情況時，我們就可以說：第六個關聯規則是多餘的(redundant)。
#多餘的關聯法則，會造成分析上的雜訊，因此需要刪除它們

# 先根據 support 大小排序 rules
sort.rule <- sort(rule, by="support")

# 'arules' version = 1.4-2 , under R-3.2.5
subset.matrix <- is.subset(x=sort.rule, y=sort.rule)
#上面的結果要解釋：在X的項目，如果是Y項目的子集(subset)，就會回傳TRUE。


# 'arules' version = 1.5-2 , under R-3.4.0
subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))

# 把這個矩陣的下三角去除，只留上三角的資訊
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

# 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# 移除多餘的規則
sort.rule <- sort.rule[!redundant]
#sort.rule <- sort(sort.rule, by="confidence")
inspect(sort.rule)

#視覺化：在R裡面，關聯式規則還提供一個視覺化的套件arulesViz，可以觀察每條規則的三項指標分布情況：
require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph")
plot(sort.rule, method="grouped")


plot(sort.rule)




