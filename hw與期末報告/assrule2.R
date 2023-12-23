install.packages("arules")
library(arules)

 # 每一個大括號內的資料，代表著消費者當次所購買的商品品項


# 直接使用 arules 套件的 read.transactions 函數讀取 CSV 檔案
transactions <- read.transactions("C:/Users/user/Desktop/MLhw1_A4.csv", format = "basket", sep = ",", header = FALSE)

# 移除 {V=,} 以及空白部分
transactions <- gsub("\\s*V=[^,]+,\\s*", "", transactions)

# 再次使用 inspect 函數檢查結果
inspect(transactions)



#由结果我们可以获得Groceries的基本信息，它包含9835条交易以及169个项，即商品；
#全脂牛奶(whole milk)是最受欢迎的商品，之后为蔬菜(other vegetables)、面包卷(rolls/buns)等

itemFrequency(transactions) #利用itemFrequency函式，去列出每一項品項佔的比例，藉由這個函數，也可以找出出現頻率比較高的商品。
itemFrequencyPlot(transactions,topN = 10) 
itemFrequencyPlot(transactions,topN = 10,type = "absolute")
itemFrequencyPlot(transactions,topN = 10,horiz = T,
                  main = "Item Frequency",xlab = "Relative Frequency")

itemFrequencyPlot(transactions,support = 0.1,
                  main = "Item Frequency with S = 0.1",ylab = "Relative Frequency")
#用itemFrequencyPlot繪出產品佔的比例圖，support參數是支持度的意思(通常會默認是0.1

#載入在關聯分析中最重要的演算法之一 —> Apriori
rules0 = apriori(transactions, parameter = list(support = 0.001, confidence = 0.5))
rules0
inspect(rules0[1:5])
#这里对第一条规则解读一下：如果一个顾客购买了蜂蜜(honey)，那么他还会购买全脂牛奶(whole milk)，支持度为0.0011，置信度为0.7333，
#这表示该规则涵盖了大约0.1%的交易，而且在购买了蜂蜜后，他购买全脂牛奶的概率为73.3%，提升度为2.87，
#表明（购买蜂蜜之后再购买全脂牛奶的可能性）是（没有购买蜂蜜但是购买全脂牛奶的可能性）的2.87倍，即相对于一般没有购买蜂蜜的顾客购买全脂牛奶的概率提升了2.87倍。
#该结果显示有5668条关联规则，如此大量的关联规则全部输出是没有意义的，而且信息的排序没有规则可言。因此，接下来要考虑选择其中关联性较强的若干条规则。

#对生成规则进行强度控制
rules1 = apriori(transactions, parameter = list(support = 0.005, confidence = 0.5))
rules1

rules2 = apriori(transactions, parameter = list(support = 0.005, confidence = 0.60))
rules2
 
rules3 = apriori(transactions, parameter = list(support = 0.005, confidence = 0.64))
rules3

#通过对“三度”进行单独控制
inspect(sort(rules2, by = 'support')[1:2])
#同理，对置信度和提升度的控制可以通过by参数设置

#改变结果的输出形式
#apriori()和eclat()函数都可以根据需要输出频繁项集(frequent itemsets)等其他形式结果。
#比如当我们想知道某超市这个月销售量最高的商品，可以将apriori()中目标参数设置为”frequent itemsets”：
itemsets_apr = apriori(transactions, parameter = list(supp = 0.001, target = 'frequent itemsets'), control = list(sort = -1))
itemsets_apr
inspect(itemsets_apr[1:5])
#如上结果显示将sort参数对项集频率进行降序排序后，销量前5的商品分别为全脂牛奶、蔬菜、面包卷、苏打以及酸奶。
#以下我们使用eclat()函数来取最适合进行捆绑销售，或者说相近摆放的5对商品:
itemsets_ecl = eclat(transactions, parameter = list(minlen = 1, maxlen = 3, supp = 0.001, target = 'frequent itemsets'), control = list(sort = -1))
itemsets_ecl

inspect(itemsets_ecl[1:5])
###########
# 設定要尋找的左手邊項目
lhs_item <- c('女', '快樂', '健康', '週工時高於中位數', '滿意工作')

# 使用 apriori 函數找出包含指定左手邊項目的所有規則
rules <- apriori(transactions, parameter = list(maxlen = 2, supp = 0.001, conf = 0.1), 
                 appearance = list(lhs = lhs_item, default = 'rhs'))

# 顯示找到的規則
lhs_items <- labels(lhs(rules))

# 篩選左手邊不為空的規則
rules <- rules[!lhs_items %in% list('{}')]
sort.rules <- sort(rules, by="lift")

sort.rules <- sort(rules, by="support")
inspect(sort.rules)


###########
lhs_item_combinations <- c('女', '快樂', '健康', '週工時高於中位數', '滿意工作', '女,快樂', '女,健康', '女,週工時高於中位數', '女,滿意工作', '快樂,健康', '快樂,週工時高於中位數', '快樂,滿意工作', '健康,週工時高於中位數', '健康,滿意工作', '週工時高於中位數,滿意工作')

rules_combinations <- apriori(transactions, parameter = list(maxlen = 2, supp = 0.001, conf = 0.1), 
                              appearance = list(lhs = lhs_item_combinations, default = 'rhs'))

# 顯示找到的規則
inspect(rules_combinations)
############
#输出结果中的全脂牛奶和蜂蜜，以及全脂牛奶与苏打作为共同出现最为频繁的两种商品，则可以考虑采取相邻摆放等营销策略

#如果我们想促销一种比较冷门的商品——芥末(mustard)，可以通过将函数apriori()中的关联结果(rhs) 参数设置为”mustard”，来搜索出rhs中仅包含mustard 的关联规则，
#从而有效地找到mustard的强关联商品，来作为捆绑商品。另外，还设置参数maxlen为2，控制lhs中仅包含一种食品，因为实际上我们仅将两种商品进行捆绑，而不是一堆商品：
rules4 = apriori(transactions, parameter = list(maxlen = 2, supp = 0.001, conf = 0.1), appearance = list(rhs = 'mustard', default = 'lhs'))
inspect(rules4)

#关联规则的可视化
install.packages("arulesViz")
library(arulesViz)
rules5 = apriori(transactions, parameter = list(support = 0.002, confidence = 0.5))
rules5

plot(rules5) 

#可以通过更改参数设置，来变换横纵轴及颜色条所对应的变量，图略
plot(rules5, measure = c('support', 'lift'), shading = 'confidence')

#将shading参数设置为"order"来绘制出一种特殊的散点图——Two-key散点图
plot(rules5, shading = 'order', control = list(main = 'Two-key plot'))

#将图形类型更改为"grouped"生成分组图
plot(rules5, method = 'grouped')

#图中每个点对应于相应的支持度和置信度值，且其中关联规则点的颜色深浅由lift值的高低决定。
#可以看出大量规则的参数取值分布情况，如提升度较高的关联规则的支持度往往较低，支持度与置信度具有相反相关性等。
#如果想具体的值这些规则对应的是哪些商品及关联强度如何等信息，可以通过设置互动参数(interactive)来进行交互显示

#图散点图的横纵轴依然为支持度和置信度，而关联规则点的颜色深洗则表示其所代表的关联规则中含有商品的多少， 
#商品种类越多，点的颜色越深。

#图中按照lift参数来看， 关联性最强(圆点颜色最深)的两种商品为{hard cheese, butter}与{whipped/sour cream}; 
#而以support参数来看则是{candy, semi-finished bread}与{whole milk}关联性最强(圆点尺寸最大〉。

#https://guomin-h-a.github.io/2020/03/14/R-association-analysis/






















