
# exercise 4
library(arules)
library(arulesViz)
#(1)
market<-read.transactions("H:/學校的事/輔仁大學管理學院/1121/1121 跨領域課程三單元/MLcode/exercise 7_4.csv",sep=",")
summary(market.rule)
#(2)
inspect(market[1:5])
#(3)
itemFrequencyPlot(market,topN=10)
itemFrequencyPlot(market,topN=10,type="absolute")
#(4)
itemFrequencyPlot(market,topN = 10,horiz = T,
                  main="Item Frequency",xlab="Relative Frequency")

# exercise 5
#(1)
market.rule<-apriori(market,parameter=list(support=0.01,confidence=0.3))
summary(market.rule)
#(2)
inspect(market.rule[1:5])

# exercise 6
require(arules)
#(1)
plot(market.rule)
#(2)
plot(market.rule,method="graph",control=
       list(nodeCol="red",edgeCol="blue",type="items"))
#(3)
plot(market.rule, method="grouped", control=list(col=4))
#(4)
submarket.rule<-head(sort(market.rule,by="lift"),5)
plot(submarket.rule,method="paracoord",
     control=list(col=4,main="Cosmetic Trade"))

# exercise 7
plot(submarket.rule,method="graph",
     control=list(alpha=1,main="Under 5 Rules"))











