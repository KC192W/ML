# CH5 
# exercise 1
babies<-read.csv("TreeEx5_1.csv")
newbabies<-babies[complete.cases(babies),]
#(1)
library(rpart)
babies.id3<-rpart(smoke~.,data=newbabies,method="class",
                  parms=list(split="information"))
babies.id3
printcp(babies.id3)
#(2)
plot(babies.id3,uniform=T,branch=0,margin=0.1,main="TREE ID3")
text(babies.id3,use.n=T,col="blue",cex=1.0)
#(3)
require(rpart.plot)
prp(babies.id3,faclen=0,fallen.leaves=TRUE,shadow.col="gray",extra=2)
#(4)
babies.pred<-predict(babies.id3,newdata=newbabies,type="class")
babies.predtable<-table(real=newbabies$smoke,predict=babies.pred)
babies.predtable
prop.table(babies.predtable)

# exercise 2
library(rpart)
#(1)
babies.cart<-rpart(smoke~.,data=newbabies,method="class",
                   parms=list(split="gini"))
babies.cart
printcp(babies.cart)
#(2)
plot(babies.cart,uniform=T,branch=0,margin=0.1,main="TREE CART")
text(babies.cart,use.n=T,col="blue",cex=1.0)
#(3)
require(rpart.plot)
prp(babies.cart,faclen=0,fallen.leaves=TRUE,shadow.col="gray",extra=2)
#(4)
babiescart.pred<-predict(babies.cart,newdata=newbabies,type="class")
babiescart.predtable<-table(real=newbabies$smoke,predict=babiescart.pred)
babiescart.predtable
prop.table(babiescart.predtable)

