######R CODE (W2)######

'''
table0<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")
y0="?魫"
x10="?̰??Ш|?{??"
x20="????"
x30="?j?Ǭ??t"
x40="?ʧO"
x50="?p?ļ?"
table=subset(table0, table0[[y0]]!="" & table0[[x10]]!=""& table0[[x20]]!=""& table0[[x30]]!=""& table0[[x40]]!=""& table0[[x50]]!="")
y<-table[[y0]]
x1<-table[[x10]]
x2<-table[[x20]]
x3<-table[[x30]]
x4<-table[[x40]]
x5<-table[[x50]]
#plot(x=x1,y=y, col="red") 
#plot(x=x2,y=y, col="blue")
#plot(x=x5,y=y, col="green")

#Step 2: ?إ߰j?k?ҫ?/?h???P?w?Y??/?j?k?Y???˩w
model<-lm(formula=y~x1+x2+x3+x4+x5)
summary(model)

#Step 3:?ܲ??Ƥ��R?u??????
summary(aov(y~x1+x2+x3+x4+x5))

#Step 4:?ݮt???`?A???˩w
library(nortest)
library(ggplot2)
model$residual
shapiro.test(model$residual)
pearson.test(model$residual) 
cvm.test(model$residual) 
ks.test(model$residual, "pnorm", mean=mean(model$residual), sd=sd(model$residual))
ad.test(model$residual)
lillie.test(model$residual)
sf.test(model$residual) 
ggplot(data.frame(x=model$residual), aes(sample=model$residual)) + stat_qq() + stat_qq_line()

#Step 5: ?ݮt???W?ߩʡB?P???ʡB?@?u???˩w
library(car)
durbinWatsonTest(model)
ncvTest(model)
vif(model)
plot(model) #²???ݮt?��R
'''
#-------------------------------------------
        '''?p???魫?? Z
        z_scores <- scale(y)
        threshold <- 1
        # ?h?? Z ?j???H?Ȫ??[????
        filtered_data <- table[abs(z_scores) <= threshold, ]
        y<-filtered_data[[y0]]
        x1<-filtered_data[[x10]]
        x2<-filtered_data[[x20]]
        ?{?b filtered_data ???]?t?F?h?????ȫ᪺????'''
#chapter 2.3
#Step 1: ?u?ʵ{???˵?
table0<-read.csv("C:/Users/user/Desktop/MLhw1_c.csv")

y0="?魫"
x10="????"
x20="?ʧO"

table=subset(table0, table0[[y0]]!="" & table0[[x10]]!=""& table0[[x20]]!="")
#?����??ն??P?V?m??
library(caTools)
set.seed(123)
split = sample.split (table,SplitRatio =0.66)
train = subset (table, split == TRUE)
test = subset (table, split == FALSE)
y<-train[[y0]]
x1<-train[[x10]]
x2<-train[[x20]]
plot(x=x1,y=y, col="blue",xlab = x10,ylab=y0,main = paste(x10,"vs",y0,"?u?ʵ{???˵?",sep=""),pch=16,cex=0.5) 

#Step 2: ?إ߰j?k?ҫ?/?h???P?w?Y??/?j?k?Y???˩w
model<-lm(formula=y~x1+x2)
summary(model)

#Step 3:?ܲ??Ƥ��R?u??????
summary(aov(y~x1+x2))

#Step 4:?ݮt???`?A???˩w
library(nortest)
library(ggplot2)
#model$residual
shapiro.test(model$residual)
pearson.test(model$residual) 
cvm.test(model$residual) 
ks.test(model$residual, "pnorm", mean=mean(model$residual), sd=sd(model$residual))
ad.test(model$residual)
lillie.test(model$residual)
sf.test(model$residual) 
ggplot(data.frame(x=model$residual), aes(sample=model$residual)) + stat_qq() + stat_qq_line()

#Step 5: ?ݮt???W?ߩʡB?P???ʡB?@?u???˩w
library(car)
durbinWatsonTest(model)
ncvTest(model)
vif(model)
plot(model,pch = 16,cex=0.5) #²???ݮt?��R

#-------------------------------------------
#chapter 2.6

#Step 1: Ū?ɻP???O???ܼ??ഫ
table0<-read.csv("C:/Users/user/Downloads/subset(table0, table0$?ʧO!="")
aname_dict <- c("?k"="0", "?k"="1")

table <- table %>%
  mutate(?ʧO = factor(?ʧO, levels = names(aname_dict), labels = aname_dict))
table=subset(table, table$?ʧO!=""&table$???~???p!=""&table$?j?Ǭ??t!="")
gender<-table$?ʧO
Hourlyrate<-table$???~???p
department<-as.factor(table$?j?Ǭ??t)

#Step 2:?إ?ù?N???j?k?ҫ??P?��R
model1<-glm(formula=gender~Hourlyrate+department, family="binomial")
summary(model1)
Odds<-exp(model1$coefficients)
Odds
#Step 3:?p???w????
gender.predict<-predict(model1,type="response")
gender.predict
new1<-data.frame(gender="0",Hourlyrate=300,department="?u?{?B?s?y?????y????")
predict(model1, newdata=new1,type="response")
new2<-data.frame(gender="1",Hourlyrate=172,department="?H?????��N????")
predict(model1, newdata=new2,type="response")

#Step 4:?ҫ??ǽT??????
gender.approved<-ifelse(gender.predict>0.5,1,0)
gender.com<-table(table$?ʧO,gender.approved,dnn=c("REAL","PRED"))
gender.com
accury<-sum(diag(gender.com))/sum(gender.com)
accury



# ??????V?c?x???
confusion_matrix <- table(table$?ʧO, gender.approved)

# ????????̫?
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])

# ??????l?^?v
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])

# ?????F1?????
f1_score <- 2 * (precision * recall) / (precision + recall)

# ???L????G
cat("???̫?:", precision, "\n")
cat("?l?^?v:", recall, "\n")
cat("F1?????:", f1_score, "\n")

