####  LIBRARIES  ####
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(ggfortify)
library(ggpubr)
library(FactoMineR)
library(gplots)
library(graphics)
library(randomForest)
library(data.table)
library(ca)
####  DATA   ####
data("footsize")
df<- footsize
set.seed(123)


size = floor(nrow(df)/2)
index = sample(1:nrow(df), size)

train = df[index,]
test = df[-index,]

model1<-randomForest(sex ~., train, ntree = 600, mtry = 2 , importance = 1)

pred0<- predict(model1, train)        
table(pred0, train$sex)

pred1<- predict(model1, test)
t2<-table(pred1, test$sex)
balloonplot(t2, main="Test sample", xlab = "Pred", ylab = "Actual")

test2<-c(185, 42)
test2<-data.frame(test2)
test2<-t(test2)
colnames(test2)<-c("size", "footsize")

pred2<-predict(model1, test2)
pred2
