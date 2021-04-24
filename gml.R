#### LIBRARIES ####
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
library(ggfortify)
library(ggpubr)
library(FactoMineR)
library(gplots)
library(graphics)
library(randomForest)
#### DATA ####
cuse<-read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header = 1)
cuse<-transform(
  cuse,
  using =as.integer(using),
  notUsing=as.integer(notUsing),
  age=as.factor(age),
  education=as.factor(education),
  wantsMore=as.factor(wantsMore)
)

#### GML ####
noMore = cuse$wantsMore == "no"
hiEduc= cuse$education =="high"
lowEdu = cuse$education =="low"

modelR<-glm(wantsMore ~ age + education + using +notUsing, data= cuse, family = binomial)
newdata= data.frame(age = "<25", education= "high", using = 60 , notUsing = 40 ) 

a<-predict(modelR, cuse, type = "response")
a<- round(a,2)
# cbind(cuse, a)

b<-predict(modelR, newdata, type = "response")
b<- round(b,2)
# cbind(newdata, b)

#### RAND FOREST ####
set.seed(1234)

size<- floor(nrow(cuse)/2)
index <- sample(1:nrow(cuse), size)

train = cuse[index,]
test = cuse[-index,]

modelF<- randomForest(wantsMore ~ ., train , ntree= 500, mtry=5, importance=1)
pred<- predict(modelF, test)
t1<- table(pred, test$wantsMore)
balloonplot(t1, main="test", xlab = "pred", ylab="actual")