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
#### PCA & k-means ####
# df1<- iris #dataset
# set.seed(18) #makes random numbers reproduceable
# df1[,5]= NULL
# dfN<-scale(df1)
# pca<-prcomp(dfN)
# p1<-fviz_screeplot(pca)
# p2<-autoplot(pca, df, colour="Species", frame = 1, frame.type="norm", size=5, alpha=.5, loadings=1, loadings.label=1, loadings.colour="darkred", loadings.label.colour=1)+theme_classic2()
# k<-kmeans(dfN, 3)
# p3<-autoplot(k, df, frame = 1, size=5, alpha=.5, loadings=1, loadings.label=1, loadings.colour="darkred", loadings.label.colour=1)+theme_classic2()
# 
# # plot_grid(p1,p2,p3)
####  RANDOM FOREST ####
df<- iris #dataset
set.seed(18) #makes random numbers reproduceable

size<-floor(nrow(df)/2) #divides the sample into 2 parts
index<-sample(1:nrow(df), 75) #sets the indexes that will determine whether something goes into the testing or the training set

train<-df[index,] 
test<-df[-index,]
test0<-test
test0[,5]=NULL
model1<- randomForest(Species ~., data=train, ntree=100, mtry=6, importance=1) #makes the model to be used for prediction

pred<- (predict(model1, train)) #see below
t1<-table(pred, train$Species)    #asses the training set after being compared to the model

pred1<- (predict(model1, test0)) #see below
t2<-table(pred1, test$Species) #assesses the testing set compared to the model
# par(mfrow=c(1,2))
balloonplot(t1, main ="Train", xlab = "")
balloonplot(t2, main = "Test", xlab = "")
