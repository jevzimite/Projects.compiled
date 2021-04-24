library(readxl)
consumerproducts <- read_excel("Desktop/R/consumerproducts.xlsm")
# View(consumerproducts)
colnames(consumerproducts)<- c("Company","Revenue", "Profit", "Percen", "Category")
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
#### PCA ####
set.seed(3)
df<-consumerproducts
dfF<- df

df[,5]=NULL
df[,1]=NULL
dfF[,1]=NULL

f1<-scale(df[,c(1,2)])
df<-cbind(f1, df[,3])
df<-normalize(df)
pca<-prcomp(df)

a<-autoplot(pca, consumerproducts, colour="Category", alpha=.5, frame=1, size=5, loadings=0, loadings.label=0)
# b<-autoplot(pca, consumerproducts, colour="Profit", alpha=.5, frame=0, size=50, loadings=0)
# c<-autoplot(pca, consumerproducts, colour="Revenue", alpha=.5, frame=0, size=50, loadings=0, color=2)

# categ<-autoplot(pca, consumerproducts, colour="Category", alpha=0, frame=1, size=0, loadings=0)
scree<-fviz_screeplot(pca)
#### RAND FOREST  ####

sapply(dfF, class)
dfF<- transform(dfF,
                Revenue=as.numeric(Revenue),
                Profit= as.numeric(Profit),
                Percen=as.numeric(Percen),
                Category=as.factor(Category)
)


size = floor(nrow(dfF)/2)
index = sample(1:nrow(dfF), size )

train = dfF[index,]
test = dfF[-index,]

model1<- randomForest(Category ~., data=train, ntree=500, mtry=2, importance=1)

pred<- (predict(model1, train)) #see below
t1<-table(pred, train$Category)  

pred2<- (predict(model1, test)) #see below
t2<-table(pred, test$Category)
balloonplot(t2, main = "test", xlab = "", ylab = "")
