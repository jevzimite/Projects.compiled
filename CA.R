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
library(readxl)
library(ggfortify)
library(ggpubr)
library(FactoMineR)
library(gplots)
library(graphics)
#### ####
df<-consumerproducts
df[,5]=NULL
df[,1]=NULL
f1<-scale(df[,c(1,2)])
df<-cbind(f1, df[,3])
df<-normalize(df)
pca<-prcomp(df)

a<-autoplot(pca, consumerproducts, colour="Category", alpha=.5, frame=1, size=5, loadings=0, loadings.label=0)
b<-autoplot(pca, consumerproducts, colour="Profit", alpha=.5, frame=0, size=50, loadings=0)
c<-autoplot(pca, consumerproducts, colour="Revenue", alpha=.5, frame=0, size=50, loadings=0, color=2)

# categ<-autoplot(pca, consumerproducts, colour="Category", alpha=0, frame=1, size=0, loadings=0)
scree<-fviz_screeplot(pca)

k<-kmeans(df, 2)
d<-fviz_cluster(k,df, shape=1)

#### CA ####
# plot_grid(a,d,c,b)  
# dt<-data("housetasks")
# dt<-as.table(as.matrix(housetasks))
# pca<-prcomp(housetasks)
# chisq.test(housetasks)
# res.ca<-CA(housetasks, graph = 0)
# 
# # balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
# #             label = FALSE, show.margins = FALSE)
# 
# # autoplot(res.ca,housetasks)
# 
# fviz_ca(res.ca)
