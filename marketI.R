#### libraries ####
# library(ggbiplot) 
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
library(ggfortify)
library(ggpubr)

#### data ####
marketing_data <- read_excel("marketing_data.xlsm")

df<- marketing_data
dfN<- df

dfN[,c(1:3,23)]=NULL
dfS<-scale(dfN)
dfN<- normalize(df)

set.seed(1)
#### kmeans/pca data ####
pca<-prcomp(dfS)
scree1<-fviz_screeplot(pca) #2-5 groups
k1<-kmeans(dfS,2)

cluster<-autoplot(k1, dfS, frame=1, size=2, alpha=.33)+theme_bw()
countries<-autoplot(pca, data= df, loadings=1, loadings.label = 1, alpha=.0, size=10, colour = "Country", loadings.colour = "darkred", loadings.label.colour= 1, frame=1)+theme_bw()
teen<-autoplot(pca, data= df, loadings=1, loadings.label = 1, alpha=.05, size=10, colour = "Teenhome", loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
kid<- autoplot(pca, data= df, loadings=1, loadings.label = 1, alpha=.05, size=10, colour = "Kidhome", loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
s1<-autoplot(pca, alpha=.33)+theme_bw()

#### amount ####
mntDf<-df[,c(7:12)]
mntDf<- scale(mntDf)
pca.mnt<- prcomp(mntDf)

scree2<- fviz_screeplot(pca.mnt) #2-3
k2<- kmeans(mntDf, 2)

cluster.mnt<-autoplot(k2, data= mntDf, loadings.label = 0, alpha=.33, size=2, frame=1)+theme_bw()
mnt1<-autoplot(pca.mnt, data= df, loadings=1, loadings.label = 1, alpha=.5, colour="Country", size=1, loadings.colour = "darkred", loadings.label.colour= 1, frame=1)+theme_bw()
mnt2<-autoplot(pca.mnt, data= df, loadings=1, loadings.label = 1, alpha=.1, colour="Kidhome", size=10, loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
mnt3<-autoplot(pca.mnt, data= df, loadings=1, loadings.label = 1, alpha=.1, colour="Teenhome", size=10, loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
s2<-autoplot(pca.mnt, alpha=.33)+theme_bw()

#### number ####
numDf<- df[,c(13:17)]
numDF<- scale(numDf)
pca.num<- prcomp(numDf)
scree3<- fviz_screeplot(pca.num) #3-5 . . . 4
k3<- kmeans(numDf, 3)

cluster.num<- autoplot(k3, dfS, frame=1, size=2, alpha=.33)+theme_bw()
num1<-autoplot(pca.num, data= df, loadings=1, loadings.label = 1, alpha=.1, size=10, colour = "Teenhome", loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
num2<-autoplot(pca.num, data= df, loadings=1, loadings.label = 1, alpha=.1, size=10, colour = "Kidhome", loadings.colour = "darkred", loadings.label.colour= 1, frame=0)+theme_bw()
num3<-autoplot(pca.num, data= df, loadings=1, loadings.label = 1, alpha=.0, size=10, colour = "Country", loadings.colour = "darkred", loadings.label.colour= 1, frame=1)+theme_bw()
s3<-autoplot(pca.num, alpha=.33)+theme_bw()
#### plots ####
wineNsweets<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()+labs(x="Wine", y="Sweet products")
fishNmeats<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()+labs(x= "Fish products", y="Meat products")
sweetNfruits<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()+labs(x="Fruits", y="Sweet products")

#### plot grids ####

a<-plot_grid(s1, cluster,     scree1, teen, kid, countries, labels = c("PCA","k-means Cluster", "","PCA (kid in household","PCA (Teen in household)", "PCA (Based on countries)"))
b<-plot_grid(s2, cluster.mnt, scree2, mnt3, mnt2, mnt1, labels = c("PCA","k-means Cluster", "","PCA (kid in household","PCA (Teen in household)", "PCA (Based on countries)"))
b1<-plot_grid( fishNmeats, wineNsweets,sweetNfruits)
c<-plot_grid(s3, cluster.num, scree3, num2, num1, num3, labels = c("PCA","k-means Cluster", "","PCA (kid in household","PCA (Teen in household)", "PCA (Based on countries)"))

z<-autoplot(pca, data= df, loadings=1, loadings.label = 1, alpha=.0, size=10, colour = "Education", loadings.colour = "darkred", loadings.label.colour= 1, frame=1)+theme_bw()

a
b
b1
c