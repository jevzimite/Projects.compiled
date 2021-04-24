#### libraries ####
library(ggbiplot) 
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
# library(ggfortify)

#### data ####
marketing_data <- read_excel("marketing_data.xlsm")

df<- marketing_data
dfN<- df

dfN[,c(1:3,23)]=NULL
dfS<-scale(dfN)
dfN<- normalize(df)




#### whole data ####
pca<-prcomp(dfS)
scree1<-fviz_screeplot(pca) #2-5 groups
k1<-kmeans(dfS,2)

cluster<-fviz_cluster(k1, data = dfS)
teen<-ggbiplot(pca, groups = df$Teenhome, var.axes = 0)
kid<-ggbiplot(pca, groups = df$Kidhome, var.axes = 0)
simple<-ggbiplot(pca, var.axes = 1, alpha = 0)

# plot_grid(teen, kid, cluster, simple)





#### amount ####
mntDf<-df[,c(7:12)]
mntDf<- scale(mntDf)
pca.mnt<- prcomp(mntDf)

mnt1<-ggbiplot(pca.mnt, alpha = 0)
mnt2<- ggbiplot(pca.mnt, var.axes = 0)

scree2<- fviz_screeplot(pca.mnt) #2-3
k2<- kmeans(mntDf, 3)
cluster.mnt<-fviz_cluster(k2, mntDf)

# plot_grid(mnt1,mnt2, cluster.mnt, scree2)

#### number ####


numDf<- df[,c(13:17)]
numDF<- scale(numDf)
pca.num<- prcomp(numDf)

num1<-ggbiplot(pca.num, var.axes = 0)
num2<-ggbiplot(pca.num, alpha = 0, varname.adjust = .95, varname.size = 1.6)

scree3<- fviz_screeplot(pca.num) #3-5 . . . 4
k3<- kmeans(numDf, 4)
cluster.num<- fviz_cluster(k3, numDf)

#### plot grids ####


# plot_grid(mnt1,mnt2, cluster.mnt, scree2) #shows data about the amount
# plot_grid(num2, num1, cluster.num, scree3) #shows data about the number
plot_grid(teen, kid, cluster, simple) #shows all data


wineNsweets<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntWines, y=dfN$MntSweetProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()
fishNmeats<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntFishProducts, y=dfN$MntMeatProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()
sweetNfruits<-ggplot()+
  geom_point(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), size = 3, alpha=.15)+
  geom_smooth(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), color="black", size=0, alpha=.15, fill="darkred")+
  geom_smooth(mapping= aes(x=dfN$MntFruits, y=dfN$MntSweetProducts), method= "lm",color="black", size=.5, alpha=0, fill="darkred", linetype = 8)+
  theme_bw()


# install.packages("ggpubr")
library(ggpubr)
cor(x=dfN$MntWines, y=dfN$MntSweetProducts) ##wines and sweets
cor(x=dfN$MntFishProducts, y=dfN$MntMeatProducts) ##meat and fish
cor(x=dfN$MntFruits, y=dfN$MntSweetProducts) ## fruits and sweets
# wineNsweets
# fishNmeats
# sweetNfruits

# plot_grid(fishNmeats, wineNsweets,sweetNfruits)

