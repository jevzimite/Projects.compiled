library(ggbiplot) #function needed to graph prcomp like files
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
business_dynamics <- read_excel("~/Downloads/business_dynamics.xlsm")
simple <- read_excel("~/Downloads/simplebus.xlsm")


df <- business_dynamics
dfO<- df
df$Year=NULL
dfN<-normalize(df)
# dfN<- dfN[,c(3,7)]


dfN<-scale(df)
pca<- prcomp(df)
pcaN<- prcomp(dfN)

p<- ggbiplot(pca)
pN<- ggbiplot(pcaN, groups = dfO$Year, varname.abbrev = 0, var.axes = TRUE, ellipse = TRUE, alpha = 0)+ theme_classic()

pD<- ggbiplot(pcaN, groups = dfO$Year, varname.abbrev = 0, var.axes = 0, ellipse = 0, alpha = 1)+ theme_classic()


dfN<-normalize(df)



#### density ####
d1<-density(dfN$Number.of.Firms)
d2<-density(dfN$Creation.Rate)
d3<-density(dfN$Entered.Rate)
d4<-density(dfN$Exited.Rate)
d5<-density(dfN$Locations)
d6<-density(dfN$Data.Job.Destruction.Rate)
d7<-density(dfN$.Reallocation.Rate)



dc<-ggplot()+
  #Creation rate is red
  geom_density(mapping=aes(dfN$Creation.Rate), color = "darkred", fill = "darkred", alpha = .2)+
  #Number of businesses
  geom_density(mapping=aes(dfN$Number.of.Firms), color ="darkgoldenrod1", fill ="darkgoldenrod1", alpha = .2)+
  geom_density(mapping=aes(dfN$Locations), , color ="darkgoldenrod1", fill ="darkgoldenrod1", alpha = .2)+
  #How much business move
  geom_density(mapping=aes(dfN$Entered.Rate), color = "steelblue", fill = "steelblue", alpha = .2)+
  geom_density(mapping=aes(dfN$Exited.Rate), color = "steelblue", fill = "steelblue", alpha = .2)+
  geom_density(mapping=aes(dfN$Data.Job.Destruction.Rate), color = "steelblue", fill = "steelblue", alpha = .2)+
  geom_density(mapping=aes(dfN$.Reallocation.Rate), color = "steelblue", fill = "steelblue", alpha = .2)+
  xlim(-.1,1)



dfN<-scale(df)




scree<-fviz_screeplot(pcaN)
kdata<-kmeans(dfN, 5)

# plot_grid(pN, dc)
#### simplified dens ####

colMeans(dfN[,c(1,6)])

newDf<- simple
newDf$Year<- NULL

pcaSimple<- prcomp(newDf)
ps<-ggbiplot(pcaSimple, groups = simple$Year, ellipse = TRUE, alpha=0)
# plot_grid(ps, pN, dc)



dfN<-normalize(df)


ndc<- ggplot()+
  geom_density(mapping=aes(x = simple$NumberOf), color= "darkgoldenrod1", alpha = .3, size= 1.2, linetype=6, fill = "darkgoldenrod1")+
  geom_density(mapping=aes(x = simple$Creation.Rate), color= "darkred", alpha = .3, size= 1.2, linetype=6, fill = "darkred")+
  geom_density(mapping=aes(x = simple$Movement), color= "steelblue", alpha = .3, size= 1.2, linetype=6, fill = "steelblue")+
  xlim(-5,10)

plot_grid(ndc,dc)



dfN<-scale(df)
#### ####

cluster<-fviz_cluster(kdata, data=dfN)
plot_grid(scree, pN, pD, cluster)


# cluster
