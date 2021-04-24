
library(readxl)
amazonsample <- read_excel("~/Downloads/amazonsample.xlsx")
# View(samplestock)
library(ggplot2)

# ggplot(
#   data=amazonsample
# )+
#   # geom_line(mapping=aes(x=Date, y=Open), color="darkorange4")+
#   # geom_line(mapping=aes(x=Date, y=Close), color="dodgerblue4")+
#   geom_ribbon(mapping=aes(ymin=Low, ymax=High, x=Date), alpha=1, color="black", fill="black")+
#   # geom_line(mapping=aes(x=Date, y=High), color="black")+
#   # geom_line(mapping=aes(x=Date, y=Low), color="black")+
#   geom_hline(yintercept=0)+
#   geom_smooth(mapping=aes(x=Date, y=aver), size=.6, color="darkred",linetype=8, alpha=.15, fill="darkred")+
#   ggtitle("Amazon stocks")+labs(x="Date",y="Price")

library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
library(ggfortify)
library(ggpubr)


amazonsample[,1]=NULL
df<-amazonsample
amazonsample[,6]=NULL
amazonsample<-scale(amazonsample)
df<-scale(df)
pca<-prcomp(amazonsample)
pca2<-prcomp(df)
# autoplot(pca, df , size=20, alpha=1, loadings=1, loadings.label=1, colour="Volume")
autoplot(pca2, df, size=20, alpha=.5, loadings=1, loadings.label=1)

kp<-kmeans(df,2)
fviz_cluster(kp, df)