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
library(readxl)
library(sjlabelled)
library(ggeffects)
library(sjPlot)
library(nlme)
library(mgcv) 
library(writexl)
library(ISLR)
library(voxel)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(mgcViz)
set_theme(theme_classic())

# spotify <- read_excel("Desktop/R/spotify.xlsm")

set.seed(12345)
df=(sample_frac(spotify ,.5))
dfN=df
dfN$name=NULL
dfN=scale(dfN)

size=floor(nrow(df)/2)
indx=sample(1:nrow(df), size)
train=df[indx,]
test=df[-indx,]

pca=prcomp(dfN)
pcaplot=autoplot(pca, df, colour="year", loadings.label=1, loadings=1)


GAM = gam(popularity ~ speechiness*duration_ms + s(speechiness) + s(duration_ms) +  s(energy) + acousticness + danceability + s(instrumentalness) + year, data = test)
predGAM=predict(GAM, test)
res=mean(abs(predGAM-test$popularity))/range(test$popularity)


resP = ggplot(test)+
  geom_point(mapping=aes(x= predGAM, y = popularity), size=10, alpha =.02)+
  geom_point(mapping=aes(x= popularity, y = popularity), size=.1, color = "darkred")+
  geom_smooth(mapping=aes(x= predGAM, y = popularity),color = "deeppink3", fill = "red", alpha =.3)



list = abs(predGAM-test$popularity)
diff = data.frame(list)

results = cbind(test$popularity, round(predGAM,0), round(diff, 2))

# write_xlsx(results, "Desktop/R/PREDspotify.xls")
