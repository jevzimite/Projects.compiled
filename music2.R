#### Libraries ####
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

#### Data ####

spotify <- read_excel("Desktop/R/xlsm/spotify.xlsm")

set.seed(12345)
df=(sample_frac(spotify ,.5))
dfN=df
dfN$name=NULL
dfN=scale(dfN)

size=floor(nrow(df)/2)
indx=sample(1:nrow(df), size)
train=df[indx,]
test=df[-indx,]

#### Building and tuning ####

# corplot = cor(dfN) %>%
#   corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
#            addCoef.col = TRUE,  
#            p.mat = cor.mtest(dfN)$p,
#            sig.level = 0.05)


GAM = gam(popularity ~ s(acousticness) + s(energy) + acousticness*energy + s(year) + s(instrumentalness) + s(danceability), data = train)

tabModel = tab_model(GAM, show.r2 = FALSE, transform = NULL,
                     digits = 1, digits.p = 8)

int=select(train, acousticness, energy, year, instrumentalness, danceability, popularity)
int=sample_n(int,300)
# plot(int)

#### Results ####

predGAM=predict(GAM, test)
res=mean(abs(predGAM-test$popularity))/range(test$popularity)

resP = ggplot(test)+
  geom_point(mapping=aes(x= predGAM, y = popularity), size=10, alpha =.02)+
  geom_point(mapping=aes(x= popularity, y = popularity), size=.1, color = "darkred")+
  geom_smooth(mapping=aes(x= predGAM, y = popularity),color = "deeppink3", fill = "red", alpha =.3)

resC=ggplot(test)+
  geom_density(mapping=aes(predGAM), color = "deepskyblue4", fill = "deepskyblue3", alpha=.6, size = 1.3)+
  geom_density(mapping=aes(popularity), color = 1, fill = 1,  alpha = 0.5, size = 1.3, linetype=6)

list = abs(predGAM-test$popularity)
diff = data.frame(list)

results = cbind(test$popularity, round(predGAM,0), round(diff, 2))

resP
resC
res
