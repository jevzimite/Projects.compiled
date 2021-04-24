library(truncnorm)
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
library(gridExtra)
library(mgcViz)
library(corrplot)
library(readxl)
library(forecast)
library(readxl)
library(quantmod)
library(tscount)
library(circlize)
theme_set(theme_classic())
library(rethinking)
#### GOLEM ####
  fun =  {#function that gives prediction 
    pred = #prediction
    return(pred)
  }
  x = #list for sapply 
  res = sapply(x , fun)
  
#### MONTECARLO GRAPH ####
res_m <- melt(res)
ggplot() +
  geom_line(data = res_m, aes(x = Var1, y = value, group = Var2), alpha=.2, size = .5)

#### BASIC GRAPHS ####

par(mfrow=c(1,3))
plot(res, type="b")
boxplot(res)
hist(res)
describe(res)
