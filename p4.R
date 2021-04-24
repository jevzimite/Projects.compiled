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
set.seed(0)

df = iris
indx= sample(1:nrow(df), floor(nrow(df)/2))
train = df[indx,]
test = df[-indx,]

dfN=df
dfN$Species = NULL

# cor(dfN) %>%
#   corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
#            addCoef.col = TRUE,
#            p.mat = cor.mtest(dfN)$p,
#            sig.level = 0.05)

m1 = glm(Petal.Width ~. , data = df)
m2 = gam(Petal.Width ~ (Sepal.Length) + (Petal.Length) + s(Sepal.Width) + Species, data = train)

m3 = randomForest(Petal.Width ~ ., data = train, ntree = 500, mtry = 4)

tm = tab_model(m2, show.r2 = FALSE, transform = NULL,
                     digits = 3, digits.p = 4)

p1 = predict(m1, test)
p2 = predict(m2, test)
p3 = predict(m3, test)
p4 = (p1+p2+p3)/3
dd= describe(test$Petal.Width)

v1 = round( mean(abs(p1-test$Petal.Width))/dd$range , 3)
v2 = round( mean(abs(p2-test$Petal.Width))/dd$range ,3)
v3 = round( mean(abs(p3-test$Petal.Width))/dd$range ,3)
v4 = round( mean(abs(p4-test$Petal.Width))/dd$range , 3)

cbind(v1,v2,v3,v4)

ggplot()+
  geom_density(mapping=aes(test$Petal.Width), color = "darkred", fill = "darkred", alpha = 1)+
  geom_density(mapping=aes(p4), fill="deepskyblue3", color = "deepskyblue4", alpha=.6, linetype =6, size =1.5)