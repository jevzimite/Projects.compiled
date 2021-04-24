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


set.seed(1)
df = iris

size = 75
indx = sample(1:nrow(df), size)

train = df[indx,]
test = df[-indx,]

modelGAM = gam(Petal.Width ~ Sepal.Length + s(Sepal.Width) + Petal.Length + Species + Sepal.Length*Sepal.Width + 3,  data = df)
pseudoGLM = gam(Petal.Width ~ Sepal.Length + (Sepal.Width) + Petal.Length + Species + Sepal.Length*Sepal.Width , data = df)
modelGLM = glm(Petal.Width ~ ., data = train)


pred=predict(modelGAM, test)
abs=abs(pred-test$Petal.Width)
abs=data.frame(abs)
mean=describe(abs)
GAM=describe(test$Petal.Width)
GAM=mean$mean/GAM$range


pred2=predict(pseudoGLM, test)
abs2=abs(pred2-test$Petal.Width)
abs2=data.frame(abs2)
mean2=describe(abs2)
GLM=describe(test$Petal.Width)
GLM=mean2$mean/GLM$range

viz = getViz(modelGAM)
gamP<-(plot(viz, allTerms = T)+
         l_points() +
         l_fitLine(linetype = 1)  +
         l_ciLine(linetype = 3) +
         l_ciBar() +
         l_rug() +
         theme_bw())
print(gamP, pages = 1)

viz2 = getViz(pseudoGLM)
k = plot(viz2, allTerms = T)+
       l_points() +
       l_fitLine(linetype = 1)  +
       l_ciLine(linetype = 3) +
       l_ciBar() +
       l_rug() +
       theme_bw()
print(k, pages = 1)

ans=data.frame(cbind(GAM,GLM))

f <- randomForest(Petal.Width ~., ntree = 500, mtry=3, data = df)
pred3= predict(f, test)
res = describe ( abs(test$Petal.Width - pred3) )
res = res$mean/res$range
cbind(ans, res)

ggplot()+
        geom_density(mapping=aes(test$Petal.Width), fill = 1, alpha=1)+
        geom_density(mapping=aes(pred3), fill = "yellow", alpha=.3)+
        geom_density(mapping=aes(pred2), fill = "blue", alpha=.3)+
        geom_density(mapping=aes(pred), fill = "red", alpha=.6)


ggplot()+
        geom_line(mapping=aes(x=pred, y=test$Petal.Width), alpha=1, color = "red")+
        geom_line(mapping=aes(x=df$Petal.Width, y=df$Petal.Width), size=.25)+
        geom_line(mapping=aes(x=pred2, y=test$Petal.Width), alpha=.3, color = "blue")+
        geom_line(mapping=aes(x=pred3, y=test$Petal.Width), alpha=.3, color = "green")
