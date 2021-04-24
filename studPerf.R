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

set.seed(0)
StudentsPerformance <- read_excel("StudentsPerformance.xlsm")
df = StudentsPerformance

sapply(df, class)

df = transform(df,
          gender = as.factor(gender),
          race = as.factor(race),
          edu = as.factor(edu),
          lunch = as.factor(lunch),
          prepTest = as.factor(prepTest),
          math = as.numeric(math),
          reading = as.numeric(reading),
          writing = as.numeric(writing)
          )

indx = sample(1:nrow(df), floor(nrow(df)/2))
train = df[indx,]
test = df[-indx,]

GLM = glm(prepTest~., train, family = binomial())
RANDF = randomForest(prepTest ~., data = train, ntree = 500, mtry = 4)

tab_model(GLM)

p1 = predict(GLM, test, type = "response")
p2 = predict(RANDF, test)

p1=levels(p1) = c(1,2)
t = cbind(round(p1, 0),test$prepTest)
resGLM = length(t[t== TRUE])/500

t2 = table(p2,test$prepTest)
resF = (t2[1,1]+t2[2,2])/500

resGLM
resF