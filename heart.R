#### SET-UP ####
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

#### 1ST-OBSERVATION & DATA TRANSFORMATIONS ####
df=heart
dfN = df
dfN[,10]=NULL
dfN = scale(dfN)
pca = prcomp(dfN)
autoplot(pca, df, colour = "sex", loadings = 1, loadings.label = 1, size =3, alpha=.3)

corplot = cor(dfN) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(dfN)$p,
           sig.level = 0.05)

df = transform(df,
               age = as.integer(age),
               anaemia = as.factor(anaemia),
               creatinine_f = as.numeric(creatinine_f),
               diabetes = as.factor(diabetes),
               ejecFrac = as.integer(ejecFrac),
               hbp = as.factor(hbp),
               platelets = as.integer(platelets),
               s_creatinine = as.numeric(s_creatinine),
               sodium = as.integer(sodium),
               sex = as.factor(sex),
               smoking = as.factor(smoking),
               time = as.integer(time),
               death = as.factor(death)
)


index = sample(1:nrow(df), floor(nrow(df)/2))
train = df[index,]
test = df[-index,]

#### MODEL BUILDING ####

modelF = randomForest(death ~., data = train, ntree= 500, mtry = 4)
predF = predict(modelF, test)
tF = table(predF, test$death)
accuracyPerc1 = (tF[2,2] + tF[1,1]) / (tF[1,1] + tF[1,2] + tF[2,1] + tF[2,2])
falsePos1 = tF[1,2]
falseNeg1 = tF[2,1]
correctHits1 = tF[2,2] + tF[1,1]
resF = cbind(correctHits1, falsePos1, falseNeg1, accuracyPerc1)

LM = lm(death~age+ejecFrac +s_creatinine+ sodium+ time, data = train, family = binomial())

GLM1 = glm(death ~ ., data = train, family = binomial())

GLM2 = glm(death ~ smoking*diabetes+smoking*age+age*smoking+diabetes*age+creatinine_f*s_creatinine+platelets*hbp+hbp*age+age + anaemia + creatinine_f + diabetes + ejecFrac + hbp + platelets + s_creatinine + sodium + sex + smoking + time, data = train, family = binomial())

GAM = gam(death~s(age)+s(ejecFrac)+(s_creatinine)+ s(time), data = train, family = binomial())

tabModel = tab_model(GLM1, show.r2 = FALSE, transform = NULL,
                     digits = 3, digits.p = 4)
 
predGLM1 = round(predict(GLM1, test, type = "response"),0)
predGLM2 = round(predict(GLM2, test, type = "response"),0)
predGAM = round(predict(GAM, test, type = "response"),0)
predLM = round(predict(LM, test, type = "response"),0)
#### TABLE CONSTRUCTION ####
tLM1 = table(predGLM1,test$death)
accuracyPerc2 = (tLM1[2,2] + tLM1[1,1]) / (tLM1[1,1] + tLM1[1,2] + tLM1[2,1] + tLM1[2,2])
falsePos2 = tLM1[1,2]
falseNeg2 = tLM1[2,1]
correctHits2 = tLM1[2,2] + tLM1[1,1]
resLM1 = cbind(correctHits2, falsePos2, falseNeg2, accuracyPerc2)

tLM2 = table(predGLM2,test$death)
accuracyPerc3 = (tLM2[2,2] + tLM2[1,1]) / (tLM2[1,1] + tLM2[1,2] + tLM2[2,1] + tLM2[2,2])
falsePos3 = tLM2[1,2]
falseNeg3 = tLM2[2,1]
correctHits3 = tLM2[2,2] + tLM2[1,1]
resLM2 = cbind(correctHits3, falsePos3, falseNeg3, accuracyPerc3)

tGAM = table(predGAM,test$death)
accuracyPerc4 = (tGAM[2,2] + tGAM[1,1]) / (tGAM[1,1] + tGAM[1,2] + tGAM[2,1] + tGAM[2,2])
falsePos4 = tGAM[1,2]
falseNeg4 = tGAM[2,1]
correctHits4 = tGAM[2,2] + tGAM[1,1]
resGAM = cbind(correctHits4, falsePos4, falseNeg4, accuracyPerc4)

tLM = table(predLM,test$death)
accuracyPerc5 = (tLM[2,2] + tLM[1,1]) / (tLM[1,1] + tLM[1,2] + tLM[2,1] + tLM[2,2])
falsePos5 = tLM[1,2]
falseNeg5 = tLM[2,1]
correctHits5 = tLM[2,2] + tLM[1,1]
resLM = cbind(correctHits5, falsePos5, falseNeg5, accuracyPerc5)

### RESULTS ####
RESULTS = rbind(resF,resLM1,resLM2,resGAM, resLM )
rownames(RESULTS) = c("RandForest", "GLM1", "GLM2", "GAM", "LM")
RESULTS

