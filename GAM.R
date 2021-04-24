#### LIBRARIES ####
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
library(nlme)
library(mgcv) 
library(writexl)

#### DATA ####
set.seed(9)
df = iris
size<-floor(nrow(df)/2)
index= sample(1:nrow(df), size)

train = df[index,]
test = df[-index,]

data = test$Sepal.Length
data=data.frame(Sepal.Length=data)

#### BUILDING & TESTING THE MODELS ####
model <- gam(Sepal.Width ~ s(Sepal.Length), data = train)
pred<-predict(model, data)

model1 = glm(Sepal.Width ~ Sepal.Length, data=train)
pred1= predict(model1, data)

#### COMPARING THE RESULTS ####
res<-cbind(test$Sepal.Width,round(pred,1), round(pred1,1))
predr=abs(res[,1]-res[,2])
pred1r=abs(res[,1]-res[,3])
summ<-summary(cbind(predr, pred1r)) #model had an average error of 0.332 /// model1 had an average error of 0.3533
descr<-describe(df$Sepal.Width) #sd is 0.44

#### PLOTS ####
par(mfrow=c(1,1))
ggplot(df)+
  geom_smooth(mapping=aes(Sepal.Length , Sepal.Width), method = "lm", size = .7, alpha = .2, color ="darkred", fill = "darkred")+
  geom_smooth(mapping=aes(Sepal.Length , Sepal.Width), size = .7, alpha = .2, color ="turquoise4", fill = "turquoise3")+
  geom_point(mapping=aes(Sepal.Length , Sepal.Width), size = 5, alpha = .3)+ 
  theme_bw()+labs(title = "model vs model1")
plot(df[,1:4])
par(mfrow=c(1,2))
plot(density(df$Sepal.Length))
plot(density(df$Sepal.Width))

#### EXAMPLE OF EXPORTING INTO EXCEL ####
# dfExp<- data.frame(res)
# write_xlsx(dfExp,"Desktop/R/results.xlsx")