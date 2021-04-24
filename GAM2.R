##################################################################################################
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
set_theme(theme_sjplot2())

##################################################################################################

set.seed(123456)
df = iris
size = round((nrow(df)/2),0)
sampleSize = sample(1:nrow(df), size)
train = df[sampleSize,]
test1 = df[-sampleSize,]
test1[,4]= NULL
test2=test1
test2[,4]= NULL

##################################################################################################

model1 = glm(Petal.Width ~., data = train)
model2 = gam(Petal.Width ~ s(Sepal.Length, Sepal.Width, Petal.Length, k = 70), data = train)
model3 = randomForest(Petal.Width ~., train , ntree = 500, mtry = 2)

pred1 = predict(model1, test1)
pred2 = predict(model2, test2)
pred3=predict(model3, test1)

results = cbind(df[-sampleSize,]$Petal.Width,pred1,pred2,pred3)
results2 = cbind(df[-sampleSize,]$Petal.Width,pred3)

pred1res = abs(results[,1]-results[,2])
pred2res = abs(results[,1]-results[,3])
pred3res = round(abs(results2[,1]-results2[,2]),3)

ans = describe(data.frame(pred1res, pred2res, pred3res)) 
describeD = describe(df[-sampleSize,]$Petal.Width) 

##################################################################################################

comparing = ggplot(results)+
  geom_smooth(mapping= aes(x= results[,1], y = results[,2]), color = "darkred", alpha = 0, method = "lm" , size = .5)+
  geom_smooth(mapping= aes(x= results[,1], y = results[,3]), color = "steelblue", alpha = 0, method = "lm", size = .5)+
  geom_smooth(mapping= aes(x= results[,1], y = results[,4]), color = "darkgreen", alpha = 0, method = "lm" , size = .5)+
  
  geom_line(mapping= aes(x= results[,1], y = results[,1]), color = 1, linetype=6, size =.3, alpha = 1)

m1<-plot_model(model1, type = "pred", title = "GLM")
m2<-plot_model(model2, type = "pred", title = "GAM")

##################################################################################################
ans
comparing
# plot_grid(m1)
# plot_grid(m2)
# par(mfrow=c(1,3))
density( pred1 )
density( pred2 )
density( pred3 )

ggplot()+
  geom_density(mapping = aes(df$Petal.Width), color =1, fill=1, alpha=1, size=0)+
  geom_density(mapping = aes(pred1), color = "turquoise4", fill="turquoise3", alpha=.5)+
  geom_density(mapping = aes(pred3), color = "goldenrod4", fill="yellow", alpha=.2)+
  geom_density(mapping = aes(pred2), color = "darkred", fill="red", alpha=.2)



