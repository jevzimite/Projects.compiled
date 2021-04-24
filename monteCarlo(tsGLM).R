#### LIBRARIES  ####
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

#### INPUTS ####
stock <- "ADXS"
nAhead = 3

#### DATA ####
tmp <- getQuote(stock)
stock <- getSymbols(stock,auto.assign = FALSE)
df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))

df = as.data.frame(df)
df = na.omit(df)

value = (df[,2] + df[,3])/2
df = cbind(df,value)
ts = ts(value, frequency = 365)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value")

oldV = value[(length(value)-6):length(value)]

#### OBS ####
# obs= (rnorm(1000,mean = 30, sd =7))
obs= runif(1000, min = 7, max = 90)

obsV1 = round(sample(obs, 1),0)
obsV2 = round(sample(obs, 1),0)
obsV3 = round(sample(obs, 1),0)
obsV4 = round(sample(obs, 1),0)
obsV5 = round(sample(obs, 1),0)
obsV6 = round(sample(obs, 1),0)
obsV7 = round(sample(obs, 1),0)
obsV8 = round(sample(obs, 1),0)
obsV9 = round(sample(obs, 1),0)
obsV10 = round(sample(obs, 1),0)
obsV11 = round(sample(obs, 1),0)
obsV12 = round(sample(obs, 1),0)
obsV13 = round(sample(obs, 1),0)
obsV14 = round(sample(obs, 1),0)
obsV15 = round(sample(obs, 1),0)
obsV16 = round(sample(obs, 1),0)
obsV17 = round(sample(obs, 1),0)
obsV18 = round(sample(obs, 1),0)
obsV19 = round(sample(obs, 1),0)
obsV20 = round(sample(obs, 1),0)
obsV21 = round(sample(obs, 1),0)
obsV22 = round(sample(obs, 1),0)
obsV23 = round(sample(obs, 1),0)
obsV24 = round(sample(obs, 1),0)
obsV25 = round(sample(obs, 1),0)

obsVb1 = round(sample(obs, 1),0)
obsVb2 = round(sample(obs, 1),0)
obsVb3 = round(sample(obs, 1),0)
obsVb4 = round(sample(obs, 1),0)
obsVb5 = round(sample(obs, 1),0)
obsVb6 = round(sample(obs, 1),0)
obsVb7 = round(sample(obs, 1),0)
obsVb8 = round(sample(obs, 1),0)
obsVb9 = round(sample(obs, 1),0)
obsVb10 = round(sample(obs, 1),0)
obsVb11 = round(sample(obs, 1),0)
obsVb12 = round(sample(obs, 1),0)
obsVb13 = round(sample(obs, 1),0)
obsVb14 = round(sample(obs, 1),0)
obsVb15 = round(sample(obs, 1),0)
obsVb16 = round(sample(obs, 1),0)
obsVb17 = round(sample(obs, 1),0)
obsVb18 = round(sample(obs, 1),0)
obsVb19 = round(sample(obs, 1),0)
obsVb20 = round(sample(obs, 1),0)
obsVb21 = round(sample(obs, 1),0)
obsVb22 = round(sample(obs, 1),0)
obsVb23 = round(sample(obs, 1),0)
obsVb24 = round(sample(obs, 1),0)
obsVb25 = round(sample(obs, 1),0)

ObsVb1 = round(sample(obs, 1),0)
ObsVb2 = round(sample(obs, 1),0)
ObsVb3 = round(sample(obs, 1),0)
ObsVb4 = round(sample(obs, 1),0)
ObsVb5 = round(sample(obs, 1),0)
ObsVb6 = round(sample(obs, 1),0)
ObsVb7 = round(sample(obs, 1),0)
ObsVb8 = round(sample(obs, 1),0)
ObsVb9 = round(sample(obs, 1),0)
ObsVb10 = round(sample(obs, 1),0)
ObsVb11 = round(sample(obs, 1),0)
ObsVb12 = round(sample(obs, 1),0)
ObsVb13 = round(sample(obs, 1),0)
ObsVb14 = round(sample(obs, 1),0)
ObsVb15 = round(sample(obs, 1),0)
ObsVb16 = round(sample(obs, 1),0)
ObsVb17 = round(sample(obs, 1),0)
ObsVb18 = round(sample(obs, 1),0)
ObsVb19 = round(sample(obs, 1),0)
ObsVb20 = round(sample(obs, 1),0)
ObsVb21 = round(sample(obs, 1),0)
ObsVb22 = round(sample(obs, 1),0)
ObsVb23 = round(sample(obs, 1),0)
ObsVb24 = round(sample(obs, 1),0)
ObsVb25 = round(sample(obs, 1),0)
ObsVbb1 = round(sample(obs, 1),0)
ObsVbb2 = round(sample(obs, 1),0)
ObsVbb3 = round(sample(obs, 1),0)
ObsVbb4 = round(sample(obs, 1),0)
ObsVbb5 = round(sample(obs, 1),0)
ObsVbb6 = round(sample(obs, 1),0)
ObsVbb7 = round(sample(obs, 1),0)
ObsVbb8 = round(sample(obs, 1),0)
ObsVbb9 = round(sample(obs, 1),0)
ObsVbb10 = round(sample(obs, 1),0)
ObsVbb11 = round(sample(obs, 1),0)
ObsVbb12 = round(sample(obs, 1),0)
ObsVbb13 = round(sample(obs, 1),0)
ObsVbb14 = round(sample(obs, 1),0)
ObsVbb15 = round(sample(obs, 1),0)
ObsVbb16 = round(sample(obs, 1),0)
ObsVbb17 = round(sample(obs, 1),0)
ObsVbb18 = round(sample(obs, 1),0)
ObsVbb19 = round(sample(obs, 1),0)
ObsVbb20 = round(sample(obs, 1),0)
ObsVbb21 = round(sample(obs, 1),0)
ObsVbb22 = round(sample(obs, 1),0)
ObsVbb23 = round(sample(obs, 1),0)
ObsVbb24 = round(sample(obs, 1),0)
ObsVbb25 = round(sample(obs, 1),0)

#### SIM ####
pred <- function(obs, nAhead){
n = c((length(ts)-obs):length(obs))

GLM = tsglm(ts, model = list(past_obs = obs, past_mean = round(mean(ts[n]),0)))
pred1 = predict(GLM, n.ahead=nAhead)

oldV = value[(length(value)-6):length(value)]

pred=pred1$pred
pred = c(oldV, pred)
length(pred)
return(pred)
}

xAxis = c(1:(nAhead+7))
plotting <- function(){
plot<- ggplot()+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV1, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV2, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV3, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV4, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV5, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV6, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV7, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV8, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV9, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV10, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV11, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV12, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV13, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV14, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV15, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV16, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV17, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV18, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV19, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV20, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV21, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV22, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV23, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV24, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsV25, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb1, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb2, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb3, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb4, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb5, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb6, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb7, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb8, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb9, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb10, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb11, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb12, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb13, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb14, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb15, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb16, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb17, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb18, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb19, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb20, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb21, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb22, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb23, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb24, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = obsVb25, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb1, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb2, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb3, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb4, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb5, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb6, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb7, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb8, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb9, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb10, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb11, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb12, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb13, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb14, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb15, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb16, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb17, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb18, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb19, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb20, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb21, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb22, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb23, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb24, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVb25, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb1, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb2, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb3, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb4, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb5, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb6, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb7, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb8, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb9, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb10, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb11, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb12, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb13, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb14, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb15, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb16, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb17, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb18, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb19, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb20, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb21, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb22, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb23, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb24, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=xAxis, y = pred(obs = ObsVbb25, nAhead = nAhead), color = rand_color(1), alpha=.5))+
  geom_line(mapping=aes(x=c(1:7), y = oldV))+
  theme(legend.position = "none")+ labs(title = "Monte Carlo simulation (100)")+xlab("Days")+ylab("Value")
  return(plot)
}


dist<-ggplot()+
  geom_density(mapping = aes(obs), size = 1.25, color = 'deepskyblue4', fill = "deepskyblue3", alpha = .55)+
  labs(title = "Distribution of 'obs' used")+xlab("Days used")+ylab("Density")

plt<-plotting()


#### NUMS SIM ####
pred2 <- function(obs){
  n = c((length(ts)-obs):length(obs))

  GLM = tsglm(ts, model = list(past_obs = obs, past_mean = round(mean(ts[n]),0)))
  pred1 = predict(GLM, n.ahead=nAhead)

  pred=pred1$pred
  length(pred)
  return(pred)
}

n=100
x <-round(sample(obs,n),0)
a=sapply(x, pred2)
day1=a[1,]
day2=a[2,]
day3=a[3,]

res<-cbind(
summary(day1),
summary(day2),
summary(day3)
)

smoothed<-ggplot()+
  geom_smooth(mapping=aes(y=day1, x=1:100), size = 2, alpha = .3, color = "steelblue", fill = "steelblue")+
  geom_smooth(mapping=aes(y=day2, x=1:100), size = 2, alpha = .3, color = "darkred", fill = "darkred")+
  geom_smooth(mapping=aes(y=day3, x=1:100), size = 2, alpha = .3, color = "darkolivegreen", fill = "darkolivegreen4" )+
  labs(title = "First three days simulated", subtitle = "Blue[1] ; Red [2] ; Green[3] ")+ ylab("Stock price")+ xlab("Index")
#### OUTPUTS ####
# dist
# smoothed
# plt
res
grid.arrange(dist,smoothed,plt)
