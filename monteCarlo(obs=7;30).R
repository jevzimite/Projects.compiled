# Assumptions:
#   The stock market has a momentum and can be described when studying the last [7:30] entries of data
#   The chance of other variables influencing the results are low and negligible on the short run

# This model of prediction for momentum must be used before the opening and after the closing of the stock market
# ARIMA should be used to when determining the selling price

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
nAhead = 7



mCarlo <- function(stock, nAhead){
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
obs= runif(1000, min = 7, max = 30)

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
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 7, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 8, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 9, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 10, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 11, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 12, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 13, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 14, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 15, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 16, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 17, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 18, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 19, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 20, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 21, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 22, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 23, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 24, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 25, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 26, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 27, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 28, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 29, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=xAxis, y = pred(obs = 30, nAhead = nAhead), color = rand_color(1), alpha = 1))+
    geom_line(mapping=aes(x=c(1:7), y = oldV))+
    theme(legend.position = "none")+ labs(title = "Monte Carlo simulation [7:30]")+xlab("Days")+ylab("Value")
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

n=50
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
  geom_smooth(mapping=aes(y=day1, x=1:50), size = 2, alpha = .3, color = "steelblue", fill = "steelblue")+
  geom_smooth(mapping=aes(y=day2, x=1:50), size = 2, alpha = .3, color = "darkred", fill = "darkred")+
  geom_smooth(mapping=aes(y=day3, x=1:50), size = 2, alpha = .3, color = "darkolivegreen", fill = "darkolivegreen4" )+
  labs(title = "First three days simulated", subtitle = "Blue[1] ; Red [2] ; Green[3] ")+ ylab("Stock price")+ xlab("Index")

#### OUTPUTS ####

# res
final <- grid.arrange(dist,smoothed,plt)

return(
list(res, final))
}

mCarlo(stock, nAhead)