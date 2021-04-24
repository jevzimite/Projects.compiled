##### LIBARIES ####
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


#### INPUTS ####
stock <- "NOU.V" #symbol used in yahoo finances
obs = 30 #how many of the last entries are being used to predict new values
nAhead = 3 #how many days ahead are being predicted
#### FUNCTIONS ####
dist<-function(stock,obs){
  percenC<-function(stock, obs){
    #### DATA ####
    tmp <- getQuote(stock)
    stock <- getSymbols(stock,auto.assign = FALSE)
    df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
    
    df = as.data.frame(df)
    df = na.omit(df)
    
    value = (df[,2] + df[,3])/2
    value = (value) #normalize?
    df = cbind(df,value)
    colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value") 
    
    n = nrow(df)
    n2 = nrow(df)-obs
    low = df$Low[n2:n]
    high = df$High[n2:n]
    value2 = df$Value[n2:n]
    
    delta = high - low
    delta = as.numeric((high-low) / value2)
    delta = (delta) #/100#normalize?
    percen <- mapply("/", delta, value2, SIMPLIFY = FALSE)
    percen = as.double(percen)
    
    obs = obs+1
    slopeX = obs-2
    slopeY = percen[(length(percen)-2):length(percen)]
    
    plot <-ggplot()+
      geom_ribbon(mapping=aes(ymin=0, ymax=percen, x = 1:obs, fill = "red", alpha =.2,))+
      geom_smooth(mapping=aes(x = 1:obs, y=percen), size = .8, fill = "red", alpha =0, color = "darkred" )+
      labs(main="Differences between hi-lo on prices")+ylab("Volatility(%)")+xlab("Days")+
      # geom_hline(yintercept = 0, color = 1, linetype=8)
      theme(legend.position = "none")+
      geom_smooth(mapping=aes(x=slopeX:obs, y =slopeY), fullrange = T, method = "lm", alpha = 0, size = .6, color = "brown3", linetype = 5)+
      ylim(0,max(percen))
    
    return(plot)
  }
  plot<-percenC(stock,obs)
  
  corrV<-function(stock,obs){
    tmp <- getQuote(stock)
    stock <- getSymbols(stock,auto.assign = FALSE)
    df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
    
    df = as.data.frame(df)
    df = na.omit(df)
    
    value = (df[,2] + df[,3])/2
    value = (value) #normalize?
    df = cbind(df,value)
    colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value") 
    
    n = nrow(df)
    n2 = n - obs
    value = df$Value
    range = df$High-df$Low
    value = value[n2:n]
    range = range[n2:n]
    days = 1:length(range)
    range2 = df$Close-df$Open
    range2 = range2[n2:n]
    close = df$Close
    open = df$Open
    close = close[n2:n]
    open = open[n2:n]
    volume = normalize(df$Volume)
    volume = volume[n2:n]
    ##divide the range in 
    
    df = data.frame(cbind(days, range, value, range2))
    
    # value = normalize(value)
    # range = normalize(range)
    # range2 = normalize(range)
    # close = normalize(close)
    # open = normalize(open)
    
    ########################################################################
    tbl = data.frame(range, open, close, range2)
    
    m2 = lm(close ~ open, data =tbl)
    new = data.frame(open = tmp$Last)
    pred2=data.frame(predict(m2, newdata = new, interval="confidence"))
    rX = open[length(open)] - pred2$fit
    
    m3 = lm(range ~ range2, data = tbl)
    new = data.frame(range2 = rX)
    pred3=data.frame(predict(m3, newdata = new, interval="confidence"))
    newRange = abs(c(pred3$fit))
    
    dist = rtruncnorm(n=1000000, a=0, b=newRange*3, mean=newRange, sd = sd(df$range))
    
    curve =   ggplot()+
      geom_density(mapping=aes(dist), color = "deepskyblue4", size = 1.2, fill = "deepskyblue3", alpha = .4)+
      geom_vline(mapping = aes(xintercept = newRange), color = "deepskyblue4", linetype = 8, size = 1.2)+
      xlab("Volatility($)")+ylab("Probability(%)")
    
    j =obs+1
    u =obs+2
    ju = rbind(j,u)
    k = rbind(range[length(range)],newRange)
    newV = data.frame(range = k, days =ju)
    row.names(newV)= NULL
    
    
    ########################################################################
    model = lm(value ~range, data = df)
    new <- data.frame(range = df$range, range2 = df$range2)
    pred=data.frame(predict(model, newdata = new, interval="confidence"))
    
    r = corr.test(value,range)
    r= c(r$r)
    
    COR = ggplot()+
      geom_line(mapping=aes(x = df$days, y = df$value))+
      geom_ribbon(mapping=aes(x = df$days, ymin = 0, ymax=range), fill = "red", alpha = .5, size = 0)+
      # geom_ribbon(mapping=aes(x = days, ymin = 0, ymax= volume), fill = 1, alpha = .25, size = 0)+
      # geom_ribbon(mapping=aes(x = df$days, ymin = 0, ymax=range2), fill = 1, alpha = .5, size = 0)+
      geom_ribbon(mapping = aes(x=days, ymin = pred$lwr, ymax=pred$upr), fill ="blue", alpha = .3, size = 0)+
      geom_line(mapping=aes(x=days, y=pred$fit), color ="blue", alpha =.7)+
      geom_ribbon(mapping = aes(x=newV$days, ymin = 0, ymax=newV$range), fill ="red", alpha = .8, size = 0)+
      geom_text(aes(x = obs/4, y = mean(pred$fit)*1.8, label = round(r,3)), size=3) +
      xlab("Days")+ylab("Price")+
      geom_hline(mapping=aes(yintercept=newRange), color = "darkred", linetype =8, alpha = .3)
    rets = list(COR, curve)
    rets
    return(rets)
  }
  u<-corrV(stock, obs)
  cor = u[[1]]
  curve = u[[2]]
  ret=grid.arrange(plot,cor,curve)
  return(ret)
}
  ARIMA<-function(stock, nAhead, obs){
  
  
  tmp <- getQuote(stock)
  stock <- getSymbols(stock,auto.assign = FALSE)
  df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
  
  
  df = as.data.frame(df)
  df = na.omit(df)
  
  nAhead = 7
  
  #### ARIMA ####
  value = (df[,2] + df[,3])/2
  df = cbind(df,value)
  ts = ts(value, frequency = 365)
  colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value")
  
  # model = auto.arima(ts)
  model = auto.arima(ts , max.p = 20, max.q = 20, ic = "bic", stepwise = 0, approximation = 0, trace = 0)
  forecast = forecast(model, level = 95, h = nAhead)
  
  #### GLM ####
  obs = 30
  
  n = c((length(ts)-obs):length(obs))
  
  # GLM = tsglm(ts, model = list(past_obs = obs, past_mean = mean(ts)))
  GLM = tsglm(ts, model = list(past_obs = obs, past_mean = round(mean(ts[n]),0)))
  pred1 = predict(GLM, n.ahead=nAhead)
  
  
  
  #### RESULTS ####
  dd = decompose(ts)
  
  # autoplot(forecast, ylab = "Value")
  a = autoplot(forecast$mean, ylab= "Value", main = "Forecast from ARIMA")
  b = autoplot(pred1$pred, ylab = "Value", main = "Forecast from tsGLM")
  
  results = cbind(forecast$mean,pred1$pred)
  colnames(results) = c("ARIMA", "GLM")
  return(list(a,b,plot(dd), forecast) )
}
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
#### OUTPUTS ####
dist(stock, obs) #intensity of the momentum ; trend line shows tren in the past two days
ARIMA(stock, nAhead, obs) #range of the predictions based on the momentum
mCarlo(stock, nAhead) #direction of the momentum; aids aproximating the range
