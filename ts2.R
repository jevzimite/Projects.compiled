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


df = economics
indx = sample(1:nrow(df), floor(nrow(df)/2))
train = df[indx,]
test =  df[-indx,]

ts2 = select(df, unemploy)
ts = select(df, unemploy)
ts = ts(ts, start=1, end=570, frequency=1)

# plot(ts) #overview of the ts

# boxplot(ts~  cycle(ts)) #see impact of each both

# ddata = decompose(ts, "multiplicative")  ##noy useful since there are no periods
# plot(ddata)

model =auto.arima(ts, trace = 0)
plot.ts(model$residuals)
forecast = forecast(model, level = 95, h = 10*1)
autoplot(forecast)

x = forecast$mean
plot(x)

forecast
