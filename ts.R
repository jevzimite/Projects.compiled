library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(ggfortify)1
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

df = AirPassengers

start(df)
end(df)
frequency(df)

plot(df)

cycle(df)
boxplot(df~  cycle(df))

tsdata = ts(df, frequency = 12)
ddata = decompose(tsdata, "multiplicative")
plot(ddata)

 model =auto.arima(df, trace = 1)
plot.ts(model$residuals)
forecast = forecast(model, level = 95, h = 10*12)
plot(forecast)

plot(forecast$mean)
# Ljung box
