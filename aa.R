#### LIBRARY ####
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

nAhead = 3
obs = 30
stock <- c("DD")
tmp <- getQuote( stock )

####  DATA ####
stock <- getSymbols(stock,auto.assign = FALSE)
df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
df = as.data.frame(df)
df = na.omit(df)
value = (df[,2] + df[,3])/2
df = cbind(df,value)
ts = ts(value, frequency = 365)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value")
n = c((length(ts)-obs):length(obs))

########################################################################################################################

model = auto.arima(ts , max.p = 20, max.q = 20, ic = "bic", stepwise = 0, approximation = 0, trace = 0)
forecast = forecast(model, level = 70, h = nAhead)

monteCarlo <- function(fCast){
upper=forecast$upper
lower=forecast$lower
stdev = describe(df[((nrow(df)-3):nrow(df)),7])
mean= stdev$mean
stdev =stdev$sd

newDist=(rnorm(1000, mean(forecast$mean), stdev))
newDist=data.frame(newDist[(newDist >= mean(lower) & newDist <= mean(upper))])
# hist(newDist$newDist..newDist....mean.lower....newDist....mean.upper...)

nV = sample_n(newDist,1)
nV=c(nV)
nV=data.frame(nV)

oldV=data.frame(df[,7])
n = nrow(oldV)
n2 = n-4
oldV = oldV[n2:n,]
oldV = data.frame(oldV)

pred <- (mapply(c, oldV, nV, SIMPLIFY=FALSE))
pred = data.frame(pred)
return(pred)
}

plotMonteC <- function(monteCarlo){
  pp1 = monteCarlo(forecast)
  pp2 = monteCarlo(forecast)
  pp3 = monteCarlo(forecast)
  pp4 = monteCarlo(forecast)
  pp5 = monteCarlo(forecast)
  pp6 = monteCarlo(forecast)
  pp7 = monteCarlo(forecast)
  pp8 = monteCarlo(forecast)
  pp9 = monteCarlo(forecast)
  pp10 = monteCarlo(forecast)
  pp1a = monteCarlo(forecast)
  pp2a = monteCarlo(forecast)
  pp3a = monteCarlo(forecast)
  pp4a = monteCarlo(forecast)
  pp5a = monteCarlo(forecast)
  pp6a = monteCarlo(forecast)
  pp7a = monteCarlo(forecast)
  pp8a = monteCarlo(forecast)
  pp9a = monteCarlo(forecast)
  pp10a = monteCarlo(forecast)
Pp1 = monteCarlo(forecast)
Pp2 = monteCarlo(forecast)
Pp3 = monteCarlo(forecast)
Pp4 = monteCarlo(forecast)
Pp5 = monteCarlo(forecast)
Pp6 = monteCarlo(forecast)
Pp7 = monteCarlo(forecast)
Pp8 = monteCarlo(forecast)
Pp9 = monteCarlo(forecast)
Pp10 = monteCarlo(forecast)
Pp1a = monteCarlo(forecast)
Pp2a = monteCarlo(forecast)
Pp3a = monteCarlo(forecast)
Pp4a = monteCarlo(forecast)
Pp5a = monteCarlo(forecast)
Pp6a = monteCarlo(forecast)
Pp7a = monteCarlo(forecast)
Pp8a = monteCarlo(forecast)
Pp9a = monteCarlo(forecast)
Pp10a = monteCarlo(forecast)
p1 = monteCarlo(forecast)
p2 = monteCarlo(forecast)
p3 = monteCarlo(forecast)
p4 = monteCarlo(forecast)
p5 = monteCarlo(forecast)
p6 = monteCarlo(forecast)
p7 = monteCarlo(forecast)
p8 = monteCarlo(forecast)
p9 = monteCarlo(forecast)
p10 = monteCarlo(forecast)
p1a = monteCarlo(forecast)
p2a = monteCarlo(forecast)
p3a = monteCarlo(forecast)
p4a = monteCarlo(forecast)
p5a = monteCarlo(forecast)
p6a = monteCarlo(forecast)
p7a = monteCarlo(forecast)
p8a = monteCarlo(forecast)
p9a = monteCarlo(forecast)
p10a = monteCarlo(forecast)



dates = c(1:6)
pp = cbind(dates, pp1,pp2,pp3,pp4,pp5,pp6,pp7,pp8,pp9,pp10,
           pp1a,pp2a,pp3a,pp4a,pp5a,pp6a,pp7a,pp8a,pp9a,pp10a,
           Pp1,Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8,Pp9,Pp10,
           Pp1a,Pp2a,Pp3a,Pp4a,Pp5a,Pp6a,Pp7a,Pp8a,Pp9a,Pp10a,
           p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
           p1a,p2a,p3a,p4a,p5a,p6a,p7a,p8a,p9a,p10a)
colnames(pp) <- c("dates" , 1:60)
pp = data.frame(pp)

plot = ggplot(pp)+
  geom_line(mapping=aes(dates, X1), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X2), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X3), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X4), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X5), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X6), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X7), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X8), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X9), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X10), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X11), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X12), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X13), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X14), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X15), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X16), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X17), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X18), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X19), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X20), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X21), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X22), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X23), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X24), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X25), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X26), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X27), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X28), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X29), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X30), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X31), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X32), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X33), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X34), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X35), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X36), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X37), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X38), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X39), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X40), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X41), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X42), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X43), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X44), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X45), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X46), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X47), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X48), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X49), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X50), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X51), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X52), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X53), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X54), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X55), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X56), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X57), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X58), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X59), size=1, alpha = .15)+
  geom_line(mapping=aes(dates, X60), size=1, alpha = .15)
  return(plot)
}

plotMonteC(monteCarlo)

