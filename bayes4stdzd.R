stock <- "MVY.V" 
nAhead = 7
f = 1
t = 30
l = t-f+1
obs = l
##### DATA ####


tmp <- getQuote(stock)
stock <- getSymbols(stock,auto.assign = FALSE)
df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
df = as.data.frame(df)
df = na.omit(df)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#### model ####

# i = abs(normalize(df$High-df$Low)) +1 #volatility
i = (df$Close-df$Open)/df$Open+1 #growth per day ; most theory

# i = (df$High)+(df$Low)/2 #value ; most utility (NOT RECOMMENDED FOR THE RUNNING CODE)

bayes<-function(i , f , t){
  
  ts = ts(i, frequency = 365)
  pred2 <- function(obs){
    n = c((length(ts)-obs):length(obs))
    
    GLM = tsglm(ts, model = list(past_obs = obs, past_mean = round(mean(ts[n]),0)))
    pred1 = predict(GLM, n.ahead=nAhead)
    
    pred=pred1$pred
    length(pred)
    return(pred)
  }
  
  l = t-f+1
  x <-seq(from=f, to = t, length.out = l)
  a=sapply(x, pred2)
  return(a)
}
a<-bayes(i, f ,t)
a= a - 1
a = a*100


par(mfrow=c(1,3))
plot(a, type="b")
boxplot(a)
hist(a)
describe(a)

y <- matrix(rnorm(100), 10, 10)
library(reshape2)
a_m <- melt(a)

# a_m$value[ a_m$value > 0 & a_m$va < 0.5] = NA

yy = rowMeans(a)
xx = 1:length(yy)

ggplot() +
  geom_ribbon(data = a_m, aes(x = Var1, ymax = value,ymin=0, group = Var2), alpha=.2, size = .5, fill = "red")+
  geom_line(mapping=aes(x = xx, y=yy), color = 1, size =.65, linetype =8, fill = "darkred")+
  labs(main = "MonteCarlo simulation")+ xlab("Days ahead")+ ylab( "i ~ nAhead ")


par(mfrow=c(1,))
for (i in 1:nAhead){
  hist(a[i,])
}


hist(xx)

