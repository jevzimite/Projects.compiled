# This model intends to show how did the price changed during the day for a given stock


stock <- "GME"
obs = 7#see when was the last time there was a spike in %change
f1<-function(stock, obs){
# nAhead = 3
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
delta = delta / mean(delta)
delta = (delta) #normalize?
percen <- mapply("/", delta, value2, SIMPLIFY = FALSE)
percen = as.double(percen)

obs = obs+1
plot <-ggplot()+
  geom_smooth(mapping=aes(x = 1:obs, y=percen), size = .8, fill = "red", alpha =.2, color = "darkred" )+
  labs(main="Differences between hi-lo on prices")+ylab("Variability")+xlab("Days")+
  geom_hline(yintercept = 0, color = 1, linetype=8)

return(plot)
}
# ts<-ts(percen, frequency = 1)
# model = auto.arima(ts , max.p = 20, max.q = 20, ic = "bic", stepwise = 0, approximation = 0, trace = 0)
# forecast(model, level = 95, h = nAhead)

f1(stock, obs)
