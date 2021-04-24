

# 1ST - EVE
# 2ND - MVY
# GLXY.TO - PERSONAL CHOICE/NOT SUSTAINED

stock <- "EVE.V" #symbol used in yahoo finances
obs = 90      -1
daysP= 7
##### DATA ####


tmp <- getQuote(stock)
stock <- getSymbols(stock,auto.assign = FALSE)
df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
df = as.data.frame(df)
df = na.omit(df)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")


a = df$Close-df$Open
prob <- function(obs){
  a = df$Close-df$Open
n= length(a)
a = a[(n-obs):n]
length(a)
pos = length(a[a>0])
neg = length(a[a<0])

p_grid <-seq(from=0,to=1,length.out=1000)
prior <-rep(1,1000)
likelihood <-dbinom(pos,size=(pos+neg),prob=p_grid)
posterior <-likelihood*prior
posterior <-posterior/sum(posterior)
samples <-sample(p_grid,size=1e4,replace=TRUE,prob=posterior)
# plot(density(posterior))
# plot(density(samples))
# hist(posterior)
return(pos/(pos+neg))
}

obs = seq(from = 1, to = obs, length.out = 30)

posterior = sapply(obs, prob)
par(mfrow=c(1,2))
# plot(density(posterior))

plot(x=1:length(posterior), y = posterior, type = "b",
     main = "Probability of profitting in 30 days",
     xlab = "Days",
     ylab = "Probability")
mean(posterior[1:daysP])
