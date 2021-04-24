stock <- "GLXY.TO" #symbol used in yahoo finances
obs = 90 #how many of the last entries are being used to predict new values
nAhead = 1 #how many days ahead are being predicted
f = 1
t = 30

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



# n = 10
# mean = forecast$mean
# min = mean(forecast$lower)
# max = mean(forecast$upper)
# sd = (max-min)/4``
# prior = dnorm(seq(from = min, to = max, length.out = n), mean, sd)
# plot(prior, type="b" )
# round(prior,3)


plot(a, type="b")
a
