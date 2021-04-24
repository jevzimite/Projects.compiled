library(quantmod)
STOCKNAMENAME = "GLXY.TO"

##### functions ####
logisticMC = function(i){
n=i
#####
STOCKNAME <- "NOU.V" #symbol used in yahoo finances
tmp <- getQuote(STOCKNAME)
STOCKNAME <- getSymbols(STOCKNAME,auto.assign = FALSE)
df <- rbind(STOCKNAME, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
df = as.data.frame(df)
df = na.omit(df)
value = (df[,2] + df[,3])/2
value = (value) #normalize?
df = cbind(df,value)
df2 = df
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value")
df2 = df
df = df[(nrow(df)-n):nrow(df) ,]
value = df$Value
#####
growthR = (value[length(value)]-value[1])/value[1]
currentRelVal = (value[length(value)]/max(df2$value))
ans = growthR+currentRelVal *(1-currentRelVal)
str = ans
list = replicate(5, {
ans = growthR + (ans)*(1-ans)
return(ans)
})
list = c(str, list)




fun = function(growthR){
    currentRelVal = (value[length(value)]/max(df2$value))
    ans = growthR+currentRelVal *(1-currentRelVal)
    str = ans
    list = replicate(5, {
      ans = growthR + (ans)*(1-ans)
      return(ans)
    })
    list = c(str, list)
    lines(list,
         type = "l",
         col = "darkblue",
         lwd = .1)
    lines(
      x = c(0,1),
      y = c(currentRelVal,list[1]),
      type = "l",
      col = "darkblue",
      lwd = .1)
    
}

f= growthR-1
t= growthR+1
x = seq(from=f, to=t, length.out = 100)
plot(x = c(0, 3), y = c(-5, 5), cex =0,
     main = "Simulation",
     xlab = "Days",
     ylab = "Size")
sapply(x, fun)
lines(list,
      col = 2,
      type = "l")
lines(x = c(0,0),
      y = c(-10,10),
      lty = "dashed")
lines(
  x = c(0,1),
  y = c(currentRelVal,list[1]),
  type = "l",
  col = 2,
  lwd = 1)

}
logistitcMC2 = function(f2,t2){
  helper = function(i){
    n=i
    #####
    STOCKNAME <- STOCKNAMENAME #symbol used in yahoo finances
    tmp <- getQuote(STOCKNAME)
    STOCKNAME <- getSymbols(STOCKNAME,auto.assign = FALSE)
    df <- rbind(STOCKNAME, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
    df = as.data.frame(df)
    df = na.omit(df)
    value = (df[,2] + df[,3])/2
    value = (value) #normalize?
    df = cbind(df,value)
    colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value")
    df2 = df
    df = df[(nrow(df)-n):nrow(df) ,]
    value = df$Value
    #####
    growthR = (value[length(value)]-value[1])/value[1]
    currentRelVal = (value[length(value)]/max(df2$value))
    ans = growthR+currentRelVal *(1-currentRelVal)
    str = ans
    list = replicate(5, {
      ans = growthR + (ans)*(1-ans)
      return(ans)
    })
    list = c(str, list)
    
    
    
    
    fun = function(growthR){
      currentRelVal = (value[length(value)]/max(df2$value))
      ans = growthR+currentRelVal *(1-currentRelVal)
      str = ans
      list = replicate(5, {
        ans = growthR + (ans)*(1-ans)
        return(ans)
      })
      list = c(str, list)
      lines(list,
            type = "l",
            col = "darkblue",
            lwd = .1)
      lines(
        x = c(0,1),
        y = c(currentRelVal,list[1]),
        type = "l",
        col = "darkblue",
        lwd = .1)
      
    }
    
    f= growthR-.5
    t= growthR+.5
    x = seq(from=f, to=t, length.out = 15)
    sapply(x, fun)
    lines(list,
          col = 2,
          type = "l")
    lines(x = c(0,0),
          y = c(-10,10),
          lty = "dashed")
    lines(
      x = c(0,1),
      y = c(currentRelVal,list[1]),
      type = "l",
      col = 2,
      lwd = 1)
    
  }
  par(mfrow=c(1,1))
  x = round(seq(from = f2, to = t2, length.out = t2-f2),0)
  plot(x = c(0, 3), y = c(-5, 5), cex =0,
       main = "Simulation",
       xlab = "Days",
       ylab = "Size")
  sapply(x, helper) 
}
##### output #####
par(mfrow=c(4,4))
x = round(seq(from = 37, to = 50, length.out = 16),0)
sapply(x, logisticMC) ##16 different `n` days with 50 different rates

par(mfrow=c(1,1))
logisticMC(1) ### 50 growth lines over `n` days

logistitcMC2(f2 = 1, t2 = 50)

t(matrix(x, ncol = 4))

matplot(matrix(x, ncol = 4), type = "b", col = rainbow(8))
