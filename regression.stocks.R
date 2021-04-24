#### LIBRARY, INPUTS & PLOT SETTINGS ####
library(quantmod)

stock <- "VLNS.TO" 
K = 35 #training size

par(mfrow = c(2,2))

#### FUNCTIONS ####
tan.slope = function(x,y){
  spl <- smooth.spline(y ~ x, spar = .0001)
  
  seq = seq(1,length(y), 1)
  list = c(rep(NA, length(seq)))
  count = c(0)
  for ( i in seq){
    count = count + 1
    newx <- i
    pred0 <- predict(spl, x=newx, deriv=0)
    pred1 <- predict(spl, x=newx, deriv=1)
    yint <- pred0$y - (pred1$y*newx)
    xint <- -yint/pred1$y
    list[count] = pred1$y
  }
  return(list)}

training = function(time){
  
  train = df[ (nrow(df) - time):nrow(df) , ]
  train$value.30.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 30) ,7 ]
  train$value.14.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 14) ,7 ]
  train$value.7.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 7) ,7 ]
  train$value.3.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 3) ,7 ]
  train$value.1.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 1) ,7 ]
  train$SHBY = train$Close - train$Open
  
  
  train$Volume.30.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 30) ,5 ]
  train$Volume.14.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 14) ,5 ]
  train$Volume.7.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 7) ,5 ]
  train$Volume.3.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 3) ,5 ]
  train$Volume.1.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 1) ,5 ]
  return(train)}

finding.time = function(time){
  train = df[ (nrow(df) - time):nrow(df) , ]
  train$value.30.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 30) ,7 ]
  train$value.14.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 14) ,7 ]
  train$value.7.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 7) ,7 ]
  train$value.3.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 3) ,7 ]
  train$value.1.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 1) ,7 ]
  train$SHBY = train$Close - train$Open
  
  
  train$Volume.30.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 30) ,5 ]
  train$Volume.14.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 14) ,5 ]
  train$Volume.7.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 7) ,5 ]
  train$Volume.3.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 3) ,5 ]
  train$Volume.1.days.ago = df[ (  (nrow(df) - time): nrow(df)  - 1) ,5 ]
  
  ind = sample(1:nrow(train), nrow(train)/2)
  train2 = train[ind,]
  test = train[-ind,]
  m = randomForest::randomForest(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago + slope,
                                 data = train2, mtry = 4, ntree = 800)
  
  pred = predict(m, test)
  ans = (pred - test$SHBY)
  length(ans[ans == T])/length(ans)
  return(mean(ans))
}

#### DATA WRANGLING ####

label = stock
tmp <- getQuote(stock)
stock <- getSymbols(stock,auto.assign = FALSE)
df = stock

df = as.data.frame(df)
df = na.omit(df)

value = (df[,2] + df[,3])/2
df = cbind(df,value)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value") 

df$slope = tan.slope(x = 1:length(df$Value) , y = df$Value)

#### FIND THE RIGHT TRAINING SIZE ####
x = round(seq(15, 150, length.out = 15))
a = sapply(x,finding.time)
plot( a, cex = 0, ylim = c(-.15,.15),
      xlab = 'Days used for training', ylab = "Error rate",
      main = 'Finding the right training size')

null =replicate(15, {
  a = sapply(x,finding.time)
  lines( (a), type = 'l', col = 'red' , lty = 1, lwd = .33)
  return(a)
})

lines(smooth.spline(a), type = 'l', col = 'blue' , lty = 2, lwd = 2)

#### FINAL MODEL ####
train=training(K)

finalModel = randomForest::randomForest(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago + slope,
                                        data = train, mtry = 4, ntree = 4000)

{query =  list(Open = tmp$Last,
               value.1.days.ago = df$Value[length(df$Value)],
               value.3.days.ago = df$Value[length(df$Value) - 2],
               value.7.days.ago = df$Value[length(df$Value) - 6],
               value.14.days.ago = df$Value[length(df$Value) - 13],
               value.30.days.ago = df$Value[length(df$Value) - 29],
               Volume.1.days.ago = df$Volume[length(df$Volume)],
               Volume.3.days.ago = df$Volume[length(df$Volume) - 2],
               Volume.7.days.ago = df$Volume[length(df$Volume) - 6],
               Volume.14.days.ago =df$Volume[length(df$Volume) - 13],
               Volume.30.days.ago = df$Volume[length(df$Volume) - 29],
               slope = df$slope[length(df$slope)] )}

predd = round(predict(finalModel, query),3)
mean = predict(finalModel, query) + tmp$Last #prediction
sd = ( (max(null)+predd) - (min(null)+predd))/4
r = round(cor(predict(finalModel, train),train$SHBY)**1,3)
dist = rnorm(1e5, mean, sd)

plot(density(dist), main = paste('Prediction =', round(mean, 4), ', r =', r, ', change =', predd) )
lines(c(mean,mean), c(-100,100), col = 2, lty = 2)

#### BASIC PLOT ####
plot(y = c(train$Close, tmp$Last), x = 1:(length(train$Close)+1), type = 'l',
     main = paste(label), xlab = 'Time', ylab = 'Value')
lines(x = c(-1, 999), y = c(tmp$Last, tmp$Last) ,
      col = 2, lty = 2)

# length(dist[dist>mean + sd*.66])/1e5
# mean + sd*.66

#### SLOPE ANALYSIS ####
plot(density(df$slope), main = paste('Change of rate over the last', K, 'days'))
lines(x  = c(0,0), y = c(-999, 999), lty = 2)


