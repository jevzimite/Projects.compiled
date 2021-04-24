library(quantmod)
library(caret)

stock <- "AMZN" #symbol used in yahoo finances
tmp <- getQuote(stock)
stock <- getSymbols(stock,auto.assign = FALSE)
df = stock
# df <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))

df = as.data.frame(df)
df = na.omit(df)
dim(df)

value = (df[,2] + df[,3])/2
value = (value) #normalize?
df = cbind(df,value)
colnames(df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Value") 


finding.time = function(time){
####note: might have to adjust the delayed values by -1
train = df[ (nrow(df) - time):nrow(df) , ]
train$value.30.days.ago = df[((nrow(df):nrow(df) - time))- 29 ,7 ]
train$value.14.days.ago = df[((nrow(df):nrow(df) - time)) - 13 ,7 ]
train$value.7.days.ago = df[((nrow(df):nrow(df) - time))- 6 ,7 ]
train$value.3.days.ago = df[((nrow(df):nrow(df) - time))- 2 ,7 ]
train$value.1.days.ago = df[((nrow(df):nrow(df) - time)),7 ]
train$SHBY = train$value.1.days.ago < train$Value
train$SHBY = train$Close > train$Open
train$SHBY = as.factor(train$SHBY)

train$Volume.30.days.ago = df[((nrow(df):nrow(df) - time))- 29 ,5 ]
train$Volume.14.days.ago = df[((nrow(df):nrow(df) - time)) - 13 ,5 ]
train$Volume.7.days.ago = df[((nrow(df):nrow(df) - time))- 6 ,5 ]
train$Volume.3.days.ago = df[((nrow(df):nrow(df) - time))- 2 ,5 ]
train$Volume.1.days.ago = df[((nrow(df):nrow(df) - time)) ,5 ]

####  ERROR  ####
# fun = function(i){
#   m = randomForest::randomForest(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago,
#     data = train, mtry = i, ntree = 50)
#   return(mean(m$err.rate[,1]))
#   }
# err = replicate(100, {
# right = sapply(1:6, fun)})
# matplot(err, type = 'l', lty = 1, col = 'red', lwd = .25)
#### 

ind = sample(1:nrow(train), nrow(train)/2)
train2 = train[ind,]
test = train[-ind,]
m = randomForest::randomForest(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago,
                               data = train2, mtry = 4, ntree = 10000)
# m = train(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago,
#                             data = train2,  method = 'svmLinear')

pred = predict(m, test)
ans = (pred == test$SHBY)
length(ans[ans == T])/length(ans)
return(length(ans[ans == T])/length(ans))
}

#### FIND THE RIGHT TRAINING SIZE ####
x = seq(10, 360, 20)
a = sapply(x,finding.time)
plot((a), type = 'l')
lines(smooth(a), type = 'l', col = 'blue' , lty = 2, lwd = 2)
x[5:15] ### check for highest %

training = function(time){
  
  train = df[ (nrow(df) - time):nrow(df) , ]
  train$value.30.days.ago = df[((nrow(df):nrow(df) - time))- 29 ,7 ]
  train$value.14.days.ago = df[((nrow(df):nrow(df) - time)) - 13 ,7 ]
  train$value.7.days.ago = df[((nrow(df):nrow(df) - time))- 6 ,7 ]
  train$value.3.days.ago = df[((nrow(df):nrow(df) - time))- 2 ,7 ]
  train$value.1.days.ago = df[((nrow(df):nrow(df) - time)),7 ]
  train$SHBY = train$value.1.days.ago < train$Value
  train$SHBY = train$Close > train$Open
  train$SHBY = as.factor(train$SHBY)
  
  train$Volume.30.days.ago = df[((nrow(df):nrow(df) - time))- 29 ,5 ]
  train$Volume.14.days.ago = df[((nrow(df):nrow(df) - time)) - 13 ,5 ]
  train$Volume.7.days.ago = df[((nrow(df):nrow(df) - time))- 6 ,5 ]
  train$Volume.3.days.ago = df[((nrow(df):nrow(df) - time))- 2 ,5 ]
  train$Volume.1.days.ago = df[((nrow(df):nrow(df) - time)) ,5 ]
  return(train)}
train=training(70)

finalModel = randomForest::randomForest(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago,
                               data = train, mtry = 4, ntree = 10000)
# finalModel = train(SHBY ~ Open + value.1.days.ago + value.3.days.ago + value.7.days.ago + value.14.days.ago + value.30.days.ago + Volume.1.days.ago+ Volume.3.days.ago + Volume.7.days.ago + Volume.14.days.ago + Volume.30.days.ago,
#           data = train,  method = 'cforest')

{query =  list(Open = tmp$Open,
value.1.days.ago = df$Value[length(df$Value)],
value.3.days.ago = df$Value[length(df$Value) - 2],
value.7.days.ago = df$Value[length(df$Value) - 6],
value.14.days.ago = df$Value[length(df$Value) - 13],
value.30.days.ago = df$Value[length(df$Value) - 29],
Volume.1.days.ago = df$Volume[length(df$Volume)],
Volume.3.days.ago = df$Volume[length(df$Volume) - 2],
Volume.7.days.ago = df$Volume[length(df$Volume) - 6],
Volume.14.days.ago =df$Volume[length(df$Volume) - 13],
Volume.30.days.ago = df$Volume[length(df$Volume) - 29])}

x[1:5]

predict(finalModel, query)
