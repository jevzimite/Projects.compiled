stock1= "IBIO"
stock2= "AC"
stock3= "PFE"

days = 30

temp1 <- getQuote(stock1)
stock1 <- getSymbols(stock1,auto.assign = FALSE)
df1 <- rbind(stock1, xts(cbind(temp1$Open,temp1$High,temp1$Low,temp1$Last,temp1$Volume,temp1$Last), order.by = Sys.Date()))
df1 = as.data.frame(df1)
df1 = na.omit(df1)
value1 = (df1[,2] + df1[,3])/2
df1 = cbind(df1,value1)
colnames(df1) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "value1")
# value1 = value1[(length(value1)-6):length(value1)]
value1 = normalize(value1)

temp2 <- getQuote(stock2)
stock2 <- getSymbols(stock2,auto.assign = FALSE)
df2 <- rbind(stock2, xts(cbind(temp2$Open,temp2$High,temp2$Low,temp2$Last,temp2$Volume,temp1$Last), order.by = Sys.Date()))
df2 = as.data.frame(df2)
df2 = na.omit(df2)
value2 = (df2[,2] + df2[,3])/2
df2 = cbind(df2,value2)
colnames(df2) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "value2")
# value2 = value2[(length(value2)-6):length(value2)]
value2 = normalize(value2)

temp3 <- getQuote(stock3)
stock3 <- getSymbols(stock3,auto.assign = FALSE)
df3 <- rbind(stock3, xts(cbind(temp3$Open,temp3$High,temp3$Low,temp3$Last,temp3$Volume,temp1$Last), order.by = Sys.Date()))
df3 = as.data.frame(df3)
df3 = na.omit(df3)
value3 = (df3[,2] + df3[,3])/2
df3 = cbind(df3,value3)
colnames(df3) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "value3")
# value3 = value3[(length(value3)-6):length(value3)]
value3 = normalize(value3)

# temp4 <- getQuote(stock4)
# stock4 <- getSymbols(stock4,auto.assign = FALSE)
# df4 <- rbind(stock4, xts(cbind(temp4$Open,temp4$High,temp4$Low,temp4$Last,temp4$Volume,temp1$Last), order.by = Sys.Date()))
# df4 = as.data.frame(df4)
# df4 = na.omit(df4)
# value4 = (df4[,2] + df4[,3])/2
# df4 = cbind(df4,value4)
# colnames(df4) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "value4")
# # value4 = value4[(length(value4)-6):length(value4)]
# value4 = normalize(value4)


n1 = length(value1)-150
nr1 = length(value1)
value1 = value1[n1:nr1]
n2 = length(value2)-150
nr2 = length(value2)
value2 = value2[n2:nr2]
n3 = length(value3)-30
nr3= length(value3)
value3 = value3[n3:nr3]
# n4 = length(value4)-150
# nr4= length(value4)
# value4 = value4[n4:nr4]


stock_Data=cbind(value1, value2, value3)
stock_Data = round((stock_Data * 100)+1 , 0)


############################################################

# stock_Price = as.matrix( stock_Data[ , 2:4] )
stock_Price = as.matrix( stock_Data)

mc_rep = 1000 # Number of Monte Carlo Simulations
training_days = 3 

# This function returns the first differences of a t x q matrix of data
returns = function(Y){
  len = nrow(Y)
  yDif = Y[2:len, ] / Y[1:len-1, ] - 1
}

# Get the stock Returns
stock_Returns = returns(stock_Price)

# Suppose we invest our money evenly among all three assets 
# We use today's Price 11/14/2018 to find the number of shares each stock 
# that we buy
portfolio_Weights = t(as.matrix(rep(1/ncol(stock_Returns), ncol(stock_Returns))))
print(portfolio_Weights)

# Get the Variance Covariance Matrix of stock Returns
coVarMat = cov(stock_Returns)
miu = colMeans(stock_Returns)
# Extend the vector to a matrix
Miu = matrix(rep(miu, training_days), nrow = 3)

# Initializing simulated 30 day portfolio returns
portfolio_Returns_30_m = matrix(0, training_days, mc_rep)

set.seed(200)
for (i in 1:mc_rep) {
  Z = matrix ( rnorm( dim(stock_Returns)[2] * training_days ), ncol = training_days )
  # Lower Triangular Matrix from our Choleski Factorization
  L = t( chol(coVarMat) )
  # Calculate stock returns for each day
  daily_Returns = Miu + L %*% Z  
  # Calculate portfolio returns for 30 days
  portfolio_Returns_30 = cumprod( portfolio_Weights %*% daily_Returns + 1 )
  # Add it to the monte-carlo matrix
  portfolio_Returns_30_m[,i] = portfolio_Returns_30;
}

# Visualising result
x_axis = rep(1:training_days, mc_rep)
y_axis = as.vector(portfolio_Returns_30_m-1)
plot_data = data.frame(x_axis, y_axis)
ggplot(data = plot_data, aes(x = x_axis, y = y_axis)) + geom_path(col = 'red', size = 0.1, alpha = .15) +
  xlab('Days') + ylab('Portfolio Returns') + 
  ggtitle('Simulated Portfolio Returns in 3 days')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Construct a 95% Confidential Interval for average returns
Avg_CI = quantile(portfolio_Returns_30_m[3,]-1, c(0.025, 0.975))
print(Avg_CI)