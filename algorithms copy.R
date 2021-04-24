#### CART ####
library(rpart)
model <- rpart(Sepal.Length ~., data = iris)
par(xpd = NULL) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)

#### Expectation - maximization ####
library(mclust)
X = iris[,1:4]
# X = diabetes[,-1]
# class.d = diabetes$class
# clPairs(X, class.d)

fit <- Mclust(X)
par(mfrow = c(1,1))
plot(fit, what = "BIC")
plot(fit, what = "classification")
plot(fit, what = "uncertainty")
plot(fit, what = "density")
fit$classification

#### apriori ####
library(arules) 
library(arulesViz) 
library(RColorBrewer) 
par(mfrow = c(1,1))
# import dataset 
data("Groceries") 
# using apriori() function 
rules <- apriori(Groceries,  
                 parameter = list(supp = 0.01, conf = 0.2)) 
# using inspect() function 
inspect(rules[1:10]) 
# using itemFrequencyPlot() function 
arules::itemFrequencyPlot(Groceries, topN = 4,  
                          col = brewer.pal(8, 'Pastel1'), 
                          main = 'Relative Item Frequency Plot', 
                          type = "relative", 
                          ylab = "Item Frequency (Relative)")
inspect(rules[1:5])

arulesViz::ruleExplorer(rules)

