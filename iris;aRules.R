library(arules) 
library(arulesViz) 

# df = iris
# df$Sepal.Length[df$Sepal.Length>mean(df$Sepal.Length)] = "large"
# df$Sepal.Length[df$Sepal.Length != "large"] = "small"
# df$Sepal.Width[df$Sepal.Width>mean(df$Sepal.Width)] = "large"
# df$Sepal.Width[df$Sepal.Width != "large"] = "small"
# df$Petal.Length[df$Petal.Length>mean(df$Petal.Length)] = "large"
# df$Petal.Length[df$Petal.Length != "large"] = "small"
# df$Petal.Width[df$Petal.Width>mean(df$Petal.Width)] = "large"
# df$Petal.Width[df$Petal.Width != "large"] = "small"
# 
# for (i in 1:5)
#   df[,i]= as.factor(df[,i])
# 
# sapply(df, class)

rules = apriori(Groceries)
ruleExplorer(rules)
inspect(rules[1:100])


arulesViz::inspectDT(rules[1:10])
