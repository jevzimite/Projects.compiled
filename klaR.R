indx = sample(1:nrow(iris), 75)
# train = iris[indx,]
# test = iris[-indx,]
set.seed(0)
dfN = iris[,1:4]
for(i in 1:4)
  dfN[,i]= scale(as.numeric(dfN[,i]))

clusters = kmeans(dfN, 3)
clusters = clusters$cluster
# clusters[clusters == 2] = "setosa"
# clusters[clusters == 1] = "virginica"
# clusters[clusters == 3] = "versicolor"
res = (clusters) == iris$Species
length(res[res == 1])/150 

df = data.frame(cbind(iris[,1:4], clusters))
df$clusters = as.factor(df$clusters)
klaR::partimat(clusters ~ ., data = df, method = "lda")
