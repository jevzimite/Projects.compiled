library("e1071")

svm_model <- svm(Sepal.Length~ ., data=iris, kernel="radial") #linear/polynomial/sigmoid
# plot(svm_model, data=iris,
#      Petal.Width~Petal.Length,
#      slice = list(Sepal.Width=2, Sepal.Length=4)
# )

points(pred, pch = 20)
error = pred - iris$Sepal.Length
Sepal.Length = seq(from = min(iris$Sepal.Length), to = max(iris$Sepal.Length), length.out = 150)
pred <- predict(svm_model, iris)
plot(round(iris$Sepal.Length,0))
points(round(pred,0), type = "p", col = 2, cex = 1.5)


m = klaR::rda(Species ~ ., data = df)

# df = iris
# df$Speci(es = as.integer(df$Species)
# df$Species = as.factor(df$Species)
# library(klaR)
klaR::partimat(Species ~ ., data = df, method = "rda", type = "p", cex = 0)
# nB <- naiveBayes(Species ~ ., data = df)
# plot(nB)