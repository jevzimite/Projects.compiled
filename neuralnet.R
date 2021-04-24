library(neuralnet)
df = iris


set.seed(3)
bn = hc(iris[,1:4])
net = bn.fit(bn, iris[,1:4])
dfS = rbn(net, 300)
nn=neuralnet(Sepal.Length~Petal.Length+Petal.Width,data=dfS, hidden=c(5),act.fct = 'logistic',
             linear.output = T, )
plot(nn)

pred = predict(nn, df)
plot(
  x = pred,
  y = y,
  type = "p",
  pch = 20,
  col = 4,
)
points(x=y, y=y,
      type = "l",
      cex = 1,
      pch = 8)

pred = round(pred,1)
cor(pred,y)

df = data.frame(pred, iris$Sepal.Length)
diff = df[,1]-df[,2]
df = data.frame(pred, iris$Sepal.Length)

