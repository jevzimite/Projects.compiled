df = iris
theme_set(theme_bw())

ind = sample(1:150, 75)
train = df[ind,]
test = df[-ind,]

# (Sepal.Length~Petal.Length + Petal.Width, df)
round(cor(train[,1:4]),1)
library(mgcv) 

lm = lm(Sepal.Length~Petal.Length + Petal.Width, df)
lm$coefficients

fun = function(pW, pT){
y = (4.1905824 + (0.5417772*1)*(pW))+(0.3195506)*(pT)  }
n = 10000
pW = rnorm(n, (mean(df$Petal.Width)), (sd(df$Petal.Width)))
pT = rnorm(n, (mean(df$Petal.Length)), sd(df$Petal.Length))
mat = mapply(fun,pW,pT)
plot(density(mat), col = "red", xlim = c(1,8))
lines(density(df$Sepal.Length))

pred2 = mapply(fun, df$Petal.Width, df$Petal.Length) 
cor(pred2, df$Sepal.Length)

pred1 =predict(lm, df)

# x = ndf[,1]
# y = ndf[,2]
# z = pred1
# 
# cloud(z~x+y)

e = (sum(abs(df$Sepal.Length-pred1)))/150
erRaw = (abs(df$Sepal.Length-pred1))
erMean = e
a1 = (pred1-e)
a2 = (pred1+e)
b1 = a2 + sd(erRaw)
b2 = a1 - sd(erRaw)
c1 = b1 + sd(erRaw)
c2 = b2 - sd(erRaw)
d1 = c1 + sd(erRaw)
d2 = c2 - sd(erRaw)
plot(pred1, pch = 20, col = scales::alpha("red", .3), cex = 0)
# points(df$Sepal.Length, pch = 20, col = scales::alpha("blue", .3), cex = .75)
lines(pred1-e, col = 3)
lines(pred1+e, col = 3)
lines(b1, col = 7)
lines(b2, col = 7)
lines(c1, col = 2)
lines(c2, col = 2)

ggplot()+
  geom_ribbon(mapping=aes(x = 1:150, ymin = d2, ymax = d1), fill = 8)+
  geom_ribbon(mapping=aes(x = 1:150, ymin = c2, ymax = c1), fill = 2)+
  geom_ribbon(mapping=aes(x = 1:150, ymin = b2, ymax = b1), fill = 7)+
  geom_ribbon(mapping=aes(x = 1:150, ymin = a2, ymax = a1),, fill = 3)+
  geom_point(mapping = aes(x = 1:150, y = iris$Sepal.Length),
             color = "black", alpha = 1, shape = 3)+
  geom_point(mapping = aes(x = 1:150, y = iris$Sepal.Length), 
             color = "black", alpha = 1, shape = 20, size = .5)+
  geom_line(mapping = aes(x=1:150, y = pred1), linetype = 7, col = "black", size = .5, alpha =5)
