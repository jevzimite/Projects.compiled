
theme_set(theme_classic())
library(rethinking)
#### DATA ####

df = iris
df = filter(df, Species == "setosa")
df[,5]=NULL

####  QUAP  ####
x =df$Sepal.Width
xbar = mean(df$Sepal.Width)
lm=lm(Sepal.Length ~ Sepal.Width, data = df)
aLM = lm$coefficients[1]
bLM = lm$coefficients[2]

quap <-quap(
  alist(
    Sepal.Length ~ dnorm(mu,sigma),
    mu <-a+(b)*(Sepal.Width-mean(Sepal.Width)),
    a ~dnorm(aLM, .4),
    b ~dnorm(bLM, .4),
    sigma ~dnorm(sd(Sepal.Length),.4)
  ) ,data=df)

post <-extract.samples(quap)
a_map <-mean(post$a)
b_map <-mean(post$b)

plot(x =df$Sepal.Width, y = df$Sepal.Length, xlim=(c(min(df$Sepal.Width),max(df$Sepal.Width))), ylim=c(min(df$Sepal.Length),max(df$Sepal.Length)))
for (i in 1:100)
  curve( post$a[i]+post$b[i]*(x-mean(df$Sepal.Width)),
         col=col.alpha("black",0.5) ,add=TRUE)
curve( a_map+(b_map*(x-xbar)),add=TRUE, col = 2, type="l")

glm(Sepal.Length ~ ., data = df)

X