#### library ####
# library(scatterplot3d)
library(bnlearn)
library(lattice)
library(rethinking)
library(gRain)
library(Rgraphviz)
# library(nlme)
# library(mgcv) 
library(plot3D)
#### data ####
df = iris
# head(df)

dfN = df
dfN$Species = NULL
for (i in 1:ncol(dfN))
  dfN[,i]=scale(dfN[,i])
# round(cor(dfN),2)

#### org cloud ####
cloud(
  Sepal.Length ~ Petal.Length * Petal.Width,
  df,
  groups = Species,
  par.settings = list(superpose.symbol = list(
    pch = 16, cex = 4,
    col = c(col.alpha(2, .2), col.alpha(3, .3), col.alpha(4, .45)
    ))),
  main = "Original"
)

#### regression plane 1 ####
x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Petal.Width

fit <- lm(z ~ x + y)
grid.lines = 25
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

scatter3D(x,y,z,
          pch = 20,  theta = 0, phi = 45,
          type = "h",
          cex = 1,
          main = "Iris data (original)",
          alpha = .6,
          xlab = "Sepal width",
          ylab ="Petal length",
          zlab = "Sepal length",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints)
)


#### bayes net ####
hc = hc(df)
# modelstring(hc) = "[Species][Petal.Length|Species][Petal.Width|Petal.Length:Species][Sepal.Width|Petal.Width:Species][Sepal.Length|Species:Sepal.Width:Petal.Length:Petal.Width]"

fit.hc = bn.fit(hc, df)
graphviz.plot(fit.hc, highlight = list(
  nodes = c("Sepal.Length", "Petal.Width", "Petal.Length"),
  fill = "orange",
  col = "1"),
  main = "Bayesian network (using hill-climbing)",
  )

#### synth cloud ####
dfS = rbn(fit.hc, 1e4)
cloud(
  Sepal.Length ~ Petal.Length * Petal.Width,
  dfS,
  groups = Species,
  par.settings = list(superpose.symbol = list(
    pch = 16, cex = 4,
    col = c(col.alpha(2, .02), col.alpha(3, .03), col.alpha(4, .045)
    ))),
  main = "Synthetic"
)

#### regression plane 2 ####
x <- sep.l <- dfS$Sepal.Length
y <- pet.l <- dfS$Petal.Length
z <- sep.w <- dfS$Petal.Width

fit <- lm(z ~ x + y)
grid.lines = 25
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

scatter3D(x,y,z,
          pch = 20,  theta = 0, phi = 45,
          type = "p",
          cex = 1,
          main = "hours",
          alpha = .5,
          xlab = "p length",
          ylab ="p width",
          zlab = "s length",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints)
)


##### quap (for lm) ####
lm = lm(Sepal.Length ~ Petal.Length * Petal.Width, dfS)
# lm = gam(Sepal.Length ~ s(Petal.Length) + s(Petal.Width) + Species, data = dfS)
pred = predict(lm , df)

x = pred
xbar = mean(x)
lm2=lm(df$Sepal.Length ~ pred)
aLM = lm2$coefficients[1]
bLM = lm2$coefficients[2]

quap <-quap(
  alist(
    Sepal.Length ~ dnorm(mu,sigma),
    mu <-a+(b)*(pred-mean(pred)),
    a ~dnorm(aLM, .4),
    b ~dnorm(bLM, .4),
    sigma ~dnorm(sd(Sepal.Length),.4)
  ) ,data=df)

post <-extract.samples(quap)
a_map <-mean(post$a)
b_map <-mean(post$b)

plot(
  x = pred,
  y = df$Sepal.Length,
  pch = 16,
  col = col.alpha(1,.65),
  cex = 1.5,
  main = "Assesment of lm",
  xlab = "Prediction",
  ylab = "Theorical"
)
for (i in 1:50)
  curve( post$a[i]+post$b[i]*(x-mean(pred)),
         col=col.alpha("steelblue",0.15) ,add=TRUE)
curve( a_map+(b_map*(x-xbar)),add=TRUE, col = "darkblue", type="l")

#### r^2 list ####
r.list =replicate(
  1e4,
  {
    dfS = rbn(fit.hc, 1e4)
    lm = lm(Sepal.Length ~ Petal.Length * Petal.Width , data = dfS)
    # lm = gam(Sepal.Length ~ s(Petal.Length) + s(Petal.Width) + Species, data = dfS)
    pred = predict(lm , df)
    r = cor(pred, df$Sepal.Length)
    return(r)
  }
)

r2.list = r.list^2
plot(density(r2.list),
     main = "Distribution of r^2 when using lm",
     col = 2)

quantile(r2.list)
precis(r2.list)

