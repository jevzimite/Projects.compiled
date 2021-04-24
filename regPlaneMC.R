#### data ####
data("Primates301")
df = Primates301

df = df[,c(2,3,8,9,10)]
cc = complete.cases(df)
df = df[cc,]
sapply(df, class)
df$research_effort = as.factor(df$research_effort)

##### bayes net ####
bn = hc(df)
modelstring(bn) = "[genus][species|genus][research_effort|species][body|genus:species][brain|genus:body:species]"
net = bn.fit(x = bn, data = df)
graphviz.plot(net)
dfS = rbn(net, 100)
df = dfS

# dfN = df
# dfN$genus = NULL
# dfN$species = NULL

# dfN[,1] = as.numeric(dfN[,1])
# dfN[,2] = as.numeric(dfN[,2])
# dfN[,3] = as.numeric(dfN[,3])

# k = kmeans(dfN, 4)
# fviz_cluster(k, dfN)

df$research_effort = as.numeric(df$research_effort)
df$brain = as.numeric(df$brain)
df$research_effort = as.numeric(df$research_effort)
# cor(df$research_effort, df$brain)
# cor(df$research_effort, df$body)
# cor(df$brain, df$body)

x = df$body
y = df$brain
z = df$research_effort
w = as.integer(df$species)
# w = k$cluster
# w = w+3

x = scale(x)
y = scale(y)
z = scale(z)

##################### monte carlo run ##################### 
library("circlize")
rgl.clear()
#### fun ####
replicate(n = 10, 
          {
dfS = rbn(net, 150)
df = dfS
df$research_effort = as.numeric(df$research_effort)
df$brain = as.numeric(df$brain)
df$research_effort = as.numeric(df$research_effort)
x = df$body
y = df$brain
z = df$research_effort
x = scale(x)
y = scale(y)
z = scale(z)
# x = as.numeric(x)
# y = as.numeric(y)
# z = as.numeric(z)
ellips <- ellipse3d(cov(cbind(x,y,z)),
                    centre=c(mean(x), mean(y), mean(z)), level = 0.99)
fit <- gam(y ~ s(x) + s(z))
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)


rgl.spheres(x,y,z, r = .1,
            alpha = 1,
            color = rand_color(1))
# wire3d(ellips, col =  "blue",  lit = FALSE, alpha = .05)
rgl.surface(x.pred, z.pred, y.pred, color = rainbow(1),
            alpha = .1, lit = FALSE)
rgl.surface(x.pred, z.pred, y.pred, color = "black",
            alpha = 0.1, lit = FALSE, front = "lines", back = "lines")
          }
)
#### texts n box ####
rgl.texts(c(min(x), max(x)), c(0, 0), c(0, 0), "X AXIS", color = "blue")
rgl.texts(c(0, 0), c(min(y),max(y)), c(0, 0), "Y AXIS", color = "blue")
rgl.texts(c(0, 0), c(0, 0), c(min(z),max(z)), "Z AXIS", color = "green")
rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = 1)
rgl.lines(c(0, 0), c(min(y),max(y)), c(0, 0), color = 1)
rgl.lines(c(0, 0), c(0, 0), c(min(z),max(z)), color = 3)
rgl.bbox(color = "white")



