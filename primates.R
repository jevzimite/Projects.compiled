#####
library(truncnorm)

data("Primates301")
d <-Primates301
d2 <-d[complete.cases(d$brain)&complete.cases(d$body),]
d2[,1:8]=NULL
d3<-d2[complete.cases(d2),]
d2<-d2[complete.cases(d2),]
sapply(d3, class)

d4 = d3
dfN = normalize(d3)
corplot = cor(dfN) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(dfN)$p,
           sig.level = 0.05)
d3 = d3[, c(1,2,7,8)]

gam = gam(brain ~ s(body), data = d3)
plot.gam(gam)
points(x = d3$body, y = d3$brain, type = "p",
       pch = 8,
       col = 4,)

df = d4 ### undo the normalizing

k = kmeans(dfN$brain, 3)
df = cbind(df, k$cluster)
c1=filter(df, k$cluster == 1)
c2=filter(df, k$cluster == 2)
c3=filter(df, k$cluster == 3)
fviz_cluster(k, d3)

r = cor(df$brain, df$`k$cluster`)
r^2
df$`k$cluster` = as.factor(df$`k$cluster` )

rF=randomForest(df$`k$cluster`~ df$brain, data = df, ntree= 100, mtry = 1)
pred =predict(rF, df)
table(pred,df$`k$cluster`)

ggplot()+
  geom_density(mapping=aes(c1$brain), fill = 2, alpha = .7)+
  geom_density(mapping=aes(c2$brain), fill = 3, alpha = .7)+
  geom_density(mapping=aes(c3$brain), fill = 4, alpha = .7)

y = c(rtruncnorm(810, a=0, b = max(c1$brain), mean(c1$brain), sd(c1$brain)) , rnorm(40, c2$brain, sd = c2$brain) , rnorm(150, c3$brain, sd = c3$brain) )
b = c(rtruncnorm(810, a=0, b = max(c1$body), mean(c1$body), sd(c1$body)) , rnorm(40, c2$body, sd = c2$body) , rnorm(150, c3$body, sd = c3$body) )
y = y[y>0]
b = b[b>0]
y = y[1:900]
b = b[1:900]


reg = data.frame(y,b)
gam=gam(y~s(b), data = reg)

plot.gam(gam)
points(x = df$body, y=df$brain)


#####
