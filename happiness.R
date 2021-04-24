library(readxl)
library(dplyr)
library(igraph)
library(tidygraph)
library(ggraph)
library(car)
library(psych)
library(mclust)

before <-rnorm(20, 10, 3)
after <-before + truncnorm::rtruncnorm(20, -1, 10, 0, 3)
t.test(before, mu = 11, alternative = 'less')


#### data wrangling ####
happiness <- read_excel("Desktop/R/xlsm/happiness.xlsm")

df = data.frame(happiness)
df = na.omit(df)
# df = filter(df, Country.name == 'Mexico')

df$Country.name = as.factor(df$Country.name)

for (i in 2:ncol(df))
  df[,i] = as.numeric(df[,i])
numeric = df[2:ncol(df)]

#### graph 1 ####
cor = round(cor(numeric),2)
width = abs(round(cor(numeric),3))
len.width = ncol(width)
g1 = matrix(rep(1, len.width**2) , ncol = len.width)
g = igraph::graph_from_adjacency_matrix(g1)
ev = sna::evcent(width)
betw = sna::betweenness(round(width,0))

ggraph(graph = g, 'fr')+
  geom_edge_link(aes(width = c(width)**2))+
  scale_edge_width(range = c(0,.5))+
  geom_node_point(size = ev**2*20)+
  geom_node_text(label = colnames(numeric),repel = 1, col = 2)+
  labs(title = "r**2 on all variables")

barplot(ev, names.arg = colnames(numeric), las = 2,
        main = 'Assosiation over other variables',
        col = rainbow(10, start = .01, end =.1), density = 70)

rethinking::precis(df)
cbind(as.data.frame(cor)$Life.Ladder, colnames(numeric))

#### graph 2 ####
df2 = data.frame(df$Life.Ladder,
df$Log.GDP.per.capita,
df$Social.support,
df$Freedom.to.make.life.choices,
df$Perceptions.of.corruption,
df$Positive.affect)


cor2 = abs( round(cor(df2),2))
g2 = graph_from_adjacency_matrix(matrix(rep(1, ncol(df2)**2), ncol = ncol(df2)))
ggraph(g2, 'fr')+
  geom_edge_link(aes(width = c(cor2)**4))+
  scale_edge_width(range = c(0,.75))+
  geom_node_point(size = 3)+
  geom_node_text(label = colnames(df2), repel = 1, col = 2)+
  labs(title = "r**2 on variables with relative high r values")

#### lm ####
df3 = data.frame(df$Life.Ladder,
                 df$Perceptions.of.corruption,
                 df$Positive.affect,
                 df$Log.GDP.per.capita)

gam = mgcv::gam(Life.Ladder ~ s(Perceptions.of.corruption) + Positive.affect + Log.GDP.per.capita + Country.name, data = df)
pred = predict(gam, newdata = list(
  Perceptions.of.corruption = df$Perceptions.of.corruption,
  Positive.affect = df$Positive.affect,
  Log.GDP.per.capita = df$Log.GDP.per.capita,
  Country.name = df$Country.name
))

plot(df$Life.Ladder, pred, col = as.integer(df$Country.name), main = 'Testing of lm', xlab = 'Target', ylab = 'Prediction')
cor(df$Life.Ladder, pred)**2




#### pca & kmeans clustering  ####
pca = prcomp(numeric[,2:ncol(numeric)])
gmM = Mclust(numeric[,2:ncol(numeric)], G = 1:3)
gmmC = gmM$classification

x = scale(pca$x[,1])
y = scale(pca$x[,2])
z = scale(pca$x[,3])

gmM = Mclust(numeric$Life.Ladder, G = 1:3)
gmmC = gmM$classification

plot(
  pca$x[,1],pca$x[,2], cex = ((df$Life.Ladder**2))/20, pch = 20,
  col = scales::alpha(gmmC  + 4,.5),
  xlab = 'PC1', ylab = 'PC2', main = 'Clustering by GMM'
)

classified = data.frame(cbind(df,gmmC))
c1 = filter(classified, gmmC == 1)
c2 = filter(classified, gmmC == 2)
c3 = filter(classified, gmmC == 3)

fn = function(mean0,mean1){
  sd = sd(classified$Life.Ladder)
  return((mean0-mean1) / sd)
}

means = c(mean(c1$Life.Ladder),mean(c2$Life.Ladder),mean(c3$Life.Ladder))
sds = c(sd(c1$Life.Ladder),sd(c2$Life.Ladder),sd(c3$Life.Ladder))
effSz.vs.2 = effectsize::cohens_f(lm(classified$Life.Ladder ~ as.factor(classified$gmmC)))$Cohens_f

i = filter(classified, gmmC == 3 & year >=2000)$Country.name
plot(as.factor(as.character(i)), las = 2, density = 90, col = 4:6,
     main = "Counts of being classified as cluster 3 since year 2000")

#### FACTOR ANALYSIS ####
f = factanal(numeric, 2)

cols = round(f$uniquenesses*100)+1
cf = colorRampPalette(c(3,7,2))
cf = cf(max(cols)+1)[c(cols)]
crit = 0.7

par(mfrow = c(2,1))
f1 <- f$loadings[,1]
f1 <- data.frame(f1,1) ## 1 is your "height"
plot(f1, type = 'o', pch = '   ', ylab = '', xlab = 'Factor1')
lines( x = c(crit,crit), y = c(-10,10), lty = 2)
text(f1, labels=names(numeric) , srt = 90, col = 2, font = 2)


f2 <- f$loadings[,2]
f2 <- data.frame(f2,1) ## 1 is your "height"
plot(f2, type = 'o', pch = '    ', ylab = '', xlab = 'Factor2')
lines( x = c(crit,crit), y = c(-10,10), lty = 2)
text(f2, labels=names(numeric) , srt = 90, col = 2, font = 2)


par(mfrow = c(1,1))
plot(x = c(0,0), y = c(-10,10), type = 'l',col = 2, lty = 1,
     xlim = c(-.8,1.5), ylim = c(-0.5,1), main = "Factor Analysis")
lines(y = c(0,0), x = c(-10,10), col = 2, lty = 1)
lines( x = c(crit,crit), y = c(-10,10), lty = 2)
lines( x = c(-10,10), y = c(crit,crit), lty = 2)
points(f$loadings[,1:2], cex = 1/f$uniquenesses**.5 +1, pch = 20,  col = cf)
text(f$loadings[,1:2], labels=names(numeric), cex=0.75, col = 'blue', font = 2)


#### random forest ####
ind = sample(1:nrow(df), nrow(df)/2)
train = df[ind,]
train$Country.name = droplevels(train$Country.name)
test = df[-ind,]
test$Country.name = as.character(test$Country.name)

rf = function(i){
  rF = randomForest::randomForest(train$Country.name ~ ., data = train, mtry = i, ntree = 100)
  pred = as.character(predict(rF, test))
  res = pred == test$Country.name
  return(length(res[res == T])/length(res))
}
# error = replicate(50, {
# error = sapply(1:ncol(df), rf)})
# matplot(error, type = 'l', lty = 1, col = 'red', lwd = .5)
rF = randomForest::randomForest(train$Country.name ~ ., data = train, mtry = 3, ntree = 300)
pred = as.character(predict(rF, test))
res = pred == test$Country.name
length(res[res == T])/length(res)
table = table(pred, pred == test$Country.name)

mistk = table[,1]
barplot(mistk[mistk > 6 ], las = 2, main = "Top mistakes in classification",
        col = rainbow(10, start = .01, end = .1), density = 80)

crrt = table[,2]
barplot(crrt[crrt > 6 ], las = 2, main = "Top classifications",
        col = rainbow(40, start = .31, end = .47), density = 80)
filter(df, Country.name == 'Chile')
# reprtree:::plot.getTree(rF)


#### LDA ####
xx = filter(df, Country.name == 'Mexico' | Country.name == 'United States' |Country.name == 'Canada')
klaR::partimat(Country.name ~ Social.support + Positive.affect, xx, method = "lda")
  
mean(df$Life.Ladder)
