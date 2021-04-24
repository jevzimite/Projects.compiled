library(bnlearn)
library(lattice)
library(rethinking)
library(gRain)
library(Rgraphviz)
library(factoextra)
library(ggbiplot)
library(heatmaply)
library(corrplot)
##### lattice graphs ####
data("Achehunting")
df = Achehunting
# df = dfS
set.seed(1)
df = df[, c(1:3,5,6,7)]
df = df[complete.cases(df),]

df$month = as.factor(df$month)
dfN = df

for (i in 1:ncol(df))
  dfN[,i] = as.numeric(dfN[,i])
# corplot = cor(dfN) %>%
#   corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
#            addCoef.col = TRUE, title = "CorPlot for initial population",
#            p.mat = cor.mtest(dfN)$p,
#            sig.level = 0.05)

# pca = prcomp(dfN)
# fviz_screeplot(pca)
k= kmeans(dfN$kg.meat, 3)

df = data.frame(df, cluster = k$cluster)
dfN = data.frame(dfN, cluster = k$cluster)
dfN$cluster = as.numeric(dfN$cluster)
df$cluster = as.factor(df$cluster)

cluster.cor = rep(NA, ncol(df))
for (i in 1:ncol(df))
  cluster.cor[i] = cor(dfN$cluster, dfN[,i])
cluster.cor = data.frame(cluster.cor)
row.names(cluster.cor)= colnames(dfN)

cluster.cor

# xyplot(kg.meat~hours|cluster,
#        groups = cluster,
#        scales = "free",
#        par.settings = list(superpose.symbol = list(
#          pch = 19, cex = 1.2,
#          col = c(col.alpha(2, .7), col.alpha(3, .7), col.alpha(4, .7)
#          ))),
#        layout = c(3,1),
#        data=df)

cloud(kg.meat ~ age*hours,
      group = cluster,
      auto.key = TRUE,
      par.settings = list(superpose.symbol = list(
         pch = 16, cex = 5,
        col = c(col.alpha(2, .3), col.alpha(3, .45), col.alpha(4, .3)
        ))),
      main = "Hunting efficiency by cluster",
      xlab = "Age",
      ylab = "Hours spent",
      zlab = "Meat gathered",
      data = df)

barchart(kg.meat ~ month|cluster,
       group = cluster,
       main = "Hunting efficiency by month",
       col = c(2,3,4),
       layout = c(1,3),
       panel = ,
       xlab = "Month",
       ylab = "Meat gathered (kg)",
       data = df)

# randomForest(month~., ntree=600, mtry = 3, data = df)

cluster.cor

####
c1=filter(df, cluster ==1)
c1$cluster = NULL
c1N = c1

for (i in 1:ncol(c1N))
  c1N[,i] = as.numeric(c1N[,i])

# pca1=prcomp(c1N)
# fviz_screeplot(pca1)
# k1=kmeans(c1N,2)
# fviz_cluster(k1, c1N)
# c1 = data.frame(c1, cluster = k1$cluster)
# c1$cluster = as.factor(c1$cluster )

# cor(df$year, df$kg.meat)
# cor(c1N$year, c1N$kg.meat)

corplot = cor(c1N) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(c1N)$p,
           title = "CorPlot for cluster 1",
           sig.level = 0.5)

c2=filter(df, cluster ==2)
c2$cluster = NULL
c3=filter(df, cluster ==3)
c3$cluster = NULL

n1 = nrow(c1)
n2 = nrow(c2)
n3 = nrow(c3)
nT = sum(n1,n2,n3)

# rate = round(seq(from =0.001, to = 1, length.out = 100),3)
# rb = rbinom(1e4, size = nT, prob = rate)

#### quap ####
quap.list = list(as.numeric(n1),as.numeric(nT))
quap <-quap(
  alist(
    n1 ~dbinom(nT,p),#binomiallikelihood
    p ~dunif(0,1)#uniformprior
  ) ,
  data=quap.list
  )

dfS.link = link(quap, df)
samp = extract.samples(quap)
qtl = quantile(samp$p)
ggplot()+
  geom_density(mapping=aes(samp$p), fill = 2, size = 0)+
  geom_vline(mapping = aes(xintercept = qtl[2]),size = 1.5, linetype = 8, alpha =.1)+
  geom_vline(mapping = aes(xintercept = qtl[4]),size = 1.5, linetype = 8, alpha =.1)+
  geom_vline(mapping = aes(xintercept = qtl[3]),size = 1.5, linetype = 8, alpha =.65)+
  labs(title = "Binomial distribution for cluster 1")+xlab("Probability")
 
#### bayes network ####
df[,c(1,7)]=NULL
for(i in 1:ncol(df))
  df[,i] = as.numeric(df[,i])

hc = hc(df)

# modelstring(hc) = "[day|hours][kg.meat|hours:age][hours][year][age|year]"
# plot(hc)

fit.hc = bn.fit(hc, df)
graphviz.plot(fit.hc,highlight = list(
  nodes = "kg.meat",
  fill = "orange",
  col = "1"),main = "Bayes network by hill-climbing")
# bn.fit.qqplot(fit.hc)

dfS = rbn(x = hc,data =df, n = 1e5)

round( (cor(dfS)-cor(df)) , 1)

nparams(hc, df)

#### cloud graph~bayes network####
dfN = dfS

for (i in 1:ncol(df))
  dfN[,i] = as.numeric(dfN[,i])
corplot = cor(dfN) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(dfN)$p,
           title = "CorPlot for modeled population",
           sig.level = 0.05)

# pca = prcomp(dfN)
# fviz_screeplot(pca)

k= kmeans(dfN$kg.meat, 3)
# fviz_cluster(k, dfS)
df = data.frame(dfS, cluster = k$cluster)
dfN = data.frame(dfN, cluster = k$cluster)
dfN$cluster = as.numeric(dfN$cluster)
df$cluster = as.factor(df$cluster)

cloud(kg.meat ~ age*hours,
      group = cluster,
      auto.key = TRUE,
      par.settings = list(superpose.symbol = list(
        pch = 16, cex = 5,
        col = c(col.alpha(2, .01), col.alpha(3, .025), col.alpha(4, .023)
        ))),
      xlab = "Age",
      ylab = "Hours spent",
      zlab = "Meat gathered",
      main = "Hunting efficiency by cluster",
      data = df)

# c1S = filter(df, cluster ==1)
# n1S = nrow(c1S)
# nTS = nrow(df)
# n1S/nTS


