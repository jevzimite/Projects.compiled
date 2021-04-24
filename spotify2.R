library(readxl)
library(expm)
library(ggraph)

#### data wrangling ####
spotify <- read_excel("Desktop/R/xlsm/spotify.xlsm")

df = spotify
ind = df$popularity[60 < df$popularity]
df = filter(df, df$popularity == ind)


df = na.omit(df)

df$explicit = as.factor(df$explicit)
df$year = as.integer(df$year)
df$name = NULL

factors =  c(5 , 13)
numeric = df
numeric[factors] = NULL

# abs = abs(round(cor(numeric), 0))

#### graph ####
# minor data wrangling
{
cor = abs(round(cor(numeric), 2))
abs2 = abs(round(cor(numeric), 0))
abs = matrix(rep(1, ncol(abs2)**2), ncol = ncol(abs2))

g = igraph::graph_from_adjacency_matrix(abs)
# tidygraph::as_tbl_graph(abs)
}

#graph
{
ggraph(g, 'circle')+
  # geom_edge_hive(aes(width = c(cor)))+
  geom_edge_link(aes(width = c(cor)**2))+
  scale_edge_width(range = c(0,.5))+
  geom_node_point(size = 1.5)+
  geom_node_text(label = colnames(numeric), col = 'red')
}

#### connectedness ####
n = nrow(cor)
frames = n
storage = list()
fun = function(i){
  k = ((cor%^%i + cor*(i-1)) / (max( cor%^%i + cor*(i-1)) ))*1/i
  storage[[i]] = k}
storage = sapply(1:frames, fun)

fun2 = function(i){
  return(colSums((matrix(storage[,i], n))))
}
means = rowMeans(sapply(1:frames, fun2))
par(mfrow = c(1,2))
matplot(storage, type = "l", lty = 1, lwd = 2, 
        col = scales::alpha(rainbow(n**2, start = .55, end = .8),.5),
        xlim = c(1,n), main = 'Strength of assosiations by degree')
barplot(means, names.arg = colnames(cor), las = 2, 
        density = 100, col = scales::alpha(rainbow(n, start = .01, end = .15)),
        main = "Strength of assosiations")


#### pca ####


# minor data wrangling
{
  # df = dplyr::sample_n(df, 3000)
vars = data.frame(y = df$danceability, x = df$instrumentalness,z = df$speechiness) # df$year)

year = round((scales::rescale(round(scale(df$year)*100,0))*10),0)
size = round(scale(df$popularity)+4,0)
func = colorRampPalette(c(2,7,3))
colist =func(max(year))
cols = colist[year]

par(mfrow = c(1,1))

numeric = as.data.frame(numeric)
for (i in 1:ncol(numeric))
  numeric[,i] = scales::rescale(numeric[,i])
}

#pca
{
pca = prcomp(numeric)
plot(pca$x[,1],pca$x[,2], col = scales::alpha(cols,.5),
     cex = (size**2)/10, pch = 20)
  }

#kmeans
{
k = kmeans(numeric, 2)
fviz_cluster(k, numeric, geom = 'point', stand = T, 
             ellipse = T, ellipse.alpha = .1, ellipse.type = 'norm',
             ggtheme = theme_bw(), show.clust.cent = F)
}

#### 3d box ####

# minor data wrangling
{
for (i in 1:3)
vars[,i] = scales::rescale(vars[,i])

lattice::cloud(y ~ x + z, col = cols,
               data = vars)
}

y = vars$y
x = vars$x
z = vars$z

#### interesting plots ####

# instrumentalness ~ speechiness
plot(y = df$instrumentalness,x =  df$speechiness, col = cols)

#popular songs by year
plot(as.factor(df$year), col = c(4,5), las = 1)

#matplot: overview
{
for ( i in 1:ncol(numeric))
  numeric[,i] = scales::rescale(numeric[,i])
for ( i in 1:ncol(numeric))
  numeric[,i] = numeric[,i] + i
numeric = round(numeric,2)
k = sort(numeric$popularity)
numeric = filter(numeric, popularity== k)
matplot(numeric, type = 'l', lty = 'solid')
}

#lm: energy ~ acousticness
{
plot(((df$acousticness)), df$energy, col = cols, 
     cex = ((df$popularity/80)**8)+.5, pch = 20)
range =(max(df$acousticness)-min(df$acousticness))
errC1 = rnorm(500, 1, sd(df$acousticness)/range)
errC1 = errC1[errC1 > 1-sd(df$acousticness)/range & errC1 < 1+sd(df$acousticness)/range]
errC2 = rnorm(length(errC1), 1, .05)
func = function(errC1,errC2){
  
lm = lm((energy)~ (acousticness)^2, data = df)
lm$coefficients[2] = lm$coefficients[2]*errC1
lm$coefficients[1] = lm$coefficients[1]*errC2
pred = predict(lm, 
               newdata = df)
lines(df$acousticness, pred, col = scales::alpha('red',.05), lwd = .025)
}
mapply(func, errC1, errC2)
}

#lm:loudness ~ log(energy)
{
  lm = lm(loudness ~ log(energy), data = df)
  plot((df$energy), (df$loudness), col = cols, 
       pch = 20, cex = (df$acousticness**2)+.5)
  points(seq(0,1, by = .001), predict(lm, list(energy =(seq(0,1, by = .001)))), pch = 20, 
         cex = .5, col = scales::alpha('red', .5))
}