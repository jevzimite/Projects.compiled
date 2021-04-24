#### libraries ####
library(ggraph)
library(graph)
library(igraph)
library(tidygraph)
library(sna)

#### names ####

names = c(LETTERS,letters)
names = names[1:38]

#### scan ####
colors = scan()
  colors = c(abs(as.numeric(colors)))
  colors[colors == 8] = "white"
con = scan()

#### building ####
netdf = data.frame(t(combn(names, 2)))
# writexl::write_xlsx(netdf, path = "Desktop/R/xlsm/netdf.xlsx")
netdf = cbind(netdf, con)

netdf$con = as.numeric(netdf$con)
netdf = filter(netdf, con == 1)
netdf = netdf[,1:2]
routes_igraph <- graph_from_data_frame(d = netdf, vertices = names, directed = FALSE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

mat = igraph::as_adjacency_matrix(routes_igraph)
mat0 = as.matrix(mat)
mat = mat0 %*% mat0 + mat0

#### network ####
n = max(round(sna::betweenness(mat0),0))+1
colfunc <- colorRampPalette(c(2, 7, 3))
col = colfunc(n)
btn = round(sna::betweenness(mat0),0)+1

btn = col[btn]
ev = ((sna::evcent(mat0)*10)**2)
c = (sna::closeness(mat0)*10)**2

ggraph(routes_igraph_tidy, layout = 'stress') + 
  geom_edge_density(aes(), fill = "gray50")+
  geom_node_point(color = scales::alpha(colors,.25), shape = 20, size = c) +
  geom_edge_diagonal(aes(), alpha = 1, linetype = 7, color = 1, width = .075)+
  geom_node_point(size = ev+1, shape = 20) +
  geom_node_point(size = ev, color = btn, shape = 20) +
  geom_node_text(aes(label = names, fontface = 1), repel = F, size = 3, color = "black")+
  labs(title = "Social Network Analysis")

#### Metrics ####
b = round(scales::rescale(sna::betweenness(mat0))*100,0)+1
c = round(scales::rescale(sna::closeness(mat0))*100,0)+1
v = round(scales::rescale(colSums(mat0%*%mat0+mat0))*100,0)+1
ev = round(scales::rescale(sna::evcent(mat0))*100,0)+1
ind = rowMeans(cbind(c,v,ev))

plot(data.frame(Closeness = c,Virality = v,
                EigenCentral. = ev,
                Betweenness = b),
     col = scales::alpha(colors,.5), pch = 20, cex = 2)

#### barplots ####
par(mfrow = c(1,2))

barplot(ind, las =2, col = rainbow(38, start = .5, end = .55),
        density = 125, main = "Centrality")

barplot(b, names.arg = names, horiz = F, 
        las = 2, density = 125,
        col = rainbow(38, start = .2, end = .4),
        main = "Betweenness")

#### dev from linear model ####
i = ind
i2 = data.frame(scale(i))+3
btw2 = data.frame(scale(b))+3
lm = lm(i2[,1] ~ log(btw2[,1]))
i3=(predict(lm, btw2))

i4.s= data.frame(seq(2.3,6.65, by = .1))
i4= (predict(lm, newdata = list(btw2=i4.s)))

dev= data.frame((scale(i2 - i3)))
barplot(dev[,1], names.arg = names, horiz = F, 
        las = 2, density = 125,
        col = rainbow(38, start = .01, end = .1),
        main = "Deviations from a lm")


#### plotting linear model ####

plot(btw2[,1], i2[,1], xlab = "Betweenness", ylab = "Centrality", 
     col = colors,
     pch = 20, cex = 2, main = "Centrality ~ Betweenness")
points(btw2[,1], i2[,1], cex = 1.5)
# points(btw2[,1], i2[,1], cex = .5, pch = names)
points(i4.s[,1], i4, pch = 20, cex = .1, type = 'l', lwd = 1.2, lty = "dashed", )
text(3.5-.5, 5.2+.35,
     paste("Correlation of", round(cor(btw2[,1],i2),2)),
     cex = .5)
text(4-.5, 5+.35,
     paste("When using log-linear model, r**2 =",round( cor(btw2[,1],i3)**2 , 2)),
     cex = .5)

r = arules::apriori(mat0)
# arulesViz::ruleExplorer(r)
# arulesViz::inspectDT(r)
l = length(r)/2
m = arules::inspect(r[1:l])
m[,2] = NULL
m[,3:7] = round(m[,3:7],2)
ind0 = sort(m$lift, decreasing = T)
f0 = filter(m, m$lift == ind0)

library(dplyr)
f = filter(m, m$rhs == "{emilio}")
ind1 = sort(f$lift, decreasing = T)
filter(f, lift == ind1)
plot(ind0,
     col = alpha(1, 1), 
     type = "l")

r = arules::apriori(mat0)
arulesViz::ruleExplorer(r)


a = shortest_paths(igraph, from = 2, to = 1:30)
a$vpath
k = c()
for (i in 1:30)
  k[i] = length(a$vpath[[i]])
plot(k, type = 'b')
