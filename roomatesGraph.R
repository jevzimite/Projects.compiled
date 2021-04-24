#### libraries ####
library(ggraph)
library(graph)
library(igraph)
library(tidygraph)
library(sna)

#### attributes ####
names = c("Ben", "Josh", "Jorge", "Emilio", "Owen", "Mustafa")
# shp = c("o", "s", "s", "i", 'i', 'o')
fromNto =t(combn(names ,2))
weights = c(7, 7.5, 6.5, 7.5, 6, 8, 7.5, 7.75, 7, 7.5, 7.75, 7, 7, 8, 7)
netdf = cbind(fromNto, weights)
netdf = as.data.frame(netdf)
netdf$weights = as.numeric(netdf$weights)
netdf$weights = scales::rescale(netdf$weights)

#### pruning ####
k = quantile(netdf$weights)[2]
# k = mean(netdf$weights)
netdf = filter(netdf, weights > k)

#### building ####
routes_igraph <- graph_from_data_frame(d = netdf, vertices = names, directed = FALSE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

mat0 = as.matrix(igraph::as_adjacency_matrix(routes_igraph))
mat = mat0 %*% mat0 + mat0
w = (sna::evcent(mat0)*10)**2*2



#### plotting ####
n = max(round(betweenness(mat0),0))+1
colfunc <- colorRampPalette(c(2,7, 3))
col = colfunc(n)
btn = round(betweenness(mat0),0)+1
btn = col[btn]

ggraph(routes_igraph_tidy, layout = "stress" ) + 
  geom_node_point(size = w, color = scales::alpha("darkorange", .3), shape = 20) +
  geom_edge_link(aes(width = weights), alpha = 1, linetype = 7, color = 1)+
  scale_edge_width(range = c(.075, .25))+
  geom_node_point(size = 8, color = "white", shape = 20) +
  geom_node_point(size = 7.5, color = (btn), shape = 20) +
  geom_node_point(size = 5, color = 1, shape = 1)+
  # geom_node_point(size = 3.5, color = 1, shape = shp)+
  geom_node_text(aes(label = names, fontface = 2), repel = F, size = 1)+
  labs(edge_width = "Strength of rel.", title = "House relationships")
  
#### further analysis ####
par(mfrow = c(1,3))
mat0 = as.matrix(igraph::as_adjacency_matrix(routes_igraph))
mat = mat0 %*% mat0 + mat0

inf =sapply(names, function(x){sum(netdf$weights[netdf$V1 == x | netdf$V2 == x])} )
barplot(inf, col = rainbow(6, start = .5, end = .6), las = 2, main = "Cumulative influence")



i = colMeans(mat)
barplot(i, las = 2, col = rainbow(6, start = .2, end = .4), main = "Virality")
barplot(betweenness(mat0), col = rainbow(6, start = .7, end = .8),
        main = "Betweeness", las = 2,
        names.arg = names)

#### dist. on the degrees ####


colfunc <- colorRampPalette(c(2, 3))
colfunc(6)

#### lm ####
par(mfrow = c(1,2))
b = betweenness(mat0)+1
i = i+1
lm2 = lm(i ~ log(b))
pred2 = predict(lm2, list(b = seq(1, 9, by = .1)))

pred = predict(lm2, list(b=b))
max = max(max(pred),max(i))
barplot(((round(pred, 2) - round(i, 2))/max), 
        names.arg = names,
        las = 2, col = rainbow(6, start = .01, end = .1),
        ylab = "Error rate")

plot(
  y = i, x = betweenness(mat0), 
  pch = 20, cex = 2,
  col = 1,
  xlab = "Betweenness", ylab = "Virality"
)
lines(seq(0, 8, by = .1), pred2, lty = 2, col = 2)

avrg.err.r = round(mean(abs((round(pred, 2) - round(i, 2))/max)), 2)
expl.var = round(cor(b,pred)**2,2)
