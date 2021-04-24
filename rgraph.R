library(sna)
library(igraph)
library(ggraph)
library(tidygraph)
ggraph = as.matrix(rgraph(30))
mat0 = ggraph

c = closeness(ggraph)*10**2
e = evcent(ggraph)
# e = e$vector*30
btn = round(betweenness(ggraph),0)
colfunc = colorRampPalette(c(2,7,3))
colist= colfunc(max(btn))
col = colist[btn]


igraph <- graph_from_adjacency_matrix(ggraph)
ggraph <- as_tbl_graph(igraph)






ggraph(ggraph, "stress")+
  # geom_node_point(size = w, alpha = .35, col = "red")+
  geom_node_point(size = c, alpha = .5, col = "blue", shape = 1)+
  geom_node_point(size = c, alpha = .1, col = "blue")+
  geom_edge_hive(width = .1, alpha = 1)+
  geom_node_point(size = e, alpha = 1, col = col)+
  geom_node_point(size = e+.2, shape = 1)+


plot(density(colSums(rgraph)),
     xlim = c(min(colSums(rgraph)),max(colSums(rgraph))),
     )




k = 100

b = scales::rescale(betweenness(ggraph))*k
c = scales::rescale(closeness(ggraph))*k
e = scales::rescale(e)*k
v = scales::rescale((colMeans(rgraph%*%rgraph+rgraph)))*k


s = 5
ggplot()+
  geom_point(mapping = aes(b,c), col = 4, size = s)+
  geom_point(mapping = aes(b,e), col = 6, size = s)+
  geom_point(mapping = aes(b,v), col = 7, size = s)+
  geom_point(mapping = aes(b,c), col = 1, shape = 1, size = s+.2)+
  geom_point(mapping = aes(b,e), col = 1, shape = 1, size = s+.2)+
  geom_point(mapping = aes(b,v), col = 1, shape = 1, size = s+.2)+
  geom_smooth(mapping = aes(b,c), col = 4, size = 1, 
              linetype = 8, alpha = .0, fill = 4)+
  geom_smooth(mapping = aes(b,e), col = 6, size = 1, 
              linetype = 8, alpha = .0, fill = 6)+
  geom_smooth(mapping = aes(b,v), col = 7, size = 1, 
              linetype = 8, alpha = .0, fill = 7)+
  theme_classic()+ylim(c(0,100))


r = arules::apriori(rgraph)
# arulesViz::ruleExplorer(r)
