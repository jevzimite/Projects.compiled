# spc = readline(prompt = "What species would you like to build a network for?")
# spc = as.character(spc)
spc = "setosa"
df = iris
df = iris %>% filter(Species == spc)
df$Species = NULL
round(cor(df),2)
cmb = combn(colnames(df), 2)
from = cmb[1,]
to = cmb[2,]
netdf = data.frame(from, to)
weights = c(.74,
            .27,
            .28,
            .18,
            .23,
            .33)
netdf = cbind(netdf, weights)
routes_igraph <- graph_from_data_frame(d = netdf, vertices = colnames(df), directed = FALSE)
plot(routes_igraph)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

ggraph(routes_igraph_tidy, layout = "linear" ) + 
  # geom_edge_arc(aes(colour = 2))+
  geom_node_point(size = 33, color = 2) +
  geom_edge_arc(aes(width = netdf$weights), alpha = 0.75, linetype = 7, color = 2) +
  scale_edge_width(range = c(.1, 5)) +
  geom_node_text(aes(label = colnames(df)), repel = 0, color = "black", fontface = 2) +
  labs(edge_width = "Correlation") + ylim(c(-.1,1.2))+xlim(c(-.2,5))+
  theme_graph()



