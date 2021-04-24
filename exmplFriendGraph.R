names = c(
  'josh',
  'anisha',
  'nina',
  'viola',
  'laura',
  'emma',
  'emilio',
  'ingrid',
  'caroline',
  'lala',
  'luis',
  'pedro',
  'tanga',
  'pope',
  'jorge',
  'mustafa',
  'ishan',
  'vivek',
  'dixita',
  'val',
  'zach',
  'ada')

netdf = t(combn(names, 2))
shp = scan()
con = scan()
netdf = data.frame(cbind(netdf, con))

netdf = filter(netdf, con == 1)
netdf = netdf[,1:2]

routes_igraph <- graph_from_data_frame(d = netdf, vertices = names, directed = FALSE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)


ggraph(routes_igraph_tidy, layout = "stress" ) + 
  geom_edge_density()+
  geom_edge_hive(aes(), alpha = 0.5, linetype = 7, color = 1, width = .2)+
  geom_node_point(size = 4.5, color = "white", shape = 20) +
  geom_node_point(size = 3, color = "black", shape = 1) 
  # geom_node_text(aes(label = names, fontface = 2), size = 2, color = 2)

mat = igraph::as_adjacency_matrix(routes_igraph)
mat = mat %*% mat
mat = melt(as.matrix(mat))
mat = data.frame(mat)

filter(mat, Var1 == "emilio" | Var2 == "emilio")

mat = as.matrix(mat)
mat = mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat%*%mat
mat = round(mat/1e15,0)
# matplot(mat, type = "l", lty = "solid", col = rainbow(1, alpha = .5), lwd = .5)
barplot(colMeans(mat), las = 2, col = rainbow(5))
