library(sna)
library(lattice)
library(ggraph)
library(igraph)


mat =sna::rgraph(26, tprob = .125)
m = mat
mat0 = scales::rescale(colSums(mat)/2)
sides = nrow(mat)
k = 8
names = letters[1:sides]


null = rep(NA, (sides**2)*k)
storage = array(data = null, dim = c(sides,sides, (k+1)))
res = matrix(rep(0,sides**2), ncol = sides)
storage[,,1] = mat
count = 1
for (i in 1:k){
  count = count + 1
  storage[,,i] = (mat %*% mat)*(1/count) 
  mat = mat%*%mat}
for ( i in 1:k){
  res = res + storage[,,i]}

measure = colSums(res)/max(colSums(res))

levelplot(res)
barplot(measure, col = 5, names.arg = names, density = 50)
barplot(mat0, col = 2, add = T, density = 50)


col = colorRampPalette(c(2, 7))
col = col( 11)
# scales::rescale(mat0, to = c(0,50))
cols = col[round((mat0)*10)+1]

g = graph_from_adjacency_matrix(m)
ggraph(g, 'fr')+
  geom_edge_link(width = .2)+
  geom_node_point(size = (measure**2)*10, color = cols)+
  geom_node_point(size = (measure**2)*10, shape = 1)+
  geom_node_text(label = names, size = (measure**2)*5)
