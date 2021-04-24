library(expm)
library(caTools)
library(igraph)
a = igraph::tree()
b = a$fun(n = 100, children =1)
c = tidygraph::as_tbl_graph(b)
c = as_adjacency_matrix(c)

k = sna::rgraph(30)
mat0 = as.matrix(c)

frames = 50#max exponent raised to

n = nrow(mat0)
# n = 5
storage <- list()
# storage <- array(0, c(n, n, frames))

fun = function(i){
  k = (
    (mat0%^%i + mat0*(i-1)) / (max( mat0%^%i + mat0*(i-1)) )
       )*1/i
  storage[[i]] = k
}

# x = seq(1,frames, by = .1)
storage = sapply(1:frames, fun)
storage2 = array(0, c(n, n, frames))

for(i in 1:frames)
  storage2[,,i] = matrix(storage[,i], n)


write.gif(storage2, "conway.gif", col="jet", delay=5)


