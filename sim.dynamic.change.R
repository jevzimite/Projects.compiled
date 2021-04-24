library(rgl)
grid.lines = 50
max.prod.x = 30
max.prod.z = 30
rel.value.x = 1
rel.value.z = 1
seq = seq(0,2, length.out = 10)
alph = 1
col = colorRampPalette(c(5,6))
col = col(grid.lines**2);col = col[round(rank(y.pred))]


storage1 = array( rep(0, 26*26*length(seq)),
                  dim = c(grid.lines,grid.lines,
                          length(seq))); storage2 = storage1
rgl.clear(); rgl.bbox()
fun = function(x,z){
  y = scales::rescale( (x)*rel.value.x + (z)*rel.value.z, to = c(.5,9.5) )
  return(y)}

count = 0 
for (i in seq){
  rel.value.x = i
  
x = rescale(( log(1:grid.lines) ), to = c(0, max.prod.x) ) 
z = rescale(( log(grid.lines:1) ), to = c(0,max.prod.z) )
y = fun(x,z); y.1 = fun(x = x, z = z)
fit = gam(y~x+z)

ind = match(max(y), y)
# plot(x,z, cex = y.1/2, type = 'l',
#      main = paste('Optimization index:', ind)); points(x[ind],                                                       z[ind], pch = 4, cex = 1.5, col = 'red')

x.pred = sort(x)
z.pred = sort(z)
xz = expand.grid(x = x.pred, z = z.pred)
y.pred = predict(fit, xz)
# y.pred = fun(x = xz$x,z = xz$z)

rgl.surface(x.pred, z.pred, y.pred, color = col,
            alpha = alph, lit = T)

# segments3d(x,y.1,z, lwd = 3, col = 'red')
count = 1 + count
storage1[,,count] = y.pred
}

count = 0
for (i in seq){
  rel.value.z = i
  
  x = rescale(( (1:grid.lines)**k ), to = c(0, max.prod.x) ) 
  z = rescale(( (grid.lines:1)**k ), to = c(0,max.prod.z) )
  y = fun(x,z); y.1 = fun(x = x, z = z)
  fit = gam(y~x+z)
  
  ind = match(max(y), y)
  # plot(x,z, cex = y.1/2, type = 'l',
  #      main = paste('Optimization index:', ind)); points(x[ind],                                                       z[ind], pch = 4, cex = 1.5, col = 'red')
  
  x.pred = sort(x)
  z.pred = sort(z)
  xz = expand.grid(x = x.pred, z = z.pred)
  y.pred = predict(fit, xz)
  # y.pred = fun(x = xz$x,z = xz$z)
  
  rgl.surface(x.pred, z.pred, y.pred, color = col,
              alpha = alph, lit = T)
  
  # segments3d(x,y.1,z, lwd = 3, col = 'red')
  count = 1 + count
  storage2[,,count] = y.pred
}

x = array(c(c(storage1),c(storage2)), dim = c(grid.lines,grid.lines,length(seq)*2))

res = matrix(rep(0,grid.lines**2), ncol = grid.lines)

lattice::levelplot(res)

for ( i in 1:length(seq)*2){
  res = res + x[,,i]}



