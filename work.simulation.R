library(rgl)
grid.lines = 20
max.prod.x = 30
max.prod.z = 30
rel.value.x = 1
rel.value.z = 1
alph = 1
col = colorRampPalette(c(2,7,3))
k = .5
rgl.clear(); rgl.bbox()

fun = function(x,z){
  y = scales::rescale( (x)*rel.value.x + (z)*rel.value.z, to = c(.5,9.5) )
  return(y)}

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
col = col(400);col = col[round(rank(y.pred))]

rgl.surface( (x.pred), (z.pred), (y.pred), 
            color = col,
            alpha = 1)

segments3d( (x), (y), (z), lwd = 3, col = 'red')

# surface3d(rescale(x.pred), rescale(z.pred), rescale(as.double(res)),
#           lit = F, col= 'red')

ind2 = match(max(rank(y.pred)), rank(y.pred))
par(mfrow = c(1,1));plot(rank(y.pred), col = col, pch = 20)
points(y = rank(y.pred)[ind2], x = ind2, col = 1, pch = 'o', cex = 2)
points(y = rank(y.pred)[ind], x = ind, col = 1, pch = 'o', cex = 2)


levelplot( matrix(y.pred, ncol = sqrt( length(y.pred))))
