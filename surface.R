library(truncnorm)
library(scales)
library(rgl)
library(mgcv)

grid.lines = 23
weight = rtruncnorm(grid.lines, 3, 110, 70, grid.lines)
# weight = seq(3,110,1)
height = rescale(log(weight), to = c(50, 200))
attractiveness =   rescale(c(max(abs(((height/2)/weight) - 1)) -
                   abs(((height/2)/weight) - 1),0), to = c(0,1));attractiveness = attractiveness[1:(length(attractiveness)-1)]
grid.lines = length(weight)
par(mfrow=c(2,2))
plot(weight,height); plot(density(weight)); plot(density(height)); plot(density(attractiveness))
par(mfrow=c(1,1))
plot(weight,height, cex = (attractiveness**2)*5)

x = weight; z = height; y = attractiveness


x.pred = seq(min(x),max(x), length.out = grid.lines)
z.pred = seq(min(z), max(x), length.out = grid.lines)

x.pred = sort(x)
z.pred = sort(z)

xz = expand.grid(x = x.pred, z = z.pred); x.pred = xz[,1]; z.pred = xz[,2]

y.pred = rescale(c(max(abs((z.pred/2)/x.pred - 1)) -
           abs((z.pred/2)/x.pred - 1),0), to = c(0,1));y.pred= y.pred[1:(length(y.pred)-1)]

# y.pred = predict(
#   gam(y~s(x)+s(z)), newdata = xz
# )

x.pred = seq(min(x), max(x), length.out = grid.lines)
z.pred = seq(min(z), max(z), length.out = grid.lines)

rgl.clear()
rgl.surface(rescale(x.pred), rescale(z.pred), rescale(y.pred), color = rainbow(length(y.pred), start = .15, end = .35),
            alpha = .8, lit = T)
rgl.surface(rescale(x.pred), rescale(z.pred), rescale(y.pred), color = 1,
            alpha = .25, lit = FALSE, front = "lines", back = "lines")
{
lines3d(c(min(rescale(x.pred)), max(rescale(x.pred))), c(0, 0), c(0, 0), color = 3, lwd = 3)
lines3d(c(0, 0), c(min(rescale(y.pred)),max(rescale(y.pred))), c(0, 0), color = 'blue', lwd = 3)
lines3d(c(0, 0), c(0, 0), c(min(rescale(z.pred)),max(rescale(z.pred))), color = 6, lwd = 3)
rgl.texts(c(max(rescale(x.pred))), c(0, 0), c(0), color = 3, text = "weight")
rgl.texts(c(0, 0), c(max(rescale(y.pred))), c(0), color = 'blue', lwd = 3, text = "attractiveness")
rgl.texts(c(0), c(0), c(max(rescale(z.pred))), color = 6, lwd = 3, text = "height")}

ind = match(max(attractiveness), attractiveness)
x[ind]
y[ind]


ind = match(max(rank(y.pred)), rank(y.pred))
par(mfrow = c(1,1));plot(rank(y.pred), col = col, pch = 20)
points(y = rank(y.pred)[ind], x = ind, col = 1, pch = 'o', cex = 2)

