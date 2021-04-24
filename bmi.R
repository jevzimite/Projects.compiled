library(scales)
library(rgl)
grid.lines = 100
weight = rnorm(1e2, 77, 25)
weight = weight[weight>5]
height = rescale((weight*2)**.025, to = c(30,220))
weight = weight + rnorm(length(weight), 0, 5)
height = height + rnorm(length(weight), 0, 6.5)

bmi = weight / (height*.01)**2
y = abs((bmi/18.5)-1)
y = rescale(max(y)-y)*100
y= round(rescale(y**10)*100)


x.pred = seq(5, 170, length.out = grid.lines)
z.pred = seq(30, 220, length.out = grid.lines)
xz = expand.grid(weight = x.pred, height = z.pred)



bmi. = xz$weight / (xz$height*.01)**2
y. = abs((bmi./18.5)-1)
y.pred = rescale(max(y.)-y.)*100
y.pred = round(rescale((y.pred)**10)*100)
# col = colorRampPalette(c('red','yellow','green')); col = col(grid.lines**2)
col = rainbow(grid.lines**2, end = .5)
col = col[rank(y.pred)]
plot(xz$weight, xz$height, pch = 15, cex = 2,
     col = col, main = 'Ideal bmi ~18.5',
     xlab = 'Weight (kg)', 
     ylab = 'Height (cm)')
points(weight,height, pch = 4, cex = .5)

i = rescale(rank(y.pred))*100
i = y.pred
rgl.clear();rgl.bbox()
rgl.surface(x.pred,z.pred, i, col = col, lit = T, alpha = .5)
rgl.surface(x.pred,z.pred, i, col = 1,
            front = 'lines', back = 'lines',
            alpha = .5)
rgl.spheres(weight,y,height, col = 'red', r = 1, lit = T)

# ans = filter( data.frame(xz, y.pred),
#         y.pred >90)
# ans = filter(ans, height > 180 & height < 183, weight > 75)
# ans[order(ans$y.pred),]
# length(i[i>98])/length(i)
# plot(density(i))

