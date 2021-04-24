library(truncnorm)
library(scales)
library(mgcv)
library(dplyr)
grid.lines = 30
x = weight = rescale( (rtruncnorm(n = 3000,5, 120, 30))**.01 ,
                  to = c(5,120))
z = height = weight*2 + rnorm(3000, 0, 17)
z = height = rescale(height, to = c(30,230))
x = weight + rnorm(3000, 0, 10); 

# x = x[x>5];z = z[z>30];z = z[1:length(x)]
weight = x; height= z

frm = abs((weight*2/height) - 1)
y = attractiveness = rescale( (max(frm)-frm) ,
                          to = c(0.5,.95))*100
x.pred = seq(min(x), max(x), length.out = grid.lines)
z.pred = seq(min(z), max(z), length.out = grid.lines)
xz = expand.grid(x = x.pred, z = z.pred)

fit = gam(y ~ s(x)+s(z))
y.pred = predict(fit, newdata = xz)
y.pred = rescale(y.pred, to = c(25,97.5))
# frm = abs((xz$x*2/xz$z) - 1)
# y.pred = rescale( (max(frm)-frm) ,
#                   to = c(0.5,.95))*100

library(mclust)
m = Mclust(y.pred, G = 1:2)

rgl.clear();rgl.bbox();rgl.bg(col = 'grey')

rgl.surface(x.pred,z.pred,y.pred, col = 2, lit = T, alpha = .8)
rgl.surface(x.pred,z.pred,y.pred, col = 1, lit = F, 
            alpha = .5, front = 'lines', back = 'lines')
spheres3d(x,y,z, col = m$classification + 5)
# plot(z,x,cex = (y/100)**5)

attractiveness = attractiveness
# rgl.snapshot('snapshot.jpg', fmt = 'png', top = F)
flt = sort(attractiveness, decreasing = T)[.5*3000]
df = data.frame(attractiveness=attractiveness,
      weight = weight,
      height = height)
df = filter(df, attractiveness >= flt)
# plot(df$weight, df$height)
rgl.spheres(df$weight,df$attractiveness, df$height,  col = 7, r = 1.1)

flt = sort(attractiveness, decreasing = T)[.05*3000]
df = data.frame(attractiveness=attractiveness,
                weight = weight,
                height = height)
df = filter(df, attractiveness >= flt)
# plot(df$weight, df$height)
rgl.spheres(df$weight,df$attractiveness, df$height,  col = 'darkred', r = 1.2)

p = sort(y.pred, decreasing = T)[.05*length(y.pred)]


b = seq(min(y.pred)-5,max(y.pred)+5, 5); p1 = hist(y.pred , breaks = b);p2 = hist( y.pred[y.pred>p], breaks = b)
plot(p1, density = 25);plot(p2, add = T, col = 'gray95')



col = colorRampPalette(c(2,7,3))
col = col((grid.lines)+1)[round(rescale(y.pred, to = c(1, grid.lines+1) ))]
plot(xz$x, xz$z, cex = 4, col = col, pch = 15)
points( weight, height, pch = 4, cex = .5)




