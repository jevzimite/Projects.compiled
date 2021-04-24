library(rethinking)
library(rgl)
library(dplyr)
library(plot3D)
library(mgcv) 


#### rgl settings ####
# ellips <- ellipse3d(cov(cbind(x,y,z)),
#                     centre=c(mean(x), mean(y), mean(z)), level = 0.95)

fit <- gam(y ~ s(x) + s(z))
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)

#### RUNNING RGL ####
rgl.clear()


rgl.spheres(x,y,z, r = .005,
            alpha = 1,
            color = 2)
# color = rainbow(50))
rgl.surface(x.pred, z.pred, y.pred, color = "red",
            alpha = .1, lit = FALSE)
rgl.surface(x.pred, z.pred, y.pred, color = 'red',
            alpha = 0.20, lit = FALSE, front = "lines", back = "lines")
# shade3d(ellips, col =max(w)+3, lit = FALSE, alpha = .05)
# wire3d(ellips, col =  1,  lit = FALSE, alpha = 1)

rgl.texts(c(min(x), max(x)), c(0, 0), c(0, 0), "X")
rgl.texts(c(0, 0), c(min(y),max(y)), c(0, 0), "Y")
rgl.texts(c(0, 0), c(0, 0), c(min(z),max(z)), "Z")
rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = 1)
rgl.lines(c(0, 0), c(min(y),max(y)), c(0, 0), color = 1)
rgl.lines(c(0, 0), c(0, 0), c(min(z),max(z)), color = 3)


rgl.bbox(color = "gray")
