#### F(X), LIBRARIES & PLOT-SETTINGS ####
library(scales)
library(rethinking)
library(rgl)
library(dplyr)
library(plot3D)
library(mgcv) 
tan.slope = function(x,y){
  spl <- smooth.spline(y ~ x, spar = .0001)
  
  seq = seq(1,length(y), 1)
  list = c(rep(NA, length(seq)))
  count = c(0)
  for ( i in seq){
    count = count + 1
    newx <- i
    pred0 <- predict(spl, x=newx, deriv=0)
    pred1 <- predict(spl, x=newx, deriv=1)
    yint <- pred0$y - (pred1$y*newx)
    xint <- -yint/pred1$y
    list[count] = pred1$y
  }
  return(list)}
par(mfrow = c(3,1))
rgl.clear()

##### SIMULATION ####

n = 50 #number of good1
rate.of.prod.good2 = 1 #relative to good1
good1 = (1:n); good2 = (((n:1)*rate.of.prod.good2) )

## ASSUMPTION OF DIMINISHING RETURNS WHEN FORAGING
    # good1 = log(good1)
    # good2 = log(good2)

benefit1 = 1 ;   benefit2 = 1 #usually related to rate.of.prod
decay1 = .7  ;   decay2 = .7 #ASSUMPTION OF DIMINISHING RETURNS 

rwd =  (good1*benefit1)**decay1 + (good2*benefit2)**decay2

index = match(max(rwd), rwd)
sum = paste('PPF;',
            'good1 = ', good1[index],
            ', good2 = ', good2[index])

plot(good1,good2, cex = (scales::rescale(rwd))*5, type = 'l',
     main = sum); points( good1[index], good2[index], col = 'red',
                          pch = 4, cex = 2)
plot(rwd, type = "l",
     main = "Utility/reward" ); plot( tan.slope(1:n,rwd),
                                      main = 'Slope of the reward curve',
                                      type = 'b' , )
#### 3D plot settings ####

z = rescale(good1); x = rescale(good2); y = rescale(rwd)

fit <- gam(y ~ s(x) + s(z))
grid.lines = 17
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)

segments3d(x,y,z, lwd = 2.5,
            alpha = 1,
            color = 'red')

rgl.surface(x.pred, z.pred, y.pred, color = rainbow(300, start = .5, end = .6),
            alpha = .55, lit = FALSE)
rgl.surface(x.pred, z.pred, y.pred, color = 1,
            alpha = .25, lit = FALSE, front = "lines", back = "lines")

rgl.texts(c(min(x), max(x)), c(0, 0), c(0, 0), "good2")
rgl.texts(c(0, 0), c(min(y),max(y)), c(0, 0), "RWD")
rgl.texts(c(0, 0), c(0, 0), c(min(z),max(z)), "good1")
rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = 1)
rgl.lines(c(0, 0), c(min(y),max(y)), c(0, 0), color = 2)
rgl.lines(c(0, 0), c(0, 0), c(min(z),max(z)), color = 3)

rgl.bbox(color = "white")
