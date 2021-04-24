y = df$Value[1:100]
x = 1:length(y)

par(mfrow = c(1,2))
spl <- smooth.spline(y ~ x, spar = .0001)
plot(spl , type = 'l', lwd = 1)

seq = seq(80, 90, .01)
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
  # points(pred0, col=1, pch=1, cex = 1) # point to predict tangent 
  lines(x, yint + pred1$y*x, 
        col='blue', lwd = .1) # tangent (1st deriv. of spline at newx)
}
lines(spl,col = 2, type = 'l', lwd = 2, lty = 1)
plot(density(list), type = 'l')
# plot(density(list))
length(list[list>0])/length(list)
length(list[list<0])/length(list)

