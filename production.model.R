# (IN)EFFICENCY AT PRODUCTION
ip.x = 1; ip.z = 1

# WEIGHTS (added value)
w.x = 1; w.z = 1

# DIMINISHING RETURNS ON Y (rate of decay)
dr.x = .999; dr.z = .999

# GRID
x = (1:100)**ip.x
z = (1:100)**ip.z; z2 = z[100:1]
xz = expand.grid(x,z)

y =  rescale(((xz[,1]*w.x)**dr.x )+ ((xz[,2]*w.z)**dr.z ))*100
y2 =  ((x*w.x)**dr.x )+ ((z2*w.z)**dr.z )

# VISUALIZATION
library(lattice) 
levelplot( matrix(y, ncol = sqrt( length(y))),
           main = '(In)efficiency and diminishing returns',
           xlab = 'X',
           ylab = 'Z')

col = colorRampPalette(c(2,2,7,7,3,3)); col = col(101); col = rainbow(101, start = .0, end = .3)
col = col[round(y)+1]
ind = match(max(y2),y2)
plot(xz$Var1, xz$Var2, cex = 13.5, pch = 15, col = col,
     main = '(In)efficiency and diminishing returns', xlab = 'X', ylab = 'Z')
lines(x,z2, lty = 2); points(x[ind],z2[ind], pch = 13, col = 'blue', cex = 3)


          