par(mfrow=c(1,1))
cols <- colorRampPalette(c("blue","yellow","red","black"))(10)

n= 100 #zoom
nn = 2000 #pixels
library(OpenImageR)
#### settings ####
# xmin = -2
# xmax = 2
# nx = nn
# ymin = -1.5
# ymax = 1.5
# ny = nn

# xmin = .2
# xmax = .4
# nx = nn
# ymin = .5
# ymax = .6
# ny = nn

xmin = .2
xmax = .4
nx = nn
ymin = .5
ymax = .6
ny = nn


# variables
x <- seq(xmin, xmax, length.out=nx)
y <- seq(ymin, ymax, length.out=ny)
c <- outer(x,y*1i,FUN="+") 
z <- matrix(0.0, nrow=length(x), ncol=length(y))
k <- matrix(0.0, nrow=length(x), ncol=length(y))
for (rep in 1:n) { 
  print(rep)
  index <- which(Mod(z) < 2)
  z[index] <- z[index]^2 + c[index]
  k[index] <- k[index] + 1
}

image
image(x,y,k, col=cols)