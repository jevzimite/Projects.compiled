#### functions ####
library(rethinking)
emptyCanvas <- function(xlim, ylim, bg="black") {
  par(mar=rep(1,4), bg=bg)
  plot(1,
       type="n",
       bty="n",
       xlab="", ylab="",
       xaxt="n", yaxt="n",
       xlim=xlim, ylim=ylim)
}
iterate <- function(object, ifun, ...) {
  linesList <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    linesList[[length(linesList)+1]] <- new_line
  }
  new_object <- do.call(rbind, linesList)
  return(new_object)
}
drawLine <- function(line, col="white", lwd=1) {
  segments(x0=line[1],
           y0=line[2],
           x1=line[3],
           y1=line[4],
           col=col,
           lwd=lwd)
}
drawObject <- function(object, col="white", lwd=1) {
  invisible(apply(object, 1, drawLine, col=col, lwd=lwd))
}
newLine <- function(line, angle, reduce=1) {

  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]

  dx <- unname(x1-x0)                      # change in x direction
  dy <- unname(y1-y0)                      # change in y direction
  l <- sqrt(dx^2 + dy^2)                   # length of the line

  theta <- atan(dy/dx) * 180 / pi          # angle between line and origin
  rad <- (angle+theta) * pi / 180          # (theta + new angle) in radians

  coeff <- sign(theta)*sign(dy)            # coefficient of direction
  if(coeff == 0) coeff <- -1

  x2 <- x0 + coeff*l*cos(rad)*reduce + dx  # new x location
  y2 <- y0 + coeff*l*sin(rad)*reduce + dy  # new y location
  return(c(x1,y1,x2,y2))

}
tree <- function(line0, angle=30, reduce=.7, randomness=0) {

  # angles and randomness
  angle1 <- angle+rnorm(1,0,randomness)  # left branch
  angle2 <- -angle+rnorm(1,0,randomness) # right branch

  # new branches
  line1 <- newLine(line0, angle=angle1, reduce=reduce)
  line2 <- newLine(line0, angle=angle2, reduce=reduce)

  # store in matrix and return
  mat <- matrix(c(line1,line2), byrow=T, ncol=4)
  return(mat)

}
#####
emptyCanvas(xlim=c(-20,20), ylim=c(0,35))
#### TREE 2 ####
replicate(5,{


fractal <- matrix(c(0,0,0,10), nrow=1)
lwd <- .75
# drawObject(fractal, lwd=lwd)
for(i in 1:12) {
  lwd <- lwd*.75
  fractal <- iterate(fractal, ifun=tree, angle=39, randomness=30)
  drawObject(fractal, lwd=lwd)
}
}) #replicate

ex = fractal
x = ex[,1]
y = ex[,2]
z = x*y

x = scale(x)
y = scale(y)
z = scale(z)
# plot(x,y,
#      pch = 20,
#      cex = 1,
#      type = "p")
# 
# typeof(fractal)
# plot(fractal,
#      type = "p",
#      cex = 1,
#      col = col.alpha("white", 1),
#      pch = 15)
# 
# plot(fractal,
#      type = "l",
#      lwd = .1,
#      cex = 4,
#      col = col.alpha("white", 1),
#      pch = 15)
