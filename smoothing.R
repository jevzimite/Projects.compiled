par(mfrow=c(1,1))
# smooth.spline()
# runmed()

l2 = c(1:(length(df$Close)))
fxn = function(k){
l = c(1:(length(df$Close)))
l = l[l%%k == 1]
h =df[l,]

lines(x = l, y = h$Close,
      lwd = 1.5,
      col = rainbow(2),
      type = "l")
}

plot(x = l2, y = df$Close,
     cex = 0,
     type = "p")
# k = round(seq(from = 55, to = 65, length.out = 3),0)
# sapply(k, fxn)
fxn(30)

