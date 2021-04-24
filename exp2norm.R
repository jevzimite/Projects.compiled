exp = 2
ndist = rnorm(100000, 50, 25)
dist = (ndist)**exp


plot(density(ndist), col = 2, lwd = 2,
     ylim=c(0, 0.03), xlim = c(5,100))

y = fun = function(x){
x = dist**x
lines(density(x), lwd = .15)
return(x)
}
y = sapply(seq((1/exp - .05), (1/exp + .05) , length.out = 500), fun)
lines(density(y[,250]), lty = 2)
colMeans(y)

