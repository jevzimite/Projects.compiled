par(mfrow=c(1,1))
set.seed(1)
fun = function(x){
                  # plot(DChaos::logistic.sim(a=x, n = 80),
                  #      col = col.alpha("darkblue", 1),
                  #      lwd = 1)
                  lines(DChaos::logistic.sim(a=x, n = 10),
                    col = col.alpha(rainbow(1),.01),
                    lwd = 1)
                  }
x = seq(from=2.8, to=4, length.out = 5000)
plot(x = c(1, 10), y = c(0, 1.1), cex =0)
sapply(x, fun)

