# ##### logistic sim ####
# par(mfrow=c(4,4))
# set.seed(1)
# fun = function(x){plot(DChaos::logistic.sim(a=x, n = 80),
#                        col = col.alpha("darkblue", 1),
#                        lwd = 1)}
# x = seq(from=0, to=4, length.out = 16)
# sapply(x, fun)
#  
# par(mfrow=c(1,1))
# # plot(DChaos::logistic.sim(a=1.449805, n = 1000),
# #   col = col.alpha("darkblue", 1),
# #   lwd = 1,
# #   type = "l",
# #   cex = .1,
# #   pch = 20
# # )
### logistic map ####
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations
  z[c((N-M):N)]
}

my.r <- seq(2.5, 4, by=0.001)
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.1, N=1000, M=300))

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, 301))

plot(Orbit ~ r,
     pch=20,cex = .05,
     col = col.alpha(1,.5))


