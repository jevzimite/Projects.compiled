set.seed(0)
R = 6
n = 1000000
pop = 16

binomialDist <- function(R , n , pop){
min = 0
max = 1
a = runif(n , min, max)

b = rep(NA , n)


gen_model <- function(rate) {
  subscribers <- rbinom(1, size = 16, prob = rate)
  subscribers 
}


for(i in 1:n){
  # b[i] = rbinom(n , size = pop , prob = a[i])
  b[i] = gen_model(a[i])
}
c = a[b==R]

# fun <-function(x){
#   return( rbinom(n , size = pop , prob = x))
# }
# c = sapply(a, fun)
# c =a[c==R]
# c =na.omit(c)

return(c)
}
c = binomialDist(R, n , pop)

plot(density(c))
mean(c)
length(c)

