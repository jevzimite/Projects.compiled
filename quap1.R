library(ggplot2)
library(rstan)
library(rethinking)
library(truncnorm)

##parameters; w = wins, l = loses, n = times it is repeated
w= 1785
l= 1764
n = 1000000

##model

ans<- quap(
  alist(
    W ~ dbinom(W+L, p),
    p ~ dunif(0,1)
  ) ,
  data = list (W = w, L = l)
)
rets<-precis(ans)
a = rtruncnorm(n=n, a=0, b=1, mean=rets$mean, sd=rets$sd)
rets$mean

return = list(mean = rets$mean, sd = rets$sd, dens = density(a), dist = a )



##results
# plot(ans$dens)
ans$sd
ans$mean

K=ans$mean + ans$sd
K2=ans$mean - ans$sd

dist=ans$dist
most = dist[dist< K & dist>K2]


ggplot()+
  geom_histogram(mapping=aes(x=dist),fill = "deepskyblue4", alpha =1)+
  geom_histogram(mapping=aes(x=most), fill="deepskyblue3", alpha=.4)+
  theme_classic()
