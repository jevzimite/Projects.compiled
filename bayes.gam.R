library(rethinking)
library(splines) 

data(cherry_blossoms)
d <-cherry_blossoms
d2 <-d[complete.cases(d$doy),]#completecasesondoy

num_knots <-14
knot_list <-quantile(d2$year,probs=seq(0,1,length.out=num_knots))
B <-bs(d2$year,
       knots=knot_list[-c(1,num_knots)] ,
       degree=1 ,intercept=TRUE)

m4.7<-quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <-a+B%*%w,
    a ~dnorm(100,10),
    w ~dnorm(0,10),
    sigma ~dexp(1)
  ), data=list(D=d2$doy,B=B),
  start=list( w=rep(0,ncol(B))))
post <-extract.samples(m4.7)

mu<-link(m4.7)
mu_PI <-apply(mu,2,PI,0.99)


plot(x = d2$year, y = d2$doy, type = "p",
     pch = 8,
     col = 1)
shade( mu_PI,
       d2$year,
       col=col.alpha(2,0.6)
)
precis(d)

cor(df$idl.102, df$rmn.103)

gam = gam(doy~s(year), data = d2)
plot.gam(gam)

ggplot()+
  geom_density(mapping=aes(mu), fill = 2)+
  geom_density(mapping=aes(d2$doy), fill = 4)
