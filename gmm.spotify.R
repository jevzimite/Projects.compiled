# spotify <- read_excel("Desktop/R/xlsm/spotify.xlsm")
df = spotify

model = Mclust(df$popularity, 1:3)
df = cbind(df, class = model$classification)

df$year = NULL
df$explicit = NULL
df$name = NULL

unpop = filter(df, class == 1)
pop = filter(df, class ==3)

a = c(rep(NA, (ncol(df))))
for (i in 1:(ncol(df)-1))
  a[i] = t.test(pop[,i], unpop[,i])$p.value

dfSD = c()
for ( i in 1:ncol(df))
  dfSD[i] = sd(df[,i])

t.tests = cbind(round(a,4),colnames(df))
res = cbind(colnames(df),round(colMeans(df),2),
      round(colMeans(pop),2),round(colMeans(unpop),2),
      round((abs(round(colMeans(pop),2) - round(colMeans(unpop),2))/dfSD),2))
res = res[order(res[,5]),]
res = res[1:11,]
rownames(res) = NULL
colnames(res) = (c("Dim", 'MU', 'Pop means', 'Unpop means', 'Effect size'))

d1 = truncnorm::rtruncnorm(1e6*0.27, -5, 20 , 0, 1.25)#15/2+2.5
d2 = truncnorm::rtruncnorm(1e6*0.73, 0, Inf, 35, ((80-30)/2)*.9)
d1 = cbind(d1, rep(1, length(d1)))
d2 = cbind(d2, rep(2, length(d2)))
dist = rbind(d1,d2)

plot(density(df$popularity), ylim = c(0,.2), lwd =2, xlim = c(3,100))
lines(density(pop$popularity), col = 6, lwd = 2, lty = 2)
lines(density(unpop$popularity), col = 5, lwd = 2, lty = 2)
lines(density(dist[,1]), col = 'red', lwd = 2, lty = 2)

res

