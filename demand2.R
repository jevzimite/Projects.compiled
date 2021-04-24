df = beezdemand::apt
price = (df$x)
cons = (df$y)
z = beezdemand::GetEmpirical(apt)
x = seq(from = min(df$x), to = max(df$x), length.out = 100)
model = mgcv::gam(cons ~ s(price))
pred = predict(model, newdata = list(price = x))^1


plot(price, cons,
     xlab = "Price", ylab = "Consumption",
     main = "Demand Curve", lwd = 0,
     col = rethinking::col.alpha("brown", .05),
     pch = 20, cex = 3)
lines(x = c(mean(z$Pmaxe),mean(z$Pmaxe)), y = c(-100,100), lty = "dashed", col = 2)
replicate(n = 1000, {
  k = rnorm(1, mean = 1, sd = .15)
  lines(x = x, y = pred^k, lwd = .05, col = rethinking::col.alpha(1,.5))
})
lines(x = x, y = pred, lwd = 1.5, col = rethinking::col.alpha("red",1),
      lty = "dashed")
# plot(sapply(1:99, fun), type = "l")



