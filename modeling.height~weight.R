height = truncnorm::rtruncnorm(1e4, 40, 220, 167, (220-40)/4)
formula = scales::rescale((height * .5)**2 , to = c(min(height*.5),
                                          max(height*.5)))
weight = formula+rnorm(1e4, 0, 10)

lm = lm(height~sqrt(weight))
pred = predict(lm, data.frame(weight))

plot(weight, height, col = scales::alpha(1,.1), pch= 20, cex = 1.3)
points(weight, pred, col = 2, cex = .1)


