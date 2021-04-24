j1 = c(1,2,3,1,8,1,5)
j2 = c(7,4,1,3,1,9,3)
j3 = c(10,9,5,9,10,3,10)

#### f test ####
nJ = 3
n = length(j1)

dfG = nJ - 1 
dfE = length(j1)+length(j2)+length(j3) - nJ
grandMean = mean(c(j1,j2,j3))

ssT = sum( (c(j1,j2,j3)-grandMean)^2 )
ssG = ((mean(j1)-grandMean)^2)*n  +  ((mean(j2)-grandMean)^2)*n  +  ((mean(j3)-grandMean)^2)*n
ssE = sum((j1 - mean(j1))^2) + sum((j2 - mean(j2))^2) + sum((j3 - mean(j3))^2)
(ssE + ssG) == ssT

msG = ssG/dfG
msE = ssE/dfE
fStatistic = msG/msE

netaSquared = ssG/ssT
netaSquared = (fStatistic*dfG)/(fStatistic*dfG+dfE)
omegaSquared = (ssG - (nJ-1)*msE)/(ssT+msE)
effectSize = sqrt(netaSquared/(1-netaSquared))

betweenGroups = c(SS = ssG, df = dfG, MS = msG)
withinGroups = c(SS = ssE, df = dfE, MS = msE)
total = c(SS = ssT, df = (dfG + dfE))
stats = c(Fobs = fStatistic, netaSquared = netaSquared, omegaSquared = omegaSquared, EffectSize = effectSize)

results = list(betweenGroups=betweenGroups, withinGroups=withinGroups, total=total, stats=stats)

#### plots ####
par(mfrow=c(1,2))
plot(NULL, 
     xlim = c(-1,13),
     ylim = c(0,.2),
     main = "Distribution of j groups",
     xlab= "Values",
     ylab= "Density")
lines(density(j1), lwd = 2, col = 2)
lines(density(j2), lwd = 2, col = 3)
lines(density(j3), lwd = 2, col = 4)

plot(NULL, 
     xlim = c(0,(fStatistic+2)),
     ylim = c(0, 1),
     main = "F-Distribution",
     xlab= "Probability",
     ylab= "Density")
curve(df(x, df1 = dfG, df2 = dfE), from = 0, to = (fStatistic+2), n = 5000, col= 'red', lwd=1, add = T)
lines(x = c(fStatistic,fStatistic), y = c(-1,2), lty = "dashed")
lines(x = c(-10,10), y = c(0,0), lwd = .5, lty = "solid")
df(fStatistic, df1 = dfG, df2 = dfE)

round(results$stats,2)
