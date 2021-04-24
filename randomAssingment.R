c1 = 1:3 #condition1
c2 = 4:6 #condition2

tab = gtools::permutations(n = 6, r = 6, v = c(c1,c2), repeats.allowed = F) #permutations
d = (rowMeans(tab[,1:3])-rowMeans(tab[,4:6])) #differences

par(mfrow=1:2)
hist(d, breaks=seq(-3,3, length.out = 11), #imitating the histogram from your notes
     density = 80, col = rainbow(12, start = .01, end = .1)) 
plot(density(d)) #probability distribution function

length(d[d>=3])/length(d) #gives p value
