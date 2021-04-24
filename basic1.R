iddle = read_csv("xlsm/cattle/idle.csv")
rumination = read_csv("xlsm/cattle/rumination.csv")
grazing = read_csv("xlsm/cattle/grazing.csv")
# n = 1800000
n = 1e4

iddle = c( idl = iddle[1:n,])
grazing = c( grz = grazing[1:n,])
rumination = c( rmn = rumination[1:n,])

df = data.frame(iddle,grazing,rumination)

ggplot()+
  geom_density(mapping = aes(iddle$idl.102), fill = 4 , alpha =.5)+
  geom_density(mapping = aes(grazing$grz.103), fill =2, alpha =.5)+
  geom_density(mapping = aes(rumination$rmn.103), fill=3, alpha =.5)

dfN = scale(df)
k=kmeans(dfN[,2], 3 )
dfC=data.frame(data= k[1], cluster = dfN[,2])
cluster<-fviz_cluster(k, data=dfN)

autoplot(cluster, dfN, loadings=1)
plot(dfC)

one=filter(dfC, cluster == 1)
two=filter(dfC, cluster == 2)
three=filter(dfC, cluster == 3)
precis(one)
length(three)
