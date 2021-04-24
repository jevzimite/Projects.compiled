df<-iris
df[,5]= NULL

k<-kmeans(df, 3) 
 
Cluster<-k$cluster

df2<- iris
cbind(df2, Cluster)