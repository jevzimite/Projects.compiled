library(ggbiplot) #function needed to graph prcomp like files
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)


df <- iris #sample data set

df.pca <- as.data.frame(iris) 
df.pca$Species <- NULL
##fixes the df

df_pca1 <- prcomp(df.pca) #transforms into prcomp format

# df_pca2<- as.data.frame(df_pca1$x) #useless


p1<-ggbiplot(df_pca1, 
         ellipse = TRUE, #makes circles around the main groups
         # circle = TRUE, #adds a circle in the middle of the data
         var.axes = TRUE, #removes arrows IF FALSE
         groups=df$Species, #makes the groups based on the "species"
         labels = colnames(df_pca1) #should label the groups but doesnt do anything 
)+
         theme_minimal()+
         ggtitle("Example of PCA")+
         theme(legend.position = "bottom")
      



# par(mfrow = c(2,3))
# plot(df_pca1$x[,1], df_pca1$x[,2])  #plots pca
# plot(df_pca1$x[,1], df_pca1$x[,3])  #plots pca
# plot(df_pca1$x[,1], df_pca1$x[,4])  #plots pca
# 
# plot(df_pca1$x[,2], df_pca1$x[,3])  #plots pca
# plot(df_pca1$x[,2], df_pca1$x[,4])  #plots pca
# 
# plot(df_pca1$x[,3], df_pca1$x[,4])  #plots pca
# 

par(mfrow = c(1,1))
pca.var.per <- round((df_pca1$sdev^2)/sum(df_pca1$sdev^2)*100, 1) #finds variability
# barplot(pca.var.per)#scree 


# corr<-ggplot(data=df.pca)+
#   geom_line(mapping=aes(x= Petal.Length, y=Petal.Width), size = .5, color = "steelblue" )
# nocorr<-ggplot(data=df.pca)+
#   geom_smooth(mapping=aes(x= Sepal.Length, y=Sepal.Width), method = "lm", size = .5, color = "darkred")
# smlcorr<-ggplot(data=df.pca)+
#   geom_smooth(mapping=aes(x= Petal.Length, y=Sepal.Width), method = "lm", size = .5, color = "gold")


# plot_grid(corr,nocorr,smlcorr,p1)


# p1
k1<-kmeans(df.pca,3,iter.max = 25)

 

p2<-fviz_cluster(k1, data = df.pca)

p3<-fviz_screeplot(df_pca1)

plot_grid(p3,p1,p2)



