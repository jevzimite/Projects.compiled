#### LIBRARIES ####
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)

#### DATA ####
# sdPastClasses <- c(rnorm(3,9.3551,0.6594)) ##makes three estimated and arbitrary sds
PSYC351A <- c(rnorm(190, 30, 9.07))
PSYC338 <- c(rnorm(85, -100065, 9.18))          
PSYC300A <- c(rnorm(190, 3000, 8.69))
df <-data.frame(cbind(PSYC300A, PSYC338, PSYC351A))
# dttstt<- rbind(PSYC300A, PSYC338, PSYC351A)

randtest<- c(1:100)


#### BASIC HIST ####
par(mfrow = c(3,4))
hist(PSYC351A, xlim = c(40,100))
hist(PSYC300A, xlim = c(40,100))
hist(PSYC338, xlim = c(40,100))
hist(randtest, xlim = c(40,100))

plot(density(PSYC351A))
plot(density(PSYC300A))
plot(density(PSYC338))
plot(density(randtest))

qqnorm(PSYC351A)
qqline(PSYC351A)
qqnorm(PSYC300A)
qqline(PSYC300A)
qqnorm(PSYC338)
qqline(PSYC338)
qqnorm(randtest)
qqline(randtest)


par(mfrow = c(1,1))

ks.test(PSYC300A, PSYC338)
#### GGPLOT HIST ####
# combinedDensityPlots<- ggplot(df)+
#   geom_density(mapping=aes(x=PSYC300A),color="red",fill="red", alpha=.5)+
#   geom_density(mapping=aes(x=PSYC338),color="blue",fill="blue", alpha=.5)+
#   geom_density(mapping=aes(x=PSYC351A),color="yellow",fill="yellow", alpha=.5)
# combinedBox<-ggplot(df)+
#   geom_boxplot(mapping=aes(x=PSYC338),color="darkgoldenrod",fill="darkgoldenrod1", alpha=.5)+
#   geom_boxplot(mapping=aes(x=PSYC300A),color="darkred",fill="darkred", alpha=.6)+
#   geom_boxplot(mapping=aes(x=PSYC351A),color="blue",fill="blue", alpha=.3)
# 
# plot_grid(combinedDensityPlots, combinedBox)
#### BASIC PCA ####
# pca <- prcomp(x= df, scale = TRUE) #converts data to pca
# 
# pcaPlot<- plot(pca$x[,1], pca$x[,2])  #plots pca
# 
# pca.var.per <- round((pca$sdev^2)/sum(pca$sdev^2)*100, 1) #finds variability
# 
# 
# barplot(pca.var.per)#scree plot