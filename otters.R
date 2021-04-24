library(readxl)
otters <- read_excel("~/Downloads/otters.xlsx")
or.otters<-otters
# View(otters)

lablls<-c("year", "independent", "pup", "total")
colnames(otters)<-lablls
library(psych)
library(ggplot2)
library(reshape2)
library(cowplot)
library(heatmaply)
library(factoextra)
library(readxl)
library(ggfortify)
library(ggpubr)
#### PLOT ####
plot<- ggplot(
  data=otters
)+
  geom_line(
    mapping=aes(x=year, y=pup), color="darkorange3"
  )+
  geom_smooth(
    mapping=aes(x=year, y=pup), method="lm", alpha=0, size=.1, linetype=8, color=1
  )+
  geom_smooth(
    mapping=aes(x=year, y=pup), size=0, linetype=9, color=1, alpha=.15
  )+
  geom_point(
    mapping=aes(x=year, y=pup), size=.6, color="darkorange3"
  )+ 
  
geom_line(
  mapping=aes(x=year, y=independent), color="turquoise4"
)+
geom_smooth(
  mapping=aes(x=year, y=independent), method="lm", alpha=0, size=.1, linetype=8, color=1
)+
  geom_smooth(
    mapping=aes(x=year, y=independent), size=0, linetype=9, color=1, alpha=.15
  )+
  geom_point(
    mapping=aes(x=year, y=independent), size=.6, color="turquoise4"
)+
  
  geom_vline(xintercept = 1989.7)+geom_hline(yintercept = 0)+
  ggtitle("California Sea Otter in the Mainland range")+labs(x="Years", y="Population count")+
  
geom_text(aes(x = 2018, y = 2800, label = "Adults"), size=3) +
geom_text(aes(x = 2018, y = 750, label = "Pups"), size=3) 

##### WEIRD ####
cor<- ggplot(otters)+
  geom_point(mapping=aes(x=pup, y= independent))+
  geom_smooth(mapping=aes(x=pup, y= independent), method= "lm", size = .2, color = "darkred", fill = "darkred", alpha= .05)
PUP<-scale(otters$pup)
INDEPENDENT<-scale(otters$independent)
dens<-ggplot()+
  geom_density(mapping = aes(PUP),fill = "darkorange3", alpha=.3,linetype=6)+
  geom_density(mapping = aes(INDEPENDENT), fill = "turquoise4", alpha = .3,linetype=6)+ xlim(-4,4)

Ootters<- otters
otters$year=NULL

otters<-normalize(otters)

pca<-prcomp(otters)
hh<-ggbiplot(pca, alpha = 0, ellipse = 0, var.axes = 1)
#### STATS ####



list1<-or.otters[,c(1,2,3)]
list1<-normalize(list1)

pc<-prcomp(list1)
fviz_screeplot(pc)
kp<-kmeans(list1, 2)
# size=20,alpha=.2, loadings=TRUE, loadings.label=1,loadings.label.colour=1, loadings.colour="darkred")
z<-autoplot(pc, list1, size=20 ,alpha=1, loadings=TRUE, loadings.label=1,loadings.label.colour=1, loadings.colour="darkred", colour="Year")+theme_classic2()
y<-autoplot(kp,list1, size=80 ,alpha=1, loadings=TRUE, loadings.label=1,loadings.label.colour=1, loadings.colour="darkred", frame=1)+theme_classic2()
plot_grid(z,y)
# cor(otters$independent, otters$pup)

z2<-autoplot(pc, list1, size=20 ,alpha=1, loadings=TRUE, loadings.label=1,loadings.label.colour=1, loadings.colour="darkred", colour="Independents")+theme_classic2()
z3<-autoplot(pc, list1, size=20 ,alpha=1 ,loadings=TRUE, loadings.label=1,loadings.label.colour=1, loadings.colour="darkred", colour="Pups")+theme_classic2()
plot_grid(z,z2,z3)
plot(list1$Independents,list1$Pups)