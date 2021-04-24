weight<-c(65,65,65,97,76,84,74,71,82,85,87,80,77,82,85,83,83,84,76,78,77)
dframe<-data.frame(weight)

library(ggplot2)
library(ggrepel)
library(psych)

summary(weight)
describe(weight)

ggplot(
  data=dframe
)+
  geom_histogram(binwidth=7, aes(x=weight,y= ..density..), fill = "darkorange3", alpha=0.9)+
  geom_density(mapping=aes(x=weight), fill = "turquoise3", alpha=.1, color ="turquoise4")+
  geom_vline(xintercept = 50)+
  geom_vline(aes(xintercept = 78.86), color=1,size=.5,linetype=5, alpha=.7)+
  geom_vline(aes(xintercept = 80), color=1,size=.5,linetype=9, alpha=.6)+
  geom_hline(yintercept = 0)+
  labs(x="Weight", y="")



