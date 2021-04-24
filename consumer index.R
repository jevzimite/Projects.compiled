# 
library(readxl)
basket <- read_excel("~/Downloads/basket.xlsx")
View(basket)

cln<-c("bathroom","shampoo","deodorant", "toothpaste")
colnames(basket)<- cln
rwn<-c("April","May", "June", "July", "August")
rownames(basket)<- rwn

one2five<-c(1,2,3,4,5)

basket
# months<-c("Apr-20",	"May-20",	"Jun-20",	"Jul-20",	"Aug-20")


library(ggplot2)
ggplot(data=basket)+

  geom_jitter(mapping=aes(x=rwn, y=bathroom), color="darkorange3")+
  geom_smooth(mapping=aes(x=one2five, y=bathroom),method="lm", alpha=0, size=.3, linetype=5, color="darkorange3")+

  
  geom_jitter(mapping=aes(x=one2five, y=shampoo), color="firebrick4")+
  geom_smooth(mapping=aes(x=one2five, y=shampoo),method="lm", alpha=0, size=.3, linetype=5, color="firebrick4")+

  geom_jitter(mapping=aes(x=one2five, y=deodorant), color="deepskyblue4")+
  geom_smooth(mapping=aes(x=one2five, y=deodorant),method="lm", alpha=0, size=.3, linetype=5, color="deepskyblue4")+

  geom_jitter(mapping=aes(x=one2five, y=toothpaste), color="palegreen4")+
  geom_smooth(mapping=aes(x=one2five, y=toothpaste),method="lm", alpha=0, size=.3, linetype=5, , color="palegreen4")+

  geom_vline(xintercept = 0)+ geom_hline(yintercept = 0)+
  
  
  ggtitle("Price changes across April-August (COVID)")+
  labs(x="Months", y="Price ($)")+
  theme(panel.background = element_rect(fill = 'white', colour = 1))
