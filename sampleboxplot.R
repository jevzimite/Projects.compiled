breeds<- c("german_sh", "labrador","poodle", "bulldog")
freq<- c(20,33,15,7)
table<- cbind(breeds, freq)
dentry<- data.frame(table)

library(ggplot2)
ggplot(data=dentry)+
  geom_smooth(mapping=aes(x=breeds, y=freq))
