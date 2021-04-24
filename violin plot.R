# ToothGrowth
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)


ggplot(data=ToothGrowth)+
  geom_violin(mapping=aes(x=dose, y=len), color="turquoise4", fill="turquoise3", alpha=.5)+
  geom_boxplot(mapping=aes(x=dose, y=len),width=.1, color="darkblue", fill="steelblue")+
  geom_jitter(mapping=aes(x=dose, y=len),shape=3, size=1, color="darkblue")