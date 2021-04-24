beaver11<-beaver1[-c(101:114), ]
beaver11
identical(beaver1,beaver11)

# if false then

df<-data.frame(beaver11,beaver2)

library(ggplot2)

ggplot(df) +
  geom_line(aes(y = temp, x=time), color = "darkred") +
  geom_point(aes(y = temp, x=time), color = "darkred") +
  geom_line(aes(y = temp.1, x=time.1), color="steelblue") +
  geom_smooth(aes(y = temp, x=time), method = "lm" , color = "black",size=.3, linetype=3, alpha=.15) +
  geom_smooth(aes(y = temp.1, x=time.1), method="lm", color="black",size=.3, linetype=3, alpha=.1)+
  geom_vline(xintercept = 0)+ geom_hline(yintercept = 36.25)+
  theme(panel.background = element_rect(fill = 'white', colour = '1'))


