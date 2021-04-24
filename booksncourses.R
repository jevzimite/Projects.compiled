n_books<- c(3,3,5,4,4,6,3,3,2,5,3,4,2,3,1,6,5,2,3,5,6,4,4,2,3,2,1,4,3,3,5,4,4,6,5,6,2,4,2,5,5,5,5,4,6,1,2,2,1,6,5,4,4,1,4,4,6,1,5,2,3,4,4,4,3,2,4,3,6,4,1,6,1,1,6,1,3,1,3,4,1,3,2,5,5,2,5,2,6,2,2,4,3,5,2)
n_pencils<- c(3,3,10,4,4,6,3,6,4,10,6,4,4,6,1,12,10,2,6,10,12,8,4,2,6,2,2,8,3,3,10,4,4,6,5,6,4,4,4,10,5,5,5,4,12,2,4,2,1,12,5,8,8,1,4,8,6,2,5,2,3,8,8,4,6,4,8,6,12,4,2,6,1,1,12,1,3,1,6,4,2,3,4,5,10,2,5,4,12,4,4,4,3,5,2)
n_courses<- c(3,2,5,4,2,3,3,2,1,5,3,2,2,1,1,6,3,1,3,5,3,1,4,2,2,1,0,2,1,2,2,4,2,6,3,6,1,1,2,5,3,5,2,1,2,0,2,1,0,2,5,2,1,1,2,2,2,1,5,1,2,2,1,4,2,1,4,2,2,2,1,3,1,1,3,1,2,1,2,1,1,2,2,5,5,1,3,1,2,2,1,2,2,2,2)

library(ggplot2)

table<-cbind(n_books,n_pencils,n_courses)
dframe<- data.frame(table)

ggplot(data=dframe)+
  geom_smooth(mapping=aes(x=n_courses, y= n_books), alpha=.1, size=.4, color ="steelblue")+
  geom_jitter(mapping=aes(x=n_courses, y= n_books), size= .3, alpha=.6)+
  geom_point(mapping=aes(x=n_courses, y= n_books), size= .7, color="darkblue")+
  geom_smooth(mapping=aes(x=n_courses, y= n_books),method= "lm", alpha=0, size=.3, linetype="dashed", color ="darkblue")+
  ggtitle("Students resources relative to amount of courses they are taking")+
  labs(x="Number of courses", y="Number of books")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(panel.background = element_rect(fill = 'white', colour = 1))



  # geom_abline(slope=1,intercept = 0)+
  # geom_abline(slope=-1,intercept = 0)

summary(dframe)