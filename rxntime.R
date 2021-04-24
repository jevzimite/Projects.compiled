trial_n<-1:20
rxn_t<-c(379,243,268,259,262,255,243,236,282,316,318,328,267,338,328,286,268,295,256,245)
rxn_c<-c(345.225,305.02,293.255,289.392,292.833,291.039,290.853,291.657,293.667,287.657,295.255,293.422,287.402,292.99,291.108,295.245,283.941,286.539,292.049,290.833)
table<-cbind(trial_n,rxn_t,rxn_c)
dframe<-data.frame(table)
# dframe
library(ggplot2)


# ggplot(data=dframe, aes(x=trial_n, y=rxn_t)) +
#  geom_line(linetype="dashed")+
#  geom_point(shape=5)+
#  labs(title="Reaction time of Emilio",x="Trials", y = "Time")
# 

ggplot(dframe, aes(x=trial_n)) +
  geom_line(aes(y = rxn_t), color = "darkred") +
  geom_line(aes(y = rxn_c), color="steelblue") +
  ggtitle("Reaction Time")+
  labs(x="Trials", y="Time (ms)") +
 geom_smooth(aes(y=rxn_c),method = "lm", color="steelblue", size=.3, alpha=0, linetype=5)+
  geom_smooth(aes(y=rxn_t),method = "lm", color="darkred", size=.3, alpha=0, linetype=5 )


