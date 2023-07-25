# 반복측정분산분석 예)

id<-c(1,2,3,4,5,6) 
pre<-c(45,42,36,39,51,44) 
after3m<-c(50,42,41,35,55,49) 
after6m<-c(55,45,43,40,59,56) 

d<-data.frame(id,pre,after3m,after6m) 

library(tidyr)
d %>% pivot_longer(cols=2:4, names_to="time") -> d.m
head(d.m)
d.m$id <- factor(d.m$id)
d.m$time <- factor(d.m$time, 
                   levels=c("pre","after3m","after6m"))

library(ggplot2)
ggplot(data=d.m,aes(x=time,y=value))+
  geom_line(aes(group=id,col=id))+
  geom_point(aes(col=id))+theme_classic()

ds<-d.m%>% 
  group_by(time)%>% 
  summarise(mean=mean(value),sd=sd(value)) 

ggplot(ds,aes(x=time,y=mean))+
  geom_point()+
  geom_line(group=1)+theme_classic()

fit<-aov(value ~ time+Error(id/time),data=d.m) 
summary(fit)

with(d.m, 
     pairwise.t.test(value,time,paired=T,p.adjust.method="bonferroni"))
