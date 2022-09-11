rm(list=ls())
study=read.csv("student.csv")
attach(study)
study

#importing all the libraries required:
library(ggplot2)
ggplot(study,aes(Hours))+geom_boxplot(fill='sky blue')
ggplot(study,aes(Scores))+geom_boxplot(fill=' pink')
ggplot(study,aes(Hours,Scores))+geom_point(col=2)+
  labs(title="Scatterplot of Scores vs Hours")
set.seed(123)
rsam=sample(x=c('train','test'),size=nrow(study),replace=T, prob= c(0.75,0.25))
train=study[rsam=='train',]
test=study[rsam=='test',]

ggplot(study,aes(Hours,Scores))+geom_point(col=2)+
  labs(title="Scatterplot of Scores vs Hours")+
  geom_smooth(method='lm')
pred_lin <- predict(stu_lin,data.frame(Hours=test$Hours))
ggplot(NULL,aes(x=test$Hours))+
  geom_point(aes(y=test$Scores,color='red'))+
  geom_line(aes(y=pred_lin)) +geom_point(aes(y=pred_lin,color='blue'))+
  labs(x='hours in test data',y='scores')+
  scale_color_discrete(name="data points",
                       labels=c("predicted value","actual value"))

error=test$Scores-pred_lin
r.sq=1- sum(error^2)/sum((test$Scores-mean(test$Scores))^2)
r.sq

hr=9.5
sc=stu_lin$coefficients[[1]] + stu_lin$coefficients[[2]]*hr;sc
