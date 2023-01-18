library(data.table)
setDT(titanic)
library(ggplot2)
str(titanic)

unique(titanic$Survived)
unique(titanic$Pclass)

titanic[,Survived:=factor(Survived)]
titanic[,Pcalss:=factor(Pclass,ordered=T)]
titanic[,Sex:=factor(Sex)]

ggplot(titanic, aes(x=Survived))+geom_bar()
ggplot(titanic, aes(x=Survived))+geom_bar()+theme_bw()

ggplot(titanic, aes(x=Survived))+geom_bar()+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Sex))+geom_bar()+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Sex, fill=Survived))+geom_bar()+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Pclass, fill=Survived))+geom_bar()+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')
 
ggplot(titanic, aes(x=Sex, fill=Survived))+geom_bar()+facet_wrap(~Pclass)+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')
 
ggplot(titanic, aes(x=Age))+geom_histogram(binwidth = 5,fill='yellow',color='black')+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Age, fill=Survived))+geom_histogram(binwidth = 5,color='black')+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Age, fill=Survived))+geom_histogram(binwidth = 5,color='black')+facet_wrap(~Pclass)+theme_bw()+
  labs(y='Passenger Count',title='Titanic Data')

ggplot(titanic, aes(x=Survived, y=Age))+geom_boxplot()+facet_wrap(~Pclass)+theme_bw()+
  labs(y='Age',title='Titanic Data')

ggplot(titanic, aes(x=Survived, y=Age))+geom_boxplot()+facet_wrap(Sex~Pclass)+theme_bw()+
  labs(y='Age',title='Titanic Data')

ggplot(titanic, aes(x=Age, fill=Survived))+geom_density(alpha=0.5)+facet_wrap(Sex~Pclass)+theme_bw()+
  labs(y='Age',title='Titanic Data')

ggplot(titanic, aes(x=Age, fill=Survived))+geom_histogram(binwidth = 5,color='black')+facet_wrap(Sex~Pclass)+theme_bw()+
  labs(y='Age',title='Titanic Data')
