library(data.table)
mark=copy(Ch3_marketing)
setDT(mark)
str(mark)

unique(mark$pop_density)
mark[,pop_density:=factor(pop_density,levels = c('Low','Medium','High'),ordered=T)]
str(mark)

grep('NA',mark)

# a=c(1,2,NA)
# b=c(3,NA,4)
# dd=data.table(a,b)
# grep('NA',dd)

str(mark)
summary(mark$google_adwords)
mark[,summary(google_adwords)]
mark[,summary(pop_density)]
mark[,fivenum(google_adwords)]

library(ggplot2)
ggplot(mark,aes(x=pop_density))+geom_bar()
ggplot(mark,aes(x=pop_density))+geom_bar(fill='blue',color='red')

ggplot(mark,aes(y=google_adwords))+geom_boxplot(fill='blue')

ggplot(mark,aes(x=google_adwords))+geom_histogram()
ggplot(mark,aes(x=google_adwords))+geom_histogram(fill='pink',color='white')
ggplot(mark,aes(x=google_adwords))+geom_histogram(fill='pink',color='white',bins=10)
ggplot(mark,aes(x=google_adwords))+geom_histogram(fill='pink',color='white',binwidth=50)

ggplot(mark,aes(y=twitter))+geom_boxplot(fill='blue')
ggplot(mark,aes(x=twitter))+geom_histogram(fill='pink',color='white',bins=10)

mark[,empFactor:=cut(employees,2)]
table(mark$empFactor,mark$pop_density)

ggplot(mark,aes(x=pop_density,fill=empFactor))+geom_bar()
ggplot(mark,aes(x=pop_density,y=marketing_total))+geom_boxplot(fill='yellow')

ggplot(mark,aes(x=revenues,y=google_adwords))+geom_point(color='purple')

cor(mark$google_adwords,mark$revenues)
mark[,cor(google_adwords,revenues)]

cor.test(mark$google_adwords,mark$revenues)
cor.test(mark$twitter,mark$revenues)
cor.test(mark$facebook,mark$revenues)
mark[,.(cor.test(twitter,revenues),cor.test(facebook,revenues))]

ggplot(mark,aes(x=revenues,y=google_adwords))+geom_point(color='purple')
ggplot(mark,aes(x=revenues,y=twitter))+geom_point(color='purple')
ggplot(mark,aes(x=revenues,y=facebook))+geom_point(color='purple')

pairs(mark)

cor(mark[,1:6])

