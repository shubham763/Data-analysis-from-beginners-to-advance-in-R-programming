library(data.table)
library(ggplot2)

advert=copy(Ch4_marketing)
setDT(advert)
str(advert)
grep('NA',advert)

advert[,.(summary(google_adwords),summary(facebook),summary(twitter))]
advert[,(summary(twitter))]
g=advert[,google_adwords]
f=advert[,facebook]
tw=advert[,twitter]
nv=c(g,f,tw)
View(nv)

nc=c(rep('g',NROW(g)),rep('f',NROW(f)),rep('tw',NROW(tw)))
advert2=data.table(nv,nc)

ggplot(advert2,aes(x=nc,y=nv))+geom_boxplot(fill='pink') 

pairs(advert)

ggplot(advert,aes(x=marketing_total,y=google_adwords))+geom_point(color='blue')
ggplot(advert,aes(x=revenues,y=marketing_total))+geom_point(color='purple')


model1=lm(revenues~marketing_total,data=advert)
model

ggplot(advert,aes(x=revenues,y=marketing_total))+geom_point(color='purple')+geom_smooth(method='lm')

str(model1)
model1$residuals

resdf=data.table('res'=model1$residuals)
ggplot(resdf,aes(x=res))+geom_histogram(bins=10,fill='purple',color='black')

mean(model1$residuals)
sd(model1$residuals)

ggplot(resdf,aes(sample=res))+stat_qq(color='blue')+stat_qq_line()

summary(model1)

summary(advert$marketing_total)                                                                                                                                                                              

advert[marketing_total>430,marketing_total]
 
newrev=data.table(marketing_total=seq(460,470,5))
predict.lm(model1,newrev,interval = 'predict')
predict.lm(model1,newrev,level = .99,interval = 'predict')

vv=5:15
set.seed(7)
sample(vv,5)

liladvert=advert[sample(.N,.3*.N)]

set.seed(4510)
liladvert=advert[sample(.N,.3*.N)]
samp_model=lm(revenues~marketing_total,data = liladvert)
samp_model
confint(samp_model)
 
x=1:10
y=c(1 ,1.41 ,1.73 ,2 ,2.24 ,2.45 ,2.65 ,2.83 ,3 ,3.16)
fit = lm(y~x)
sampdt = data.table(x,y)
ggplot(sampdt ,aes(x=x,y=y)) + 
  geom_point(color = 'purple') + 
  geom_smooth(method = "lm") + labs(title="Linearity?")

sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + 
labs(title= "Normality?")

ggplot(sampdt,aes(sample=res))+stat_qq(color='blue')+stat_qq_line()

sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + 
  geom_point(color='purple') + geom_smooth(method = 'lm') +
  labs(title= "Equal Variance?")

y2 = y^2
fit2=lm(y2~x)
sampdt2 = data.table(x,y2)
sampdt2[,res:=fit2$residuals]
sampdt2[,pred:=fit2$fitted.values]

ggplot(sampdt2 ,aes(x=x,y=y2)) + 
  geom_point(color = 'purple') + 
  geom_smooth(method = "lm") + labs(title="Linearity?")

ggplot(sampdt2 ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + 
  labs(title= "Normality?")

ggplot(sampdt2,aes(sample=res))+stat_qq(color='blue')+stat_qq_line()

ggplot(sampdt2 ,aes(x=pred ,y=res)) + 
  geom_point(color='purple') + geom_smooth(method = 'lm') +
  labs(title= "Equal Variance?")


model2 = lm(revenues ~ google_adwords + facebook + twitter , data=advert)
plot(advert)

resdf2=data.table(res=model2$residuals,pred=model2$fitted.values) 


ggplot(resdf2 ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + 
  labs(title= "Normality?")

ggplot(resdf2,aes(sample=res))+stat_qq(color='blue')+stat_qq_line()

ggplot(resdf2 ,aes(x=pred ,y=res)) + 
  geom_point(color='purple') + geom_smooth(method = 'lm') +
  labs(title= "Equal Variance?")

summary(model2)

qf(.95,df1=3,df2=168)
