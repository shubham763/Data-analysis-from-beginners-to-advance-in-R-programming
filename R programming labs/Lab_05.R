library(data.table)
bike=copy(Ch5_bike_station_locations)
setDT(bike)
str(bike)

grep('NA', bike)

library(ggplot2)
ggplot(bike,aes(x=longitude,y=latitude))+geom_point()

set.seed(100)
k3=kmeans(bike,3)
k3

bike[,clus3:=k3$cluster]
bike

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) +geom_point()

str(bike)
bike[,clus3:=factor(clus3)]
ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) +geom_point()

k3$centers
str(k3$centers)
class(k3$centers)

centdt3=data.table(k3$centers)
centdt3
str(centdt3)
class(centdt3)

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) +geom_point() +geom_point(data=centdt3,aes(x=longitude,y=latitude),color="purple",shape=11,size=2)


set.seed(100)
k2=kmeans(bike[,.(latitude,longitude)],2)
k2
bike[,clus2:=k2$cluster]
bike[,clus2:=factor(clus2)]
centdt2=data.table(k2$centers)
ggplot(bike,aes(x=longitude,y=latitude,color=clus2)) +geom_point() +geom_point(data=centdt2,aes(x=longitude,y=latitude),color="purple",shape=11,size=2)

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) +geom_point() +geom_point(data=centdt3,aes(x=longitude,y=latitude),color="purple",shape=11,size=2)+geom_point(data=centdt2,aes(x=longitude,y=latitude),color="black",shape=19,size=2)

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) +geom_point() +geom_point(data=centdt3,aes(x=longitude,y=latitude),color="purple",shape=11,size=2)+geom_point(data=centdt2,aes(x=longitude,y=latitude),color="black",shape=19,size=2)+facet_wrap(~clus2)


library(geosphere)

dd=distm(c(40.777250,-73.872610),c(40.6895,-74.1745),fun = distHaversine)
dd
class(dd)

dd=distm(c(40.777250,-73.872610),c(40.6895,-74.1745),fun = distHaversine)/1609
dd

res_matrix3=distm(bike[,.(latitude,longitude)],centdt3,fun=distHaversine)/1609
res_matrix3

#lab 5 part b
head(res_matrix3)
bike[,c('k31','k32','k33'):=as.data.table(res_matrix3)]
bike

bike[clus3==1,c3dist:=k31]
bike[clus3==2,c3dist:=k32]
bike[clus3==3,c3dist:=k33]

res_matrix2=distm(bike[,.(latitude,longitude)],centdt2,fun=distHaversine)/1609
bike[,c('k21','k22'):=as.data.table(res_matrix2)]
bike

bike[clus2==1,c2dist:=k21]
bike[clus2==2,c2dist:=k22]
bike

bike[,c('k21','k22'):=NULL]
bike

StaDist=bike[,.(clus3,k31,k32,k33)]

StaDist[,c('nok1','nok2','nok3'):=pmin(k31,k32,k33)]

StaDist[,nok1:=pmin(k32,k33)]
StaDist[,nok2:=pmin(k31,k33)]
StaDist[,nok3:=pmin(k31,k32)]
StaDist[,.(mean(nok1),mean(nok2),mean(nok3))]

StaDist[,allk:=pmin(k31,k32,k33)]
StaDist[,.(mean(nok1),mean(nok2),mean(nok3),mean(allk))]

stadist=c(StaDist[,nok1],StaDist[,nok2],StaDist[,nok3])
kn=StaDist[,.N]
stanm=c(rep('nok1',kn),rep('nok2',kn),rep('nok3',kn))

StaPlot=data.table(stadist,stanm)
ggplot(StaPlot,aes(x=stanm,y=stadist))+geom_boxplot()

StaDist[,summary(.SD),.SDcols=c('nok1','nok2','nok3')]
bike[,hdist:=StaDist$nok1]
bike

bike[,summary(.SD),.SDcols=c('c3dist','c2dist','hdist')]

clusdist=c(bike[,c3dist],bike[,c2dist],bike[,hdist])
d=bike[,.N]
clusnm=c(rep('3 kiosks',d),rep('2 kiosks',d),rep('Hybri kiosks',d))
clusplot=data.table(clusdist,clusnm)
ggplot(clusplot,aes(x=clusdist))+geom_histogram(color='white',fill='purple')+facet_wrap(~clusnm)
ggplot(clusplot,aes(x=clusdist))+geom_histogram(breaks=seq(0,4,.5),color='white',fill='purple')+scale_x_continuous(breaks=seq(0,4,.5))+facet_wrap(~clusnm)

bike[,distI:=hdist-c3dist]
ggplot(bike,aes(x=distI))+geom_histogram(breaks=seq(-3,2.5,.5),color='white',fill='purple')+scale_x_continuous(breaks = seq(-3,2.5,.5))


######## hierarchy ############


marketing=Ch5_age_income_data
setDT(marketing)
grep('NA',marketing)

str(marketing)
unique(marketing$bin)

marketing[,bin:=factor(bin,ordered=T)]
str(marketing)

ggplot(marketing,aes(x=bin,y=age))+geom_boxplot(fill='pink')
ggplot(marketing,aes(x=bin,y=income))+geom_boxplot(fill='yellow')

mk3=kmeans(marketing [,.(age ,income)],3)
mk3plot = data.table(age=marketing$age ,income=marketing$income ,cluster=mk3$cluster)
mk3plot[,cluster := factor(cluster)]
ggplot(mk3plot ,aes(x=age ,y=income ,color=cluster)) +geom_point()

#### Lab 5 d ######

marketing[,age_s:=scale(age)]
marketing[,income_s:=scale(income)]
marketing
mk3=kmeans(marketing [,.(age_s ,income_s)],3)
mk3plot = data.table(age=marketing$age_s ,income=marketing$income_s ,cluster=mk3$cluster)
mk3plot[,cluster := factor(cluster)]
ggplot(mk3plot ,aes(x=age ,y=income ,color=cluster)) +geom_point()


hc=hclust(dist(marketing[,.(income_s,age_s)]),method = 'ward.D2')
hc

plot(hc)
  
library(dendextend)
dend=as.dendrogram(hc)
dend_six_color=color_branches(dend,k=6)
plot(dend_six_color,leaflab='none')

mk3=kmeans(marketing[,.(age_s,income_s)],3)
mk3$tot.withinss


mk1=kmeans(marketing[,.(age_s,income_s)],1)
mk2=kmeans(marketing[,.(age_s,income_s)],2)
mk3=kmeans(marketing[,.(age_s,income_s)],3)
mk4=kmeans(marketing[,.(age_s,income_s)],4)
mk5=kmeans(marketing[,.(age_s,income_s)],5)
mk6=kmeans(marketing[,.(age_s,income_s)],6)
mk7=kmeans(marketing[,.(age_s,income_s)],7)
mk8=kmeans(marketing[,.(age_s,income_s)],8)
mk9=kmeans(marketing[,.(age_s,income_s)],9)
mk10=kmeans(marketing[,.(age_s,income_s)],10)

wss=c()

wss[1]=mk2$tot.withinss
wss[2]=mk3$tot.withinss
wss[3]=mk4$tot.withinss
wss[4]=mk5$tot.withinss
wss[5]=mk6$tot.withinss
wss[6]=mk7$tot.withinss
wss[7]=mk8$tot.withinss
wss[8]=mk9$tot.withinss
wss[9]=mk10$tot.withinss

elbow=data.table(k=2:10,wss)

ggplot(elbow,aes(x=k,y=wss))+geom_line(color='blue')


### or you can use for loop

wss= c()
for (i in 2:10) {
  mk2=kmeans(marketing[,.(age_s,income_s)],2)
  wss[i-1]=mk2$tot.withinss
}

ggplot(elbow,aes(x=k,y=wss))+geom_line(color='blue')


marketing[,k5:=mk5$cluster]
marketing[,k6:=mk6$cluster]

marketing[,h5:=cutree(dend,k=5)]
marketing[,h6:=cutree(dend,k=6)]

marketing[,k5:=factor(k5)]
marketing[,k6:=factor(k6)]
marketing[,h5:=factor(h5)]
marketing[,h6:=factor(h6)]

ggplot(marketing,aes(x=age,y=income,color=k5))+geom_point()
ggplot(marketing,aes(x=age,y=income,color=k6))+geom_point()
ggplot(marketing,aes(x=age,y=income,color=h5))+geom_point()
ggplot(marketing,aes(x=age,y=income,color=h6))+geom_point()