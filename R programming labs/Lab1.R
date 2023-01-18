x=5
x<-5
class(x)
class(Ch1_bike_sharing_data)
class(Ch1_bike)
bike = Ch1_bike_sharing_data
str(bike)

library(data.table)

setDT(bike)
class(bike)


bike$season
unique(bike$season)
bike[season==4]
bike[season==4 |season==2]

bike[season %in% c(4,1)]
f=c(4,1)
bike[season %in% f]

bike[season==4 & weather==1]
bike[season!=4 & weather==1]

nowinter = bike[season!=4 & weather==1]

bike[,.(datetime, windspeed)]
bike[,windspeed]
bike[,.(windspeed)]

bike[,mean(temp)]
bike[season==4, mean(temp)]

bike[,.(mean(temp), sd(temp), min(temp), max(temp))]

bb=bike[,.(mean(temp), sd(temp), min(temp), max(temp))]

bb 

names(bb)=c('Mean', 'SD', 'Min', 'Max')

bike[,.(mean(temp), sd(temp), min(temp), max(temp)), by=season]

bike[,.(mean(temp), sd(temp), min(temp), max(temp)), by=.(season,weather)]

bws=bike[,.(mean(temp), sd(temp), min(temp), max(temp)), by=.(season,weather)]
names(bws)=c('Season', 'Weather', 'Mean', 'SD', 'Min', 'Max')
bws=bws[order(Season,Weather)]

bike[,avgTemp:=mean(temp)]
bike[,SeasonAvgTemp:=mean(temp),by=season]
head(bike,10)
tail(bike,10)
bike[season==4]


ticker=c(rep('a' ,5),rep('b' ,5))
oi = c(seq(2,10,2),seq(5,25,5))
dfexp=data.table(ticker ,oi)

dfexp[, c('oi2', 'oi3', 'oi4'):=shift(oi, n = 1:3)]