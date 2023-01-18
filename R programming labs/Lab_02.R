str(Ch2_raw_bikeshare_data)
library(data.table)
setDT(Ch2_raw_bikeshare_data)
bikeraw= copy(Ch2_raw_bikeshare_data)
class(bikeraw)
str(bikeraw)

is.na(bikeraw)
table(is.na(bikeraw))

library(data.table)
c1=c('a','b','c')
c2=c('b','a','b')
c3=c('d','c','a')
dt=data.table(c1,c2,c3)

library(stringr)
str_detect(dt,'d')
str_detect(bikeraw,'NA')

bikeraw[is.na(sources)]
bikeraw[is.na(sources), sources]
bikeraw[is.na(sources), NROW(sources)]

bikeraw[grep('[a]', humidity)]
dt[grep('[a]',c1)]

bikeraw[grep('[a-z A-Z]', humidity)]

bikeraw[grep('[a-z A-Z]', humidity), humidity:='61']
bikeraw[grep('[a-z A-Z]', humidity)]

str(bikeraw)

bikeraw[ ,humidity:= as.numeric(humidity)]
str(bikeraw)


data=c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata=factor(data)
fdata

rdata=factor(data, labels = c("Apple", "Melon", "Banana"))
rdata

levels(rdata)=c('I', 'II', 'III')
rdata

str(bikeraw)
unique(bikeraw$holiday)
table(bikeraw$holiday)


bikeraw[ , holiday:=factor(holiday, levels = c(0,1), labels = c('not holiday', 'holiday'))]


bikeraw[ , workingday:=factor(workingday, levels = c(0,1), labels = c('not workingday', 'workingday'))]


bikeraw[ , season:=factor(season, levels = c(1,2,3,4), labels = c('spring', 'summer', 'fall', 'winter'), ordered = T)]



bikeraw[ , weather:=factor(weather, levels = c(1,2,3,4), labels = c('clr_part_cloud', 'mist_cloudy_snow', 'lt_rain_snow', 'hvy_rain_snow'), ordered = T)]
  
bikeraw[,.(datetime)]

dt=c(as.Date('2018-01-01'),as.Date('2018-03-03'),as.Date('2018-05-01'),as.Date('2018-05-31'))
dt
price=c(4,8,10,12)
sampdt = data.table(dt ,price)
str(sampdt)

bikeraw[,datetime:=as.Date(datetime,'%m/%d/%Y %H:%M')]
str(bikeraw)

unique(bikeraw$sources)

bikeraw[,sources:=tolower(sources)]
bikeraw[,sources:=trimws(sources)]
bikeraw[is.na(sources), sources:='unkown']

bikeraw[grep('www.[a-z]*.[a-z]*', sources),sources:='web']

bikeraw[,sources:=factor(sources, labels = c('ad campaign', 'web', 'twitter', 'facebook page', 'unknown', 'direct', 'blog'))]

str(bikeraw)


