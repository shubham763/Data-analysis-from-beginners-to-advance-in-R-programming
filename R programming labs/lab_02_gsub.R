library(data.table)
a = c('4x','6a','234aax','abc56')
b = c('rty1a','cc6b','2h3g4','abc56')
dt = data.table(a,b)

dt[,c:=gsub('[a-z]','',a)]
dt
dt[,d:=gsub('[a-z]','',b)]
dt
dt[,e:=gsub('[1-9]','',a)]
dt[,f:=gsub('[1-9]','',b)]
View(dt)
