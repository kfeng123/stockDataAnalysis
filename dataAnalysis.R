library(readr)
library(plyr)
data=read_csv("myData.csv")
date=read_csv("myDate.csv")
theWeekDay=format(date[,1],"%u")
X1=data[theWeekDay==1,]
X2=data[theWeekDay!=1,]
