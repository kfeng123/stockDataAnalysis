data1=read.csv("myData.csv")
date=read.csv("myDate.csv",stringsAsFactors = FALSE)
data2=data1[2:nrow(data1),]/data1[1:(nrow(data1)-1),]
data2=rbind(1,data2)
D=data2
D=log(data2)

theMissing=colMeans(is.na(D))
D=D[,theMissing==0]



theWeekDay=format(as.Date(date[,1]),"%u")
X1=D[theWeekDay==1,]
X2=D[theWeekDay!=1,]
X1=as.matrix(X1)
X2=as.matrix(X2)
p=ncol(X1)
rmax=10
n1=nrow(X1)
n2=nrow(X2)
source("./stat.R")
source("./permutationTest.R")
JJJ=myPermutation(X1,X2,n1=nrow(X1),n2=nrow(X2),B=1000,rmax=10)
