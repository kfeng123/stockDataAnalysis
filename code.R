library(readr)
library(plyr)
myFileName=dir("stock data")
data=list()
for(i in myFileName){
    temp=read_csv(paste0("stock data/",i))
    data=c(data,list(temp[,c(2,6)]))
}

date=NULL
for(i in data){
   date=union(date,as.character(i[,1]))
}
date=as.Date(date)
date=sort(date)
date=as.character(date)
date=data.frame(date=date)
myList=llply(data,function(x){
    x[,1]=as.character(x[,1])
    merge(date,x,all.x=TRUE)$close
})

temp=unlist(myList)
dim(temp)=c(nrow(date),length(temp)/nrow(date))

