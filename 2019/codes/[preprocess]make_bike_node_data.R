# node data 만들기

library(data.table)
library(dplyr)

data <- fread("2017_1.csv")

## 최종데이터와 colname이 다를 수 있음
colnames(data) <- c("bike_no","brow_date","brow_no","brow_name","set_no","return_date","return_no",
                    "return_name","return_set_no","time","dist")

## loc_num 없는 애들 제외
data<-data[!data$brow_name=='중랑센터',]
data<-data[!data$brow_name=='위트콤',]
data<-data[!data$brow_name=='상암센터 정비실',]
data<-data[!data$return_name=='중랑센터',]
data<-data[!data$return_name=='위트콤',]
data<-data[!data$return_name=='상암센터 정비실',]


b=table(data$brow_no)
a=table(data$return_no)
aa=a+b


node=data.frame(sum=aa) 
colnames(node)=c("no","sum")
head(node)


ncol(unique(arrange(data[,c(3,4)], brow_no)))
unique(arrange(data[,c(3,4)], brow_no))[,2]
node$name <- unique(arrange(data[,c(3,4)], brow_no))[,2]

write.csv(node, file = "node_2017_1.csv",row.names = F)
