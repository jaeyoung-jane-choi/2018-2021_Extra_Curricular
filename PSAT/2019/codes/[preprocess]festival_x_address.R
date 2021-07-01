library(data.table)
library(dplyr)
library(stringr)
library(geosphere)

festival=fread("newfestival.csv")
festival=festival[-1,]

address=fread("address.csv")

node=fread("net_node.csv")

address=address[address$loc_num %in% node$loc_num,]


a=str_split_fixed(address$loc_real_address, ' ', n=4)[,2]
b=str_split_fixed(address$loc_real_address, ' ', n=4)[,3]
address$key <- paste(a,b,sep = ' ')



festival=fread("absolfestival.csv")
loc=fread("loc.csv")
loc$address<-NULL



bb <- festival
aa<-loc

my_coord <- (cbind(bb$lon, bb$lat))
coods<- (cbind(aa$lon, aa$lat))

dd<- matrix(0, nrow=length(aa$lon), ncol=length(bb$lat)) 

dd2 <- matrix(0, nrow=length(aa$lon), ncol=length(bb$lat)) 

outer_loop_state <- 0

for(i in 1:length(aa$lon)) {
  for (j in 1:length(bb$lon)) {
    dd[i,j] <- as.numeric(distHaversine(my_coord[j,], coods[i,])) # 행: 대여소 / 열: 거리 이름 
    dd2[i, j] <- dd[i,j] <= 1000 } #(1km 반경)
  if((dd2[i,j])){ 
    outer_loop_state <- 1
  } else {
    if(outer_loop_state == 1){
      
    }
  }
}
colnames(dd2)<-1:nrow(festival)
rownames(dd2)=sort(loc$loc_num)
head(dd2)
dd2_t=t(dd2)


################################ 날짜 x 축제 data.frame
library(tidyr)

festival=as.data.frame(festival)

fest=as.Date("2017-10-01")
for (i in 1:456) {
  fest <- c(fest,fest[1]+i)
}

nalza_x_festival=as.data.frame(matrix(0,457,203))
rownames(nalza_x_festival)=fest

for (i in 1:203) {
  nalza_x_festival[i] <- ifelse(as.Date(festival[,1][i]) <= 
                                  fest & as.Date(festival[,2][i])>= fest,1,0)
}

nalza_x_festival=as.matrix(nalza_x_festival)
dd2_t=as.matrix(dd2_t)
nalza_loc=nalza_x_festival %*% dd2_t
colnames(nalza_loc)<-aa$loc_num


gather_festival=as.data.frame(nalza_loc) %>% gather(loc, festival, 1:443)
gather_festival$rent_data<- rep(fest, 443) 
gather_festival <- gather_festival[,c(3,1,2)]
gather_festival=arrange(gather_festival, loc)
gather_festival[gather_festival$festival==3,]

## train_y

train$mergekey=paste(train$rent_date, train$loc_num,sep = "~")

gather_festival$mergekey=paste(gather_festival$rent_data, gather_festival$loc, sep = "~")

gather_festival_reduced=gather_festival[gather_festival$mergekey %in% train$mergekey,]
gather_festival_reduced=arrange(gather_festival_reduced, rent_data, loc)
train$festival=gather_festival_reduced$festival

write.csv(train, file = "train_y_festival.csv", row.names = F)


## test_y
te=fread("test_y.csv")
head(te)
te$mergekey=paste(te$rent_date, te$loc_num,sep = "~")
gather_festival_reduced_test=gather_festival[gather_festival$mergekey %in% te$mergekey,]

gather_festival_reduced_test=arrange(gather_festival_reduced_test, rent_data, loc)
te$festival=gather_festival_reduced_test$festival

write.csv(te, file = "test_y_festival.csv", row.names = F)

