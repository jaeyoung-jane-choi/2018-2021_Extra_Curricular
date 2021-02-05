setwd("C:/Users/JIWON/Desktop/bumju code/real final")


library(tidyverse)
library(data.table)
library(openxlsx)
library(lubridate)
library(igraph)
library(NbClust)
library(factoextra)
library(randomcoloR)
library(mice)
library(tibble)

####################################################
####################################################
################# data carpentary ##################
####################################################
####################################################


###################### date data #####################


date <- rep(as.Date("2017-01-01"),730) + 0:729

# red day (Saturday, Sunday)
weekend <- rep(0,730)
for (i in 0:102) { weekend[c(7+7*i,8+7*i)] <- 1 } 

heuil <- data.frame(date = date, heuil = weekend)

# red day (official red day)
red_day <- c("2017-01-01","2017-01-27","2017-01-28","2017-01-29","2017-01-30",
             "2017-03-01","2017-05-03","2017-05-05","2017-06-06","2017-08-15",
             "2017-10-03","2017-10-04","2017-10-05","2017-10-06","2017-10-09",
             "2017-12-25",
             "2018-01-01","2018-02-15","2018-02-16","2018-02-17","2018-03-01",
             "2018-05-05","2018-05-07","2018-05-22","2018-06-06","2018-06-13",
             "2018-08-15","2018-09-23","2018-09-24","2018-09-25","2018-09-26",
             "2018-10-03","2018-10-09","2018-12-25")

heuil$heuil[heuil$date %in% as.Date(red_day,"%Y-%m-%d")] <- 1

heuil$month <- substr(as.character(heuil$date),6,7)
heuil$day <- substr(as.character(heuil$date),9,10) 

#write.csv(heuil, file = "date.csv", row.names = T)



############### choose location ##############
table1 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_1 .csv")
table2 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_2.csv")
table3 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_3.csv")
table4 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_4.csv")
table <- rbind(table1,table2,table3,table4)
remove(table1,table2,table3,table4)

table <- table[,1:8]
names(table) <- c("rent_date","rent_hour","loc_num","loc_name","code","sex","age","usage")
for (i in 1:7) { table[[i]] <- gsub("'","",table[[i]]); print(i)}

# location which have record in first week
survive <- table %>% filter(rent_date == "2017-01-01" | rent_date == "2017-01-02"| 
                              rent_date == "2017-01-03" | rent_date == "2017-01-04" | 
                              rent_date == "2017-01-05" | rent_date == "2017-01-06" | 
                              rent_date == "2017-01-07") %>% pull(loc_num)

# remove outliers
outliers <- c('350','중랑센터','상암센터 정비실','중랑정비팀test 1005','위트콤공장')
#final_survive <- setdiff(survive, outliers)

#remove(survive,outliers)
#write.csv(final_survive,"final_survive.csv",row.names=F)
final_survive <- read.csv("final_survive.csv") %>% pull(x) %>% as.character()

table <- table[table$loc_num %in% final_survive,]




################## location data #####################
table <- read.xlsx("대여소별 대여내역(2017년).xlsx")
table <- table[-1,1:2] 

names(table) <- c("gu","loc")
table$loc_num <-  str_split_fixed(table$loc,"\\.",2)[,1]

gu <- table[table$loc_num %in% final_survive,c(1,3)] %>% distinct()

#write.csv(gu,"gu.csv",row.names = F)




############### 1st variable #################

############# make sex/age data
people <- table %>% select(loc_num,sex,age,usage)
# merge sex and age
people$type <-  paste(people$sex, people$age, sep = '')
people$type <- ifelse (people$type == "" | people$type == "70대~" | people$type == "F" | people$type == "M", "Unknown", people$type) 

people_final <- people %>% group_by(loc_num,type) %>% summarise(sum_usage = sum(usage)) %>% 
  spread(type,sum_usage) %>% ungroup()
names(people_final) <- c("loc_num", "female_10","female_20","female_30","female_40","female_50","female_60","female_70",
                         "male_10","male_20","male_30","male_40","male_50","male_60","male_70","Unknown")


############# make user code data
user_final <- table %>% select(loc_num,code,usage) %>% group_by(loc_num,code) %>% summarise(sum_usage = sum(usage)) %>% 
  spread(code,sum_usage) %>% ungroup()
names(user_final) <- c("loc_num", "group","oneday_2hour","oneday_no_id","oneday_id","commutation","commutation_2hour")


############# make time data
daytime <- table %>% select(loc_num,rent_hour,usage) %>% group_by(loc_num,rent_hour) %>% summarise(sum_usage = sum(usage)) %>% 
  spread(rent_hour,sum_usage) %>% ungroup()
# group hour with new names
daytime_final <- data.frame(loc_num = daytime$loc_num,
                            morning_0612 = rowSums(daytime[,8:13],na.rm = T),
                            afternoon_1218 = rowSums(daytime[,14:19],na.rm =T),
                            night_1800 = rowSums(daytime[,20:25], na.rm = T),
                            dawn_0006 = rowSums(daytime[,2:7],na.rm=T) )

daytime_final$loc_num <- as.character(daytime_final$loc_num)


############# make week data
week <- table %>% select(loc_num,rent_date,usage)
# group hour with new names
week$rent_date <- weekdays(as.Date(week$rent_date,"%Y-%m-%d"))
week$weekend <- ifelse(week$rent_date == "토요일" | week$rent_date == "일요일", "weekend", "weekday")

week_final <- week %>% group_by(loc_num,weekend) %>% 
  summarise(sum_usage = sum(usage)) %>% spread(weekend,sum_usage) %>% ungroup()


############# merge
merge <- merge(daytime_final, people_final, by = "loc_num")
merge <- merge(merge, user_final, by = "loc_num")
merge <- merge(merge, week_final, by = "loc_num")


#write.csv(merge, "usage.csv", row.names = FALSE)
merge <- fread("usage.csv")
#merge$loc_num <- as.character(merge$loc_num)


merge[is.na(merge)] <- 0

############# Noye index
noye <- merge %>% select(morning_0612,night_1800,
                        female_30,female_40,female_50,
                        male_30,male_40,male_50,
                        commutation,commutation_2hour,
                        weekday)

pca_noye <- prcomp(noye, center = T, scale. = T)

# pca screeplot
summary(pca_noye)
plot(pca_noye,type = "l")

# how much each variable contribute to PC
aload <- abs(pca_noye$rotation)
sweep(aload, 2, colSums(aload), "/")

# make index
data_n <- as.matrix(scale(noye))
pc1_n <- as.matrix(pca_noye$rotation)
pc1_n <- sweep(aload, 2, colSums(aload), "/")


index_n <- data_h %*% pc1_n[,1]

############# Hanlyang index
han <- merge %>% select(afternoon_1218,
                        female_20,male_20,Unknown,
                        group,oneday_2hour,oneday_no_id,oneday_id,
                        weekend)
pca_han <- prcomp(han, center = T, scale. = T)

# pca screeplot
summary(pca_han)
plot(pca_han,type = "l")

# how much each variable contribute to PC
aload <- abs(pca_han$rotation)
sweep(aload, 2, colSums(aload), "/")

# make index
data_h <- as.matrix(scale(han))
pc1_h <- as.matrix(pca_han$rotation)
pc1_h <- sweep(aload, 2, colSums(aload), "/")

index_h <- data_h %*% pc1_h[,1]

index <- data.frame(loc_num = merge$loc_num, 
                    hanlyang_index = round(index_h), 
                    slave_index = round(index_n))

# see the distribution
eda <- index %>% gather(index,value,hanlyang_index,slave_index)

ggplot(eda, aes(x = value, fill = index, group = index)) +   
  geom_histogram(binwidth = 1,position="identity", alpha=0.5)

#write.csv(index,"pca.csv",row.names = F)


############### 2nd variable #################

############# link data
table1 <- fread("서울특별시 공공자전거 대여이력 정보_2017년_1.csv")
names(table1) <- c("bic_num","rent_date","rent_loc_num","rent_loc_name","rent_loc_spec",
                   "return_date","return_loc_num","return_loc_name","return_loc_spec",
                   "use_time","use_dist")
table2 <- fread("서울특별시 공공자전거 대여이력 정보_2017년_2분기_1.csv")
table3 <- fread("서울특별시 공공자전거 대여이력 정보_2017년_2분기_2.csv")
table4 <- fread("서울특별시 공공자전거 대여이력 정보_2017년_3분기_1.csv")
table5 <- fread("서울특별시 공공자전거 대여이력 정보_2017년_3분기_2.csv")
table11 <- rbind(table2,table3,table4,table5)
names(table11) <- c("bic_num","rent_date","rent_loc_num","rent_loc_name","rent_loc_spec",
                    "return_date","return_loc_num","return_loc_name","return_loc_spec",
                    "use_time","use_dist")

# merge data and select needy variable 
table <- rbind(table11,table1)
link <- table %>% select(rent_loc_num,return_loc_num)
remove(table,table1,table11,table2,table3,table4,table5)

# get rid of unnecassary single quote
link$rent_loc_num <- gsub("'","",link$rent_loc_num)
link$return_loc_num <- gsub("'","",link$return_loc_num)


# summarise total usage number by link
table_sum <- link %>% group_by(rent_loc_num,return_loc_num) %>% summarise(tot_use = n())

# cut with survived set
rent_link <- table_sum[table_sum$rent_loc_num %in% final_survive,]
rent_return_link <- rent_link[rent_link$return_loc_num %in% final_survive,]

#write.csv(rent_return_link,"link.csv",row.names = FALSE)


#############  node data
table1 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_1 .csv")
table2 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_2.csv")
table3 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_3.csv")
table4 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_4.csv")

# merge data and select needy variable 
table <- rbind(table1,table2,table3,table4)
remove(table1,table2,table3,table4)
table <- table[,c(3,8)]
names(table) <- c("loc_num","usage")

# get rid of unnecassary single quote
table$loc_num <- gsub("'","",table$loc_num )

# cut with survived set
table <- table[table$loc_num %in% final_survive,]

# summarise total usage number by node
node <- table %>% group_by(loc_num) %>% summarise(tot_usage = sum(usage)) %>% ungroup()

#write.csv(node, "node.csv", row.names=FALSE)

node <- fread("node.csv")
link <- fread("link.csv")



############# network analysis

# make adjacency matrix
adj.matrix <- link %>% spread(return_loc_num,tot_use) %>% remove_rownames %>% 
  column_to_rownames(var="rent_loc_num") %>% as.matrix()
adj.matrix[is.na(adj.matrix)] <- 0

adj <- graph.adjacency(adj.matrix, mode="directed", weighted = TRUE, diag = TRUE)

# degree centurality
degree <- adj.matrix %*% matrix(1,443,1)


# eigen centurality
eigen <- eigen_centrality(adj, directed = T, scale = TRUE, weights = E(adj)$weight)$vector

# betweenness
between <- betweenness(adj, v = V(adj), directed = TRUE,weights = E(adj)$weight, nobigint = TRUE, normalized = F)

# closeness
close <- closeness(adj, vids = V(adj), mode = "out", weights = E(adj)$weight, normalized = T)

# cluster
cluster <- data.frame(loc_num = colnames(adj.matrix), degree = degree, btw = between, eigen = eigen, close = close)
cluster <- cluster %>% remove_rownames %>% column_to_rownames(var="loc_num")

cluster_sc <- scale(cluster)
cor(cluster_sc)


nc <- NbClust(cluster_sc, min.nc=2, max.nc=15,distance = "euclidean", method="kmeans")

k <- kmeans(cluster_sc, centers = 5, nstart = 25)
fviz_cluster(k, data = cluster_sc, geom = "point", frame.type = "norm")

group1 <- cluster[k$cluster==1,]
group2 <- cluster[k$cluster==2,]
group3 <- cluster[k$cluster==3,]
group4 <- cluster[k$cluster==4,]
group5 <- cluster[k$cluster==5,]
summary(group1)
summary(group2)
summary(group3)
summary(group4)
summary(group5)
network <- data.frame(loc_num = names(k$cluster), group = k$cluster)

#write.csv(network,"network.csv",row.names = F)

############# network graph

# cut link more than 300
sum(link$tot_use>=300)

graph <- link %>% filter(tot_use>=300)

### 구 data 따오기
final=fread("realfinal.csv")
final2=data.frame(loc_num=final$loc_num, loc_group.x=final$loc_group.x)
gu=unique(final2)
gugu=gu[gu$loc_num %in% node$loc_num,]
gugu=arrange(gugu, gugu$loc_num)
node$loc_num==gugu$loc_num
node$gu=gugu$loc_group.x
rm(final, final2, gu, gugu, link)

write.csv(link, file = "net_link.csv", row.names = F)
write.csv(node, file = "net_node.csv", row.names = F)

### network 시각화

node=fread("net_node.csv")  
link=fread("net_link.csv")
set.seed(1)

net = graph_from_data_frame(link,node,directed = T)
net2=simplify(net, remove.multiple = T, remove.loops = F)
net2 = delete.vertices(simplify(net,remove.multiple = F, degree(net2)==0), degree(net2)==0)
rm(net)
n <- nlevels(as.factor(V(net2)$gu))
palette <- distinctColorPalette(n)
pal2 <- palette

x = V(net2)$total_num_tot
V(net2)$size = ((x-min(x))/(max(x)-min(x)))*11+4
y = link$tot_use
E(net2)$weight = edge_attr(net2, "tot_use")*0.01


l <- layout_with_fr(delete.vertices(simplify(net2,remove.multiple = F, 
                                             degree(net2)==0),
                                    degree(net2)==0),niter=3000)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)



plot(net2, edge.width=E(net2)$weight,edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))],edge.width=1,
     vertex.label=NA, layout=l, vertex.size=vertex_attr(net2,'total_num_tot')*0.0003) 

## loc_num 보이는 버전
plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))],edge.width=1, layout=l, vertex.size=vertex_attr(net2,'total_num_tot')*0.0003) 

legend(x=-1.5, y=-1.1, unique(vertex_attr(net2,'gu')), pch=21,
       pt.bg=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))], pt.cex=2, cex=.8,
       bty="n", ncol=5)



vertex.label.cex=V(net2)$total_num_tot*0.06 ## 얘가 다운의 원인이었음 이 argument 사용 ㄴㄴ


## degree top 5

dc_top5 <- head(sort(degree(net2), decreasing = T), 5)
node[node$loc_num %in% as.integer(names(dc_top5)),]
V(net2)[name %in% as.integer(names(dc_top5))]

cdc <- centr_degree(net2, normalized = T)


### degree cen 바탕으로 시각화
vcol <- rep("grey40", vcount(net2))
vcol[V(net2)$name %in% as.integer(names(dc_top5))] <- "skyblue"
vsize=V(net2)$size
vsize[V(net2)$name %in% as.integer(names(dc_top5))] <-30

vlabel=rep(NA, 443)
vlabel[V(net2)$name %in% as.integer(names(dc_top5))] <- V(net2)$name[V(net2)$name %in% as.integer(names(dc_top5))]



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=vlabel, layout=l*1.5, vertex.size=vsize) 



## close top 5
cc <- closeness(net2)
ccc <- centr_clo(net2, normalized = T)
ccc_top10 <- head(sort(ccc$res, decreasing = T), 10)
node[node$loc_num %in% as.integer(names(cc_top10)),]
V(net2)[name %in% as.integer(names(cc_top10))]

mean(sqrt((cc-ccc$res)^2))

## btw top 5
bc <- betweenness(net2) 
cbc <- centr_betw(net2, normalized = T)
bc_top10 <- head(sort(bc, decreasing = T), 10)
node[node$loc_num %in% as.integer(names(bc_top10)),]
V(net2)[name %in% as.integer(names(bc_top10))]

scale(cbc$res)


## eigen top 5
eigc <- eigen_centrality(net2, directed = T, weights = NA)$vector
ceigc <- centr_eigen(nnet)
eigc_top10 <- head(sort(eigc, decreasing = T), 10)

node[node$loc_num %in% as.integer(names(eigc_top10)),]
V(net2)[name %in% as.integer(names(eigc_top10))]



### 혜화
V(net2)$gu=="종로구"

node[node$loc_num %in% c(386, 340,388,358,341,342,337,356,355),]

vcol <- rep("grey", vcount(net2))

vcol[V(net2)$gu=="종로구"] <- "orange"
vcol[V(net2)$name %in% c(386, 340,388,358,341,342,337,356,355)] <- "green"


vsize=rep(10, vcount(net2))
vsize[V(net2)$name %in% c(386, 340,388,358,341,342,337,356,355)] <-30

edge.start <- as.integer(ends(net2, es=E(net2), names=T)[,1]) %in% c(386, 340,388,358,341,342,337,356,355)
E(net2)$color[edge.start] <- "green"


plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=NA, layout=l, asp=1.9)



edge.col <- V(net2)$color[edge.start] ##이거 제외  

plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))],edge.width=1,
     vertex.label=NA, layout=l, vertex.size=vertex_attr(net2,'total_num_tot')*0.0003)




# 시각화용 link num >= 300만 남기기 
node=fread("net_node.csv")  
link=fread("net_link.csv")

set.seed(1)

## network data
net = graph_from_data_frame(link,node,directed = T) 
net2=simplify(net, remove.multiple = T, remove.loops = F)
rm(net)
n <- nlevels(as.factor(V(net2)$gu))
palette <- distinctColorPalette(n)
pal2 <- palette

x = V(net2)$total_num_tot
V(net2)$size = ((x-min(x))/(max(x)-min(x)))*11+4
y = link$tot_use
E(net2)$weight = ((y-min(y))/(max(y)-min(y)))*33+4


l <- layout_with_fr(delete.vertices(simplify(net2,remove.multiple = F, 
                                             degree(net2)==0),
                                    degree(net2)==0),niter=3000)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))],edge.width=1,
     vertex.label=NA, layout=l, vertex.size=vertex_attr(net2,'total_num_tot')*0.0003,
     edge.width=E(net2)$weigth,edge.width=E(net2)$weight) 


E(net2)$weigth <- edge_attr(net2, "tot_use")*0.01 ## link weight는 시각화 불가


## degree top 5

dc_top5 <- head(sort(degree(net2), decreasing = T), 5)
node[node$loc_num %in% as.integer(names(dc_top5)),]
V(net2)[name %in% as.integer(names(dc_top5))]

cdc <- centr_degree(net2, normalized = T)

### degree cen 바탕으로 시각화
vcol <- rep("grey40", vcount(net2))
vcol[V(net2)$name %in% as.integer(names(dc_top5))] <- "skyblue" ## top5만 색 넣기

vsize=V(net2)$size
vsize[V(net2)$name %in% as.integer(names(dc_top5))] <-25 ## top5만 크게 보이게

vlabel=rep(NA, vcount(net2))
vlabel[V(net2)$name %in% as.integer(names(dc_top5))]<-
  V(net2)$name[V(net2)$name %in% as.integer(names(dc_top5))] ### top5만 label 나오도록


### link  ends로 link 찾고 그 중 top5의 링크만 찾기
edge.start <- as.integer(ends(net2, es=E(net2), names=T)[,1]) %in% names(dc_top5)
edge.start2 <- as.integer(ends(net2, es=E(net2), names=T)[,2]) %in% names(dc_top5)

E(net2)$color[edge.start] <- "skyblue" ## 그걸 바탕으로 시각화
E(net2)$color[edge.start2] <- "skyblue"

### link를 이렇게 찾을 수도 있음 (이번 분석에서는 안씀)
dinc=incident(net2, V(net2)[V(net2)$name %in% as.integer(names(dc_top5))], mode = "all")

dinc1=incident(net2, V(net2)[name == "155"])
dinc2=incident(net2, V(net2)[name == "111"])
dinc3=incident(net2, V(net2)[name == "147"])
dinc4=incident(net2, V(net2)[name == "136"])
dinc5=incident(net2, V(net2)[name == "707"])


ecol <- rep("gray40", ecount(net2))
ecol[dinc1] <- "skyblue"
ecol[dinc2] <- "skyblue"
ecol[dinc3] <- "skyblue"
ecol[dinc4] <- "skyblue"
ecol[dinc5] <- "skyblue"


plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=vlabel, layout=l*1.5, vertex.size=vsize) 


## close top 5
cc <- closeness(net2)
ccc <- centr_clo(net2, normalized = T)
cc_top5 <- head(sort(cc, decreasing = T), 5)
node[node$loc_num %in% as.integer(names(cc_top5)),]
V(net2)[name %in% as.integer(names(cc_top5))]

## cc 시각화
vcol <- rep("grey40", vcount(net2))
vcol[V(net2)$name %in% as.integer(names(cc_top5))] <- "skyblue"
vsize=V(net2)$size
vsize[V(net2)$name %in% as.integer(names(cc_top5))] <-25

vlabel=rep(NA, vcount(net2))
vlabel[V(net2)$name %in% as.integer(names(cc_top5))]<-V(net2)$name[V(net2)$name %in% as.integer(names(cc_top5))]



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=vlabel, layout=l*1.5, vertex.size=vsize) 


## bc top 5
bc <- betweenness(net2)
bc_top5 <- head(sort(bc, decreasing = T), 5)
node[node$loc_num %in% as.integer(names(bc_top5)),]
V(net2)[name %in% as.integer(names(cc_top5))]

## bc 시각화
vcol <- rep("grey40", vcount(net2))
vcol[V(net2)$name %in% as.integer(names(bc_top5))] <- "skyblue"
vsize=V(net2)$size
vsize[V(net2)$name %in% as.integer(names(bc_top5))] <-25

vlabel=rep(NA, vcount(net2))
vlabel[V(net2)$name %in% as.integer(names(bc_top5))]<-V(net2)$name[V(net2)$name %in% as.integer(names(bc_top5))]



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=vlabel, layout=l*1.5, vertex.size=vsize)


## ec top 5
ec <- eigen_centrality(net2, directed = T, weights = NA)$vector
ec_top5 <- head(sort(ec, decreasing = T), 5)
node[node$loc_num %in% as.integer(names(ec_top5)),]
V(net2)[name %in% as.integer(names(ec_top5))]

## ec 시각화
vcol <- rep("grey40", vcount(net2))
vcol[V(net2)$name %in% as.integer(names(ec_top5))] <- "skyblue"
vsize=V(net2)$size
vsize[V(net2)$name %in% as.integer(names(ec_top5))] <-25

vlabel=rep(NA, vcount(net2))
vlabel[V(net2)$name %in% as.integer(names(ec_top5))]<-V(net2)$name[V(net2)$name %in% as.integer(names(ec_top5))]



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=vlabel, layout=l*1.5, vertex.size=vsize)



### 혜화
V(net2)$gu=="종로구"

node[node$loc_num %in% c(386, 340,388,358,341,342,337,356,355),]

vcol <- rep("grey", vcount(net2))

vcol[V(net2)$gu=="종로구"] <- "orange"
vcol[V(net2)$name %in% c(386, 340,388,358,341,342,337,356,355)] <- "green"


vsize=rep(10, vcount(net2))
vsize[V(net2)$name %in% c(386, 340,388,358,341,342,337,356,355)] <-30

edge.start <- as.integer(ends(net2, es=E(net2), names=T)[,1]) %in% c(386, 340,388,358,341,342,337,356,355)
E(net2)$color[edge.start] <- "green"

l <- layout_with_fr(delete.vertices(simplify(net2,remove.multiple = F, 
                                             degree(net2)==0),
                                    degree(net2)==0),niter=3000)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=vcol,edge.width=1,
     vertex.label=NA, layout=l)



plot(net2, edge.arrow.size=0.1, vertex.label.color="black",
     vertex.color=pal2[as.numeric(as.factor(vertex_attr(net2,'gu')))],edge.width=1,
     vertex.label=NA, layout=l, vertex.size=vertex_attr(net2,'total_num_tot')*0.0003)






################## weather data ####################
weather <- fread("20190526234104.csv")
weather <- weather[,c(2,3,4,6,13,20,26)] 
names(weather) <- c("date","mean_temp","min_temp","max_temp","precip","wind","humid")
weather <- weather %>% filter(date >= "2017-10-01" & date <= "2018-11-31")

# get diff_temp and date variable
weather <- weather %>% mutate(diff_temp = max_temp - min_temp)
weather$date <- as.Date(weather$date, "%Y-%m-%d")

# visualization of NA in precip
weather$group <- as.factor(ifelse(is.na(weather$precip), "NA","EXIST"))
ggplot(weather, aes(x = humid, fill = group, group = group)) +   
  geom_histogram(binwidth = 2.5,position="identity", alpha=0.8) +   
  scale_x_continuous(breaks = seq(10,100,10)) 

# handle precip NA to zero
weather$precip[is.na(weather$precip)] <- 0

weather <- weather %>% select(date,mean_temp,precip,wind,humid,diff_temp)
colSums(is.na(weather))


# handle wind NA with mice
wind <- fread("20190526234104.csv")
wind <- wind[,c(2,14:22)] 
names(wind) <- c("date","max_wind_1","max_wind_2","max_wind_3","max_wind_4",
                 "max_wind_5","max_wind_6","wind","max_wind_8","max_wind_9")
wind <- wind %>% filter(date >= "2017-10-01" & date <= "2018-11-31")
wind$date <- as.factor(wind$date)


windData <- mice(wind, m = 5, maxit = 20, method ='cart', seed=500)

# check mice worked properly
plot(windData)
stripplot(windData, pch = 20, cex = 1.2)
densityplot(windData)
fit <- with(data = windData, exp = lm(wind ~  max_wind_1 + max_wind_2 + max_wind_3 + max_wind_4 +
                                   max_wind_5 + max_wind_6 + max_wind_8 + max_wind_9))
p <- pool(fit) 
round(summary(p),2)
pool.r.squared(fit)

# get mean 
wind_mice <- mice::complete(windData,"all")
pool <- rbind(wind_mice$`1`, wind_mice$`2`, wind_mice$`3`, wind_mice$`4`, wind_mice$`5`)
wind_final <- pool %>% group_by(date) %>% summarise_all(mean) %>% ungroup()
colSums(is.na(wind_final))


# handle humid NA with mice
humid <- fread("20190526234104.csv")
humid <- humid[,c(2,23:26)] 
names(humid) <- c("date","humid_1","humid_2","humid_3","humid")
humid <- humid %>% filter(date >= "2017-10-01" & date <= "2018-11-31")
humid$date <- as.factor(humid$date)


humidData <- mice(humid, m = 5, maxit = 20, method ='cart', seed=500)

# check mice worked properly
plot(humidData)
stripplot(humidData, pch = 20, cex = 1.2)
densityplot(humidData$data$humid)

fit <- with(data = humidData, exp = lm(humid ~ humid_1 + humid_2 + humid_3))
p <- pool(fit) 
round(summary(p),2)
pool.r.squared(fit)

# get mean 
humid_mice <- mice::complete(humidData,"all")
pool <- rbind(humid_mice$`1`, humid_mice$`2`, humid_mice$`3`, humid_mice$`4`, humid_mice$`5`)
humid_final <- pool %>% group_by(date) %>% summarise_all(mean) %>% ungroup()
colSums(is.na(humid_final))


weather$wind <- wind_final %>% pull(wind)
weather$humid <- humid_final %>% pull(humid)
colSums(is.na(weather))

#write.csv(weather, "weather.csv", row.names = F)


#################### dust data #####################
dust.17 <- read.xlsx("일별평균대기오염도_2017.xlsx")
names(dust.17) <- c("when","where","NO2","O3","n","SO2","pm10","pm2.5")
dust.17 <- dust.17 %>% filter(when >= 20171001)
dust.17 <- dust.17 %>% filter(where != "강남대로" & where != "강변북로" &
                                where != "공항대로" & where != "도산대로" &
                                where != "동작대로" & where != "신촌로" & 
                                where != "정릉로" & where != "종로" &
                                where != "천호대로" & where != "청계천로" &
                                where != "한강대로" & where != "홍릉로" &
                                where != "화랑로" & where  != "영등포로")

names(dust.17) <- c("rent_date","gu","NO2","O3","n","SO2","pm10","pm2.5")


dust.18 <- read.xlsx("일별평균대기오염도_2018.xlsx")
names(dust.18) <- c("when","where","NO2","O3","n","SO2","pm10","pm2.5")
dust.18 <- dust.18 %>% filter(when >= 20181001 & when < 20181201)
dust.18 <- dust.18 %>% filter(where != "강남대로" & where != "강변북로" &
                                where != "공항대로" & where != "도산대로" &
                                where != "동작대로" & where != "신촌로" & 
                                where != "정릉로" & where != "종로" &
                                where != "천호대로" & where != "청계천로" &
                                where != "한강대로" & where != "홍릉로" &
                                where != "화랑로" & where  != "영등포로" & 
                                where != "관악산" & where != "궁동" &
                                where != "남산" & where != "북한산" &
                                where != "세곡" & where != "행주" &
                                where != "시흥대로" )
names(dust.18) <- c("rent_date","gu","NO2","O3","n","SO2","pm10","pm2.5")

f <- fread("2018년 1분기.csv")
s <- fread("2018년 2분기.csv")
t <- fread("2018년 3분기.csv")
t <- t[,-2]
dust.new <- rbind(f,s,t)
dust.new <- dust.new[,c(1,4,5,6,7,8,9,10)]
names(dust.new) <- c("gu","rent_date","SO2","n","O3","NO2","pm10","pm2.5") 

dust.new$rent_date <- substr(dust.new$rent_date,1,8)
dust.tidy <- dust.new %>% group_by(rent_date,gu) %>% 
  summarise_all(mean,na.rm = TRUE) %>% ungroup() %>% filter(gu != "서울 중구 청" & gu != "서울 강서로")

dust.tidy$city <- data.frame(do.call('rbind', strsplit(dust.tidy$gu,split=" ")))$X1 %>% as.character()
dust.tidy$gu <- data.frame(do.call('rbind', strsplit(dust.tidy$gu,split=" ")))$X2 %>% as.character()
dust.tidy <- dust.tidy %>% filter(city == "서울") %>% select(-city,-n)


dust1 <- dust.17 %>% select(-n)
dust2 <- dust.18 %>% select(-n)
dust3 <- cbind(dust.tidy[,2],dust.tidy[,1],dust.tidy[,5],dust.tidy[,4],dust.tidy[,3],dust.tidy[,c(6,7)])



dust <- rbind(dust1,dust2,dust3)
colSums(is.na(dust))

rm(f,s,t,dust.17,dust.18,dust.tidy,dust1,dust2,dust3,dust.new)

# mice: missing at random
dust <- dust %>% mutate_if(is.character,as.factor)
tempData <- mice(dust, m = 5, maxit = 10, method ='cart', seed=500)


# check mice worked properly
plot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)
densityplot(tempData)


dust_mice <- mice::complete(tempData,"all")
pool <- rbind(dust_mice$`1`, dust_mice$`2`, dust_mice$`3`,dust_mice$`4`,dust_mice$`5`)
dust_final <- pool %>% group_by(when,where) %>% summarise_all(mean) %>% ungroup()
colSums(is.na(dust_final))

#write.csv(dust_final,"dust.csv",row.names = F)



################## bike road data ###################



################## festival data ###################



################## X (full) data ###################

train <- fread("train_y.csv")
test <- fread("test_y.csv")
data <- rbind(train,test)

weather <- fread("weather.csv")
names(weather) <- c("rent_date",names(weather)[2:6])
data <- merge(data,weather,by="rent_date")

gu <- fread("gu.csv")
data <- merge(data,gu,by="loc_num")

dust <- fread("dust.csv")
names(dust) <- c("rent_date","gu",names(dust)[3:7])
dust$rent_date <- as.Date(as.character(dust$rent_date),"%Y%m%d") 
dust$rent_date <- as.character(dust$rent_date)
dust$mergegu <- paste(dust$rent_date, dust$gu, sep = '-')
data$mergegu <- paste(data$rent_date, data$gu, sep = '-')
data <- merge(data,dust[,-c(1,2)],by="mergegu")
data <- data %>% select(-mergegu,-mergekey)



date <- fread("date.csv") %>% select(-V1)
names(date) <- c("rent_date",names(date)[2:4])
data <- merge(data,date,by="rent_date")

data <- data %>% select(-y)

index <- fread("pca.csv")
#group <- fread("network.csv")




############# Y (rental number) data ###############

# get data
table5 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_5.csv")
table6 <- fread("서울특별시 공공자전거 이용정보(시간대별)_2017년_6.csv")
table7 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_2018년_01_02.xlsx")
table8 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_2018년_03_04.xlsx")
table9 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201805.xlsx")
table10 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201806.xlsx")
table11 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201807.xlsx")
table12 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201808.xlsx")
table13 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201809.xlsx")
table14 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201810.xlsx")
table15 <- read.xlsx("서울특별시 공공자전거 시간대별 대여정보_201811.xlsx")

# need to bind seperately because of column name
time <- rbind(table5,table6)
time2 <- rbind(table7,table8,table9,table10,table11,table12,table13)
test <- rbind(table14,table15)
time <- time[,c(1,3,4,8)]
time2 <- time2[,c(1,3,4,8)]
test <- test[,c(1,3,4,8)]

# change column name to English
names(time) <- c("rent_date","loc_num","loc_name","usage")
names(time2) <- c("rent_date","loc_num","loc_name","usage")
names(test) <- c("rent_date","loc_num","loc_name","usage")

# get rid of unnecessary single quote
time$rent_date <- gsub("'","",time$rent_date)
time$loc_num <- gsub("'","",time$loc_num )
time$loc_name <- gsub("'","",time$loc_name)
time2$loc_num <- as.character(time2$loc_num)
test$loc_num <- as.character(test$loc_num)

# cut with survived set
time <- time[time$loc_num %in% final_survive,]
time2 <- time2[time2$loc_num %in% final_survive,]
test <- test[test$loc_num %in% final_survive,]

# change date to date class
time$rent_date <- as.Date(as.character(time$rent_date),"%Y-%m-%d")
time2$rent_date <- as.Date(as.numeric(time2$rent_date), origin = "1899-12-30")
test$rent_date <- as.Date(as.numeric(test$rent_date), origin = "1899-12-30") 

# train and test
train <- rbind(time,time2)
remove(time,time2,table5,table6,table7,table8,table9,table10,table11,table12,table13,table14,table15)

train_y <- train %>% group_by(rent_date,loc_num) %>% summarise(y = sum(usage)) %>% ungroup()
test_y <- test %>% group_by(rent_date,loc_num) %>% summarise(y = sum(usage)) %>% ungroup()

#write.csv(train_y,"train_y.csv",row.names = F)
#write.csv(test_y,"test_y.csv",row.names = F)











############## merge data ###############





red.day.17 <- fread("gongheuil.csv")
red.day.18 <- fread("heuil_2018.csv")
names(red.day.18) <- c("date","heuil") 

red_train <- rbind(red.day.17[274:365,],red.day.18[1:273,])
red_test <- red.day.18[274:334,]
rm(red.day.17,red.day.18)


red <- rbind(red_train,red_test)
names(red) <- c("rent_date","redday")
data5 <- merge(data4,red,by="rent_date")


data6 <- arrange(data5,rent_date,loc_num)

data_f_train <- fread("train_y_festival.csv")
data_f_test <- fread("test_y_festival.csv")
f <- rbind(data_f_train,data_f_test)

data6$festival <- f$festival




data_train <- fread("newtrain.csv")
data_test <- fread("newtest.csv")

j <- rbind(data_train,data_test)


data_final <- cbind(data6,j[,4:9])
data_final <- data_final %>% select(-rent_date)

names(data_final) <- c(names(data_final)[1:19],"month","day")

colSums(is.na(data_final))


data_final <- data_final %>% select(-slave_index,-hanlyang_index)

pca <- fread("pca.csv")
data_final_p <- merge(data_final,pca,by = 'loc_num')

ggplot(data_final, aes(x = y, fill = gu)) +   
  geom_histogram(binwidth = 10) +   
  scale_x_continuous(breaks = seq(100,500,100)) +
  theme(panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )


ggplot(data_final, aes(x = networkgroup)) +   
  geom_histogram(binwidth = 1) +   
  theme(panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )



write.csv(data_final_p,"data_new_index.csv",row.names = F)
data_final_train <- data_final %>% filter(rent_date >= "2017-10-01" & rent_date < "2018-10-01")
data_final_test <- data_final %>% filter(rent_date >= "2018-10-01")




tr <- fread("data_new_index.csv")
tr <- tr %>% filter(loc_num == 358)
summary(tr$y)

ggplot(tr, aes(x = y)) +   
  geom_histogram(binwidth = 4,fill="#F8766D") +   
  theme(panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )


train <- fread("skku_crossed_street.csv")





####################################################
####################################################
################# interpret model #################
####################################################
####################################################


train<-fread('data_new_index.csv')
train<-train[,-1]  # erase 'loc_num' column for reducing dimension of factor loc_num

train<-as.data.frame(train)


#making variables as factor#

for ( i in c(7,17,19,20,14,18,13) ) #gu/ networkgroup/ month/ day/holiday/bikeroad / festival
  
{train[,i]<-as.factor(train[,i])}

str(train)

#check VIF for multicollinearity in train data

#install.packages('usdm')
library(usdm)
df <- train[,2:20]   
vif(df) 



# modeling except mean_temp variable due to multicollinearity #

library(MASS)

fit_negbin <- glm.nb(y~.-mean_temp, data = train)
summary(fit_negbin)


# train <- within(train, rent_month <- relevel(rent_month, ref = 4)) # for comparing variables of factors with different reference level 


# caluculating 5-fold CV error for training data #

cost <- function(obs, hat) { MAPE(obs,hat) } # function for MAPE 

cv.negbin<-cv.glm(data=train , glmfit=fit_negbin, cost=cost(train$y,tr.yhat) , K=5)
cv.negbin$delta[1]



####################################################
####################################################
################## predict model ###################
####################################################
####################################################
