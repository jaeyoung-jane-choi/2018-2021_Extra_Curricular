library(tidyr)
library(dplyr)
library(data.table)
library(igraph)
library(randomcoloR)
library(caret)

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

