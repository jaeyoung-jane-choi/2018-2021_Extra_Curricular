# 따릉이 네트워크

library(tidyr)
library(dplyr)
library(data.table)
library(igraph)
library(randomcoloR)
library(caret)

node=fread("net_node.csv")  
link=fread("net_link.csv")

## 링크 300이상인 친구들만 남기기
sum(link$tot_use>=300)
link2=link[link$tot_use>=300,]
glimpse(node)

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
library(tidyr)
library(dplyr)
library(data.table)
library(igraph)
library(randomcoloR)
library(caret)
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


#### hub & Authority top 5
hs=hub_score(net2, weights = E(net2)$rent_loc_num)$vector

hs_top5 <- head(sort(hs, decreasing = T), 5)
node[node$loc_num %in% as.integer(names(hs_top5)),]
V(net2)[name %in% as.integer(names(cc_top10))]


as=authority_score(net2, weights = E(net2)$return_loc_num)$vector
as_top5 <- head(sort(as, decreasing = T), 5)
node[node$loc_num %in% as.integer(names(as_top5)),]



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




##### 중심성 바탕으로 새 노드 만들기
node2=as.data.frame(node)
node2$degree=degree(net2)
node2$cent_close=ccc$res
node2$cent_btw=cbc$res
node2$eigen=eigen_centrality(net2)$vector
node2$hub=hs
node2$authority=as

write.csv(node2, file = "cen_node.csv", row.names = F)




####### 필요없음
un=union(link$rent_loc_num, link$return_loc_num)
setdiff(un, node$loc_num)
length(un)
length(node$loc_num)
range(node$loc_num)
length(unique(node$loc_num))

glimpse(node)

ggplot(link, aes(x = tot_use)) +
  geom_bar(stat="bin", fill="steelblue") 


