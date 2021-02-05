### make cluster

library(data.table)
library(dplyr)
library(NbClust)
library(cluster)
library(factoextra)
library(tidyverse)
library(clValid)

set.seed(1234)

cluster <- fread("node_centrality.csv")
cluster <- cluster %>% select(loc_num,close,btw,degree)
cluster <- cluster %>% remove_rownames %>% column_to_rownames(var="loc_num")
cluster_sc <- scale(cluster)

intern <- clValid(cluster_sc, nClust = 2:6, clMethods = c("hierarchical","kmeans","pam"),
                  validation = "internal")
summary(intern)

nc <- NbClust(cluster_sc, min.nc=2, max.nc=8,distance = "euclidean", method="kmeans")
plot(nc$All.index[,4], type="o", ylab="CCC", xlab="Number of clusters", col="blue")
colnames(cluster_sc)


k <- kmeans(cluster_sc, centers = 4, nstart = 25)
fviz_cluster(k, data = cluster_sc, geom = "point", elipse.type = "norm") +
  theme(panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )



k1 <- k$cluster

group1 <- cluster[k1==1,]
group2 <- cluster[k1==2,]
group3 <- cluster[k1==3,]
group4 <- cluster[k1==4,]


summary(group1)
summary(group2)
summary(group3)
summary(group4)

k.res <- eclust(cluster_sc, "kmeans", k = 4, graph = FALSE)
fviz_cluster( geom = "point", frame.type = "norm") +
  theme(panel.background = element_rect(fill = "transparent") # bg of the panel
        , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

fviz_silhouette()

library(mlbench)
hc<-hclust(dist(cluster_sc), method = "average")
plot(hc)
rect.hclust(hc, k=3, border = "red")
plot(silhouette(cutree(hc,k=3), dist=dist(cluster_sc),col=1:7))

