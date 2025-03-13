library(tidyverse)
library(cluster)

red_wine <- read.csv(file = 'winequality-red.csv', header = TRUE, sep = ',')

data <- red_wine %>%
  group_by(free.sulfur.dioxide) %>%
  summarise_all(mean, na.rm = TRUE)
str(data)

dis = dist(data, method = 'euclidean', diag = T)

single_linkage_track = hclust(dis, method = "single")
plot(single_linkage_track,  sub="",xlab="free sulfur dioxide", main="Cluster Dendrogram using Single Linkage", cex=0.5, ylab="Distance")
segments(31,23,35,23, col="green")

complete_linkage_track = hclust(dis,  method = "complete")
plot(complete_linkage_track,  sub="",xlab="free sulfur dioxide", main="Cluster Dendrogram using Complete Linkage", cex=0.5, ylab="Distance")
abline(h=40, col="red")

avg_linkage = hclust(dis, method = "average")
plot(avg_linkage,  sub="",xlab="free sulfur dioxide", main="Cluster Dendrogram using Average Linkage", cex=0.5, ylab="Distance")
segments(3,70,27,70, col="red")
segments(-1,200,3,200, col="red")

ward_linkage = hclust(dis, method = "ward.D")
plot(ward_linkage,  sub="",xlab="free sulfur dioxide", main="Cluster Dendrogram using Ward's method", cex=0.5, ylab="Distance")
segments(1,300,13,300, col="green")
segments(15,400,40,400, col="green")

km3 = kmeans(data, 3)
name = c(1, 2, 3, 4, 5, 5.5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 37.5, 38, 39, 40, 40.5, 41, 42, 43, 45, 46, 47, 48, 50, 51, 52, 53, 54, 55, 57, 66, 68, 72)
result <- data.frame(Object = name, Cluster = km3$cluster)

km4 = kmeans(data, 4)
result2 <- data.frame(Object = name, Cluster = km4$cluster)

km5 = kmeans(data, 5)
result3 <- data.frame(Object = name, Cluster = km5$cluster)

cbind(result, result2, result3)

clusplot(data[,-1], km3$cluster, color = T, shade = T, labels = 2, main = "K-menas Clustering for K=3")

clusplot(data[,-1], km4$cluster, color = T, shade = T, labels = 2, main = "K-menas Clustering for K=4")

clusplot(data[,-1], km5$cluster, color = T, shade = T, labels = 2, main = "K-menas Clustering for K=5")

sil_3 = silhouette(km3$cluster, dis)
plot(sil_3, main = "Silhouette plot for 3-means",  col = c("red", "green", "blue"))

sil_4 = silhouette(km4$cluster, dis)
plot(sil_4, main = "Silhouette plot for 4-means",  col = c("red", "green", "blue", "purple"))

sil_5 = silhouette(km5$cluster, dis)
plot(sil_5, main = "Silhouette plot for 5-means",  col = c("red", "green", "blue", "purple", "orange"))

km2=kmeans(data,centers = 2)
Silkm2=silhouette(km2$cluster,dist(data))
plot(Silkm2,main = "silhouette of kmean with k=2",col = c("red", "green"))

Silhclsinglek2 <- silhouette(cutree(single_linkage_track,k=2),dist(data))
plot(Silhclsinglek2,main = "silhouette of hierarchical single linkage with k=2",col = c("red", "green"))

Silhclcompletek2 <- silhouette(cutree(complete_linkage_track,k=2),dist(data))
plot(Silhclcompletek2,main = "Silhouette of hierarchical complete linkage with k=2",col = c("red", "green"))

Silhclaveragek2 <- silhouette(cutree(avg_linkage,k=2),dist(data))
plot(SilhclwardDk2,main = "Silhouette of hierarchical average linkage with k=2",col = c("red", "green"))

SilhclwardDk2 <- silhouette(cutree(ward_linkage,k=2),dist(data))
plot(SilhclwardDk2,main = "Silhouette of hierarchical ward.D linkage with k=2",col = c("red", "green"))

km3=kmeans(data,centers = 3)
Silkm3=silhouette(km3$cluster,dist(data))
plot(Silkm3,main = "silhouette of kmean with k=3",col = c("red", "green", "blue"))

Silhclsinglek3 <- silhouette(cutree(single_linkage_track,k=3),dist(data))
plot(Silhclsinglek2,main = "silhouette of hierarchical single linkage with k=3",col = c("red", "green","blue"))

Silhclcompletek3 <- silhouette(cutree(complete_linkage_track,k=3),dist(data))
plot(Silhclcompletek3,main = "Silhouette of hierarchical complete linkage with k=3",col = c("red", "green", "blue"))

Silhclaveragek3 <- silhouette(cutree(avg_linkage,k=3),dist(data))
plot(Silhclaveragek3,main = "Silhouette of hierarchical average linkage with k=3",col = c("red", "green", "blue"))

SilhclwardDk3 <- silhouette(cutree(ward_linkage,k=3),dist(data))
plot(SilhclwardDk3,main = "Silhouette of hierarchical ward.D linkage with k=3",col = c("red", "green", "blue"))