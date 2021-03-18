wine <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/PCA St/wine.csv")
View(wine)
winedata<-scale(wine[,2:14])
pca<-princomp(winedata, scores = TRUE)
summary(pca)
pca$scores
plot(pca)
plot(pca$scores[,1:2],col="Blue",cex=0.2)
biplot(pca)
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")
text(pca$scores[,1:2],labels =c(1:25),cex=0.5)
plot(pca$scores[,1:3],col="Blue",cex=0.2)
text(pca$scores[,1:3],labels =c(1:25),cex=0.7)
pca$scores[,1:3]
wine<-cbind(wine,pca$scores[,1:3])
?dist
?hclust

#clustering of 3 PCA

#euclidean
clusterdata<-wine[,15:17]
cluster_scale<-scale(clusterdata)
distanceclsut<-dist(cluster_scale,method="euclidean")
fitclust<-hclust(distanceclsut,method="average")
plot(fitclust)
dendrogram<-cutree(fitclust,4)
rect.hclust(fitclust,k=5,border="red")
winecomp<-as.matrix(dendrogram)
View(winecomp)
finaloutput<-cbind(winecomp,wine)
output_wineclusters<- data.frame('Winecomponents'=wine[,1],clusterdata,'Cluster'=dendrogram)
View(output_wineclusters)

#manhattan
clusterdata<-wine[,15:17]
cluster_scale2<-scale(clusterdata)
distanceclsut2<-dist(cluster_scale2,method="manhattan")
fitclust2<-hclust(distanceclsut2,method="average")
plot(fitclust2)
dendrogram2<-cutree(fitclust2,6)
winecomp2<-as.matrix(dendrogram2)
View(winecomp2)
finaloutput2<-cbind(winecomp2,wine)
output_wineclusters2<- data.frame('Winecomponents'=wine[,1],clusterdata,'Cluster'=dendrogram2)
View(output_wineclusters2)

#clust= complete
clusterdata<-wine[,15:17]
cluster_scale3<-scale(clusterdata)
distanceclsut3<-dist(cluster_scale3,method="euclidean")
fitclust3<-hclust(distanceclsut3,method="complete")
plot(fitclust3)
dendrogram3<-cutree(fitclust3,5)
winecomp3<-as.matrix(dendrogram3)
View(winecomp3)
finaloutput3<-cbind(winecomp3,wine)
output_wineclusters3<- data.frame('Winecomponents'=wine[,1],clusterdata,'Cluster'=dendrogram3)
View(output_wineclusters3)


#k means clustering
clusterdata<-wine[,15:17]
cluster_scale<-scale(clusterdata)
fitKmean2<-kmeans(cluster_scale,4)
str(fitKmean2)
outputKmean<- data.frame(wine,fitKmean2$cluster)
View(outputKmean)
aggregate(wine[,15:17],by=list(fitKmean2$cluster),FUN=mean)
wsscluster<-(nrow(cluster_scale)-1)*sum(apply(cluster_scale,2,var))
for(i in 15:17) wsscluster[i]=sum(kmeans(cluster_scale,centers = i)$wsscluster)
plot(1:17,wsscluster,type="b",xlab="clusters",ylab = "wsscluster")
fitKmean2$centers
fitKmean2$cluster
