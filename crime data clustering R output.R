crime_data <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/Clustering St/crime_data.csv")
View(crime_data)

#euclidean
crime<-scale(crime_data[,2:5])
distance1<-dist(crime,method ="euclidean" )
distance1
fit1<-hclust(distance1,method ="average" )
plot(fit1)
clusters<-cutree(fit1,k=4)
rect.hclust(fit1,k=4,border="red")
crime_matrix<- as.matrix(clusters)
output_clusters<- data.frame('countries'=crime_data[,1],'Cluster'=clusters)
View(output_clusters)

#manhattan
crime<-scale(crime_data[,2:5])
distance2<-dist(crime,method ="manhattan" )
distance2
fit2<-hclust(distance2,method ="average" )
plot(fit2)
clusters2<-cutree(fit2,k=4)
rect.hclust(fit2,k=4,border="blue")
crime_matrix2<- as.matrix(clusters2)
output_clusters2<- data.frame('countries'=crime_data[,1],'Cluster'=clusters2)
View(output_clusters2)

#canberra
crime<-scale(crime_data[,2:5])
distance3<-dist(crime,method ="canberra" )
distance3
fit3<-hclust(distance3,method ="average" )
plot(fit3)
clusters3<-cutree(fit3,k=4)
rect.hclust(fit3,k=4,border="blue")
crime_matrix3<- as.matrix(clusters3)
output_clusters3<- data.frame('countries'=crime_data[,1],'Cluster'=clusters3)
View(output_clusters3)

#clust=complete
crime<-scale(crime_data[,2:5])
distance4<-dist(crime,method ="manhattan" )
distance4
fit4<-hclust(distance4,method ="complete" )
plot(fit4)
clusters4<-cutree(fit4,k=4)
rect.hclust(fit4,k=4,border="blue")
crime_matrix4<- as.matrix(clusters4)
output_clusters4<- data.frame('countries'=crime_data[,1],'Cluster'=clusters4)
View(output_clusters4)

#clust=median
crime<-scale(crime_data[,2:5])
distance5<-dist(crime,method ="manhattan" )
distance5
fit5<-hclust(distance5,method ="average" )
plot(fit5)
clusters5<-cutree(fit5,k=4)
rect.hclust(fit5,k=4,border="blue")
crime_matrix5<- as.matrix(clusters5)
output_clusters5<- data.frame('countries'=crime_data[,1],'Cluster'=clusters5)
View(output_clusters5)


#K means clustering
crime_data <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/Clustering/crime_data.csv")
View(crime_data)
crime<-scale(crime_data[,2:5])
fitK<-kmeans(crime,4)
str(fitK)
output_final<-data.frame(crime_data,fitK$cluster)
output_final
aggregate(crime_data[,2:5],by=list(fitK$cluster),FUN=mean)
wsscluster<-(nrow(crime)-1)*sum(apply(crime,2,var))
for(i in 2:6) wsscluster[i]=sum(kmeans(crime,centers = i)$wsscluster)
plot(1:6,wsscluster,type="b",xlab="clusters",ylab = "wsscluster")
fitK$centers
fitK$cluster
#best cluster is 2

