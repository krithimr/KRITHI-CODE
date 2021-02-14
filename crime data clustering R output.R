crime_data <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/Clustering/crime_data.csv")
View(crime_data)
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

