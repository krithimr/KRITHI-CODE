EastWestAirlines <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/Clustering St/EastWestAirlines.csv")
View(EastWestAirlines)

?dist
?hclust
#1.euclidean
airlines<-scale(EastWestAirlines[,2:12])
distairlines<-dist(airlines,method ="euclidean" )
distairlines
fit1<-hclust(distairlines,method ="average" )
plot(fit1)
clusters<-cutree(fit1,k=5)
rect.hclust(fit1,k=2,border="red")
airline_matrix<- as.matrix(clusters)
airline_matrix
output_airclusters<- data.frame('AirlineID'=EastWestAirlines[,1],'Cluster'=clusters)
View(output_airclusters)

#2.manhattan
airlines<-scale(EastWestAirlines[,2:12])
distairlines2<-dist(airlines,method ="manhattan" )
distairlines2
fit2<-hclust(distairlines2,method ="average" )
plot(fit2)
clusters2<-cutree(fit2,k=5)
rect.hclust(fit1,k=2,border="red")
airline_matrix2<- as.matrix(clusters2)
airline_matrix2
output_airclusters2<- data.frame('AirlineID'=EastWestAirlines[,1],'Cluster'=clusters2)
View(output_airclusters2)

#3.canberra
airlines<-scale(EastWestAirlines[,2:12])
distairlines3<-dist(airlines,method ="canberra" )
distairlines3
fit3<-hclust(distairlines3,method ="average" )
plot(fit3)
clusters3<-cutree(fit3,k=5)
rect.hclust(fit3,k=2,border="red")
airline_matrix3<- as.matrix(clusters3)
airline_matrix3
output_airclusters3<- data.frame('AirlineID'=EastWestAirlines[,1],'Cluster'=clusters3)
View(output_airclusters3)


#clust=complete
airlines<-scale(EastWestAirlines[,2:12])
distairlines4<-dist(airlines,method ="euclidean" )
distairlines4
fit4<-hclust(distairlines4,method ="complete" )
plot(fit4)
clusters4<-cutree(fit4,k=5)
rect.hclust(fit4,k=2,border="red")
airline_matrix4<- as.matrix(clusters4)
airline_matrix4
output_airclusters4<- data.frame('AirlineID'=EastWestAirlines[,1],'Cluster'=clusters4)
View(output_airclusters4)

#clust=median
airlines<-scale(EastWestAirlines[,2:12])
distairlines5<-dist(airlines,method ="manhattan" )
distairlines5
fit5<-hclust(distairlines5,method ="median" )
plot(fit5)
clusters5<-cutree(fit5,k=5)
rect.hclust(fit5,k=2,border="red")
airline_matrix5<- as.matrix(clusters5)
airline_matrix5
output_airclusters5<- data.frame('AirlineID'=EastWestAirlines[,1],'Cluster'=clusters5)
View(output_airclusters5)


#kmeans
EastWestAirlines <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/Clustering/EastWestAirlines.csv")
View(EastWestAirlines)
airlines<-scale(EastWestAirlines[,2:12])
fitKmean<-kmeans(airlines,4)
str(fitKmean)
output_final<-data.frame(EastWestAirlines,fitKmean$cluster)
output_final
View(output_final)
aggregate(EastWestAirlines[,2:5],by=list(fitKmean$cluster),FUN=mean)
wsscluster<-(nrow(airlines)-1)*sum(apply(airlines,2,var))
for(i in 2:6) wsscluster[i]=sum(kmeans(airlines,centers = i)$wsscluster)
plot(1:6,wsscluster,type="b",xlab="clusters",ylab = "wsscluster")
fitKmean$centers
fitKmean$cluster
library(cluster)
largeclus<-rbind(cbind(rnorm(4000,0,13),rnorm(4000,0,13),cbind(rnorm(4000,50,13))))
xcl<-clara(largeclus,4,sample=200)
clusplot(xcl)
