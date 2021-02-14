EastWestAirlines <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/Clustering/EastWestAirlines.csv")
View(EastWestAirlines)
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


#kmeans
EastWestAirlines <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/Clustering/EastWestAirlines.csv")
View(EastWestAirlines)
airlines<-scale(EastWestAirlines[,2:12])
fitKmean<-kmeans(airlines,4)
str(fitKmean)
output_final<-data.frame(EastWestAirlines,fitKmean$cluster)
output_final
View(output_final)
aggregate(crime_data[,2:5],by=list(fitK$cluster),FUN=mean)
wsscluster<-(nrow(crime)-1)*sum(apply(crime,2,var))
for(i in 2:6) wsscluster[i]=sum(kmeans(crime,centers = i)$wsscluster)
plot(1:6,wsscluster,type="b",xlab="clusters",ylab = "wsscluster")
fitK$centers
fitK$cluster
library(cluster)
largeclus<-rbind(cbind(rnorm(4000,0,13),rnorm(4000,0,13),cbind(rnorm(4000,50,13))))
xcl<-clara(largeclus,4,sample=200)
clusplot(xcl)
