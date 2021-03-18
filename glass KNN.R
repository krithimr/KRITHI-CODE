glass <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/KNN/glass.csv")
View(glass)
install.packages("corrplot")
library(purrr)
library(caret)
library(class)
library(corrplot)
library(dplyr)
library(ggplot2)
library(caTools)
library(cluster)
std_glass <- scale(glass[,1:9])
glass_std<-cbind(std_glass,glass[10])
head(glass_std)
summary(glass_std)
attach(glass_std)
hist(RI)
hist(Na)
hist(Mg)
hist(Al)
ggplot(glass_std)+geom_bar(aes(x=Type))
correlation<-cor(glass_std)
correlation
corrplot(correlation)
set.seed(150)
test_glass<-glass_std[1:80,]
train_glass<-glass_std[81:214,]

#k=1
knn_glass_1<-knn(train_glass[1:9],test_glass[1:9],train_glass$Type,k=1)
mean_glass_1<-mean(knn_glass_1!=test_glass$Type)
mean_glass_1
confusion_matrix_1<-table(test_glass$Type,knn_glass_1)
confusion_matrix_1
accuracy1<-(sum(diag(confusion_matrix_1))/length(std_glass))*100
accuracy1
knn_error1<-as.data.frame(cbind(k=1,error.type=mean_glass_1))
View(cbind(knn_error1,glass_std))


#k=3
knn_glass_3<-knn(train_glass[1:9],test_glass[1:9],train_glass$Type,k=3)
mean_glass_3<-mean(knn_glass_3!=test_glass$Type)
mean_glass_3
confusion_matrix3<-table(test_glass$Type,knn_glass_3)
confusion_matrix3
accuracy3<-(sum(diag(confusion_matrix3))/length(std_glass))*100
accuracy3
knn_error3<-as.data.frame(cbind(k=3,error.type=mean_glass_3))
View(cbind(knn_error3,glass_std))


#k=6
knn_glass_6<-knn(train_glass[1:9],test_glass[1:9],train_glass$Type,k=6)
mean_glass_6<-mean(knn_glass_6!=test_glass$Type)
mean_glass_6
confusion_matrix6<-table(test_glass$Type,knn_glass_6)
confusion_matrix6
accuracy6<-(sum(diag(confusion_matrix6))/length(std_glass))*100
accuracy6
knn_error6<-as.data.frame(cbind(k=6,error.type=mean_glass_6))
View(cbind(knn_error6,glass_std))


#k=10
knn_glass_10<-knn(train_glass[1:9],test_glass[1:9],train_glass$Type,k=10)
mean_glass_10<-mean(knn_glass_10!=test_glass$Type)
mean_glass_10
confusion_matrix10<-table(test_glass$Type,knn_glass_10)
confusion_matrix10
accuracy10<-(sum(diag(confusion_matrix10))/length(std_glass))*100
accuracy10
knn_error10<-as.data.frame(cbind(k=10,error.type=mean_glass_10))
View(cbind(knn_error10,glass_std))

#optimum k
wss<-function(k){
  kmeans(glass_std,k,nstart=10)$tot.withinss
}
k.values<-1:10
wss.values<-map_dbl(k.values,wss)
plot(k.values,wss.values,type="b",pch=20)
