install.packages("tibble")
library(tibble)
library(class)
library(corrplot)
zoo <- read_excel("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/KNN/Zoo.xlsx")
View(zoo)
animals<-data.frame(Zoo)
animals_target<-animals[,18]
animals_k<-animals[,1]
category_type<-table(animals$type)
animals$animal.name<-NULL
View(animals)
names(category_type)<-c("mammal","bird","reptile","fish","amphibian","insect","crustacean")
category_type
summary(animals)


 
#knn=1
k_zoo<-1
k_zoo
knn_zoo<-knn.cv(animals,animals_target,k_zoo,prob=TRUE)
pred_zoo<-knn_zoo
confusion_matrix<-table(animals_target,pred_zoo)
accuracy<-(sum(diag(confusion_matrix))/length(animals_target))*100
accuracy
confusion_matrix
#knn=3
k_zoo_2<-3
k_zoo_2
knn_zoo2<-knn.cv(animals,animals_target,k_zoo_2,prob=TRUE)
pred_zoo2<-knn_zoo2
confusion_matrix2<-table(animals_target,pred_zoo2)
accuracy2<-(sum(diag(confusion_matrix2))/length(animals_target))*100
accuracy2
confusion_matrix2

#knn=6
k_zoo_3<-6
k_zoo_3
knn_zoo3<-knn.cv(animals,animals_target,k_zoo_3,prob=TRUE)
pred_zoo3<-knn_zoo3
confusion_matrix3<-table(animals_target,pred_zoo3)
accuracy3<-(sum(diag(confusion_matrix3))/length(animals_target))*100
accuracy3
confusion_matrix3
#knn=8
k_zoo_4<-8
k_zoo_4
knn_zoo4<-knn.cv(animals,animals_target,k_zoo_4,prob=TRUE)
pred_zoo4<-knn_zoo4
confusion_matrix4<-table(animals_target,pred_zoo4)
accuracy4<-(sum(diag(confusion_matrix4))/length(animals_target))*100
accuracy4
confusion_matrix4

#k optimum plot
wss_k<-function(k)
  {
  kmeans(animals,k,nstart=5)$tot.withinss
}
k_values<-1:8
wss_values_k<-map_dbl(k_values,wss_k)
plot(k_values,wss_values_k,type="b",pch=20)

