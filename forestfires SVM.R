forestfires<-read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/Support vector machines st/forestfires (1).csv")
View(forestfires)
library(dplyr)
library(caret)
library(kernlab)
library(ROCR)
library(ggplot2)
library(tseries)
attach(forestfires)
head(forestfires)
normalize<-function(y){
  return ( (y-min(y))/(max(y)-min(y)))
}
temp<-normalize(temp)
wind<-normalize(wind)
RH<-normalize(RH)
rain<-normalize(rain)
hist(temp)
jarque.bera.test(temp)
hist(wind)
jarque.bera.test(wind)
hist(RH)
jarque.bera.test(RH)
ggplot(forestfires)+geom_bar(aes(x=month))
hist(area)
View(forestfires)
attach(forestfires)
forestarea<-mutate(forestfires,x=log(area+1))
hist(forestarea$x)
summary(forestfires)
set.seed(175)
forest_train<-forestfires[1:400,]
forest_test<-forestfires[401:517,]
?ksvm
#kernal vanilladot
Kernal_model<-ksvm(size_category~rain+temp+wind+RH,forest_train,kernel="vanilladot")
Kernal_model
predict_kernel<-predict(Kernal_model,forest_test)
confusion<-table(predict_kernel,forest_test$size_category)
confusion
prop.table(table(predict_kernel,forest_test$size_category))
mean(predict_kernel==forest_test$size_category)
plot()
#kernal rbfdot
Kernal_rbfdot<-ksvm(size_category~rain+temp+wind+RH,forest_train,kernel="rbfdot")
Kernal_rbfdot
predict_rbfdot<-predict(Kernal_rbfdot,forest_test)
confusion_rbfdot<-table(predict_rbfdot,forest_test$size_category)
confusion_rbfdot
mean(predict_rbfdot==forest_test$size_category)
#kernal polydot
Kernal_polydot<-ksvm(size_category~rain+temp+wind+RH,forest_train,kernel="polydot")
Kernal_polydot
predict_polydot<-predict(Kernal_polydot,forest_test)
confusion_polydot<-table(predict_polydot,forest_test$size_category)
confusion_polydot
mean(predict_polydot==forest_test$size_category)
#kernal tanhdot
Kernal_tanhdot<-ksvm(size_category~rain+temp+wind+RH,forest_train,kernel="tanhdot")
Kernal_tanhdot
predict_tanhdot<-predict(Kernal_tanhdot,forest_test)
confusion_tanhdot<-table(predict_tanhdot,forest_test$size_category)
confusion_tanhdot
mean(predict_tanhdot==forest_test$size_category)