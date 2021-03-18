library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
forestfires <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/neural network/forestfires.csv")
attach(forestfires)
View(forestfires)
summary(forestfires)
normalize<-function(y){
  return ( (y-min(y))/(max(y)-min(y)))
}
temp<-normalize(temp)
wind<-normalize(wind)
RH<-normalize(RH)
rain<-normalize(rain)
size_category_dummy<-ifelse(size_category=='large',1,0)
forestfires<-cbind(forestfires,size_category_dummy)
forest_train<-forestfires[1:410,]
forest_test<-forestfires[411:517,]
#neural network
set.seed(5500)
#using size_category
forest_model<-neuralnet(size_category~temp+wind+RH+rain,data=forest_train)
summary(forest_model)
predict_model<-predict(forest_model,forest_test)
plot(forest_model,rep="best")
plotnet(forest_model)
#using size_dummy
forest_model<-neuralnet(size_category_dummy~temp+wind+RH+rain,data=forest_train)
summary(forest_model)
predict_model<-predict(forest_model,forest_test)
plot(forest_model,rep="best")
plotnet(forest_model)
model<-neuralnet::compute(forest_model,forest_test)
predicted_pro<-model$net.result
cor(predicted_pro,forest_test$size_category_dummy)
plot(predicted_pro,forest_test$size_category_dummy)
forest_model<-neuralnet(size_category_dummy~temp+wind+RH+rain,data=forest_train)
summary(forest_model)
predict_model<-predict(forest_model,forest_test)
plot(forest_model,rep="best")
plotnet(forest_model)
cor(predicted_pro,forest_test$size_category_dummy)
#layer2
forest_model2<-neuralnet(size_category_dummy~temp+wind+RH+rain,data=forest_train,hidden=5)
summary(forest_model2)
predict_model<-predict(forest_model2,forest_test)
plot(forest_model2,rep="best")
plotnet(forest_model2)
model2<-neuralnet::compute(forest_model2,forest_test)
predicted_pro2<-model2$net.result
cor(predicted_pro2,forest_test$size_category_dummy)
plot(predicted_pro2,forest_test$size_category_dummy)


