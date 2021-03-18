Startups <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/neural network/50_Startups (1).csv")
View(Startups)
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(neu)
library(dplyr)
library(plyr)
library(caret)
attach(Startups)
Startups2<-select(Startups,-4)
View(Startups2)
#Relationship between variables with that of Profit

cor(Startups2)
pairs(Startups)
plot(R.D.Spend,Profit)                     
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
normalized<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(Startups2,FUN=normalized))
summary(startups_norm)
str(Startups2)
set.seed(751)
startup_train<-startups_norm[1:40,]
startup_test<-startups_norm[41:50,]
startups_model<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+Profit,data=startup_train)
summary(startups_model)
plot(startups_model,rep="best")
plotnet(startups_model)

#Predict model
set.seed(1234)
model<-neuralnet::compute(startups_model,startup_test[1:4])
predicted_pro<-model$net.result
cor(predicted_pro,startup_test$Profit)
plot(predicted_pro,startup_test$Profit)
table(predicted_pro,startup_test$Profit)

startups_model2<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+Profit,data=startup_train,hidden=5)
summary(startups_model2)
plot(startups_model2,rep="best")
plotnet(startups_model2)
model2<-neuralnet::compute(startups_model2,startup_test[1:4])
predicted_pro2<-model2$net.result
cor(predicted_pro2,startup_test$Profit)
plot(predicted_pro2,startup_test$Profit)
table(predicted_pro2,startup_test$Profit)
