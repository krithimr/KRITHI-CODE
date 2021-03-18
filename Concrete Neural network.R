install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(dplyr)
concrete <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/neural network/concrete.csv")
View(concrete)
attach(concrete)
cor(concrete)
normalized<-function(x){
  return(x-min(x))/(max(x)-min(x))
}
concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalized))
summary(strength)
concrete_norm <- cbind(concrete_norm,strength)
colnames(concrete_norm)[9] <- "strength"
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
#NEURAL NETWORKS
formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse ="+"),sep="~")
concrete_model <- neuralnet(formula = formula_nn,data = concrete_train)
str(concrete_model)
plot(concrete_model)
set.seed(7542)
model_results <- compute(concrete_model,concrete_test[1:8])
str(model_results)
#PREDICTION
predicted_strength <- model_results$net.result
plot(predicted_strength,concrete_test$strength)
model_final<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model_final)
model_final_res<-compute(model_final,concrete_test[1:8])
pred_strn<-model_final_res$net.result
plot(pred_strn,concrete_test$strength)


