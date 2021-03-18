library(readr)
Airlines_Data <- read_excel("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/forecasting/Airlines+Data.xlsx")
View(Airlines_Data)
install.packages("forecast")
library(forecast)
library(fpp)
library(smooth)
windows()
airlines<-data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(airlines)<-month.abb
View(airlines)
Airlines_Data_original<-cbind(Airlines_Data,airlines)
View(Airlines_Data_original)
Airlines_Data_original["t"]<-c(1:96)
View(Airlines_Data_original)
Airlines_Data_original["t_square"]<-Airlines_Data_original["t"]*Airlines_Data_original["t"]
View(Airlines_Data_original)
attach(Airlines_Data_original)
install.packages("plotly")
library(plotly)
plot_ly(x=t,y=Passengers,type="scatter",mode="lines",data=Airlines_Data_original)
train_data<-Airlines_Data_original[1:67,]
test_data<-Airlines_Data_original[69:96,]
Airlines_Data
#Linear model
linear_model<-lm(Passengers~t,data=train_data)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval = 'predict',newdata=test_data))
rmse_lm_linear<-sqrt(mean((test_data$Passengers-linear_pred$fit)^2,na.rm=T))
rmse_lm_linear

#Exponential model
exponential_model<-lm(log(Passengers)~t,data=train_data)
summary(exponential_model)
exponential_pred<-data.frame(predict(exponential_model,interval = 'predict',newdata=test_data))
rmse_lm_expo<-sqrt(mean((test_data$Passengers-exponential_pred$fit)^2,na.rm=T))
rmse_lm_expo

#Quadratic model
quadratic_model<-lm(Passengers~t+t_square,data=train_data)
summary(quadratic_model)  
quad_pred<-data.frame(predict(quadratic_model,interval = 'predict',newdata=test_data))
rmse_lm_quad<-sqrt(mean((test_data$Passengers-quad_pred$fit)^2,na.rm=T))
rmse_lm_quad

#Additive seasonality
additive_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(additive_model)
additive_pred<-data.frame(predict(additive_model,interval = 'predict',newdata =test_data))
rmse_lm_add<-sqrt(mean((test_data$Passengers-additive_pred$fit)^2,na.rm=T))
rmse_lm_add

#Additive seasonality with linear
additive_linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(additive_linear_model)
additive_lin_pred<-data.frame(predict(additive_linear_model,interval = 'predict',newdata =test_data))
rmse_lm_addlin<-sqrt(mean((test_data$Passengers-additive_lin_pred$fit)^2,na.rm=T))
rmse_lm_addlin

#Additive Seasonality with Quadratic 
additive_quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(additive_quad_model)
additive_quad_pred<-data.frame(predict(additive_quad_model,interval = 'predict',newdata =test_data))
rmse_lm_addquad<-sqrt(mean((test_data$Passengers-additive_quad_pred$fit)^2,na.rm=T))
rmse_lm_addquad

#Multiplicative Seasonality
multi_model<-lm(log(Passengers)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(multi_model)
multi_pred<-data.frame(predict(multi_model,interval = 'predict',newdata =test_data))
rmse_multi<-sqrt(mean((test_data$Passengers-multi_pred$fit)^2,na.rm=T))
rmse_multi

#multiplicative seasonality with Linear
multi_lin_model<-lm(log(Passengers)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(multi_lin_model)
multi_lin_pred<-data.frame(predict(multi_lin_model,interval = 'predict',newdata =test_data))
rmse_multi_lin<-sqrt(mean((test_data$Passengers-multi_lin_pred$fit)^2,na.rm=T))
rmse_multi_lin

table_rmse<-data.frame(c("rmse_lm_linear","rmse_lm_expo","rmse_lm_Quad","rmse_lm_add","rmse_lm_addlin","rmse_lm_addquad","rmse_multi","rmse_multi_lin"),c(rmse_lm_linear,rmse_lm_expo,rmse_lm_quad,rmse_lm_add,rmse_lm_addlin,rmse_lm_addquad,rmse_multi,rmse_multi_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


final_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Airlines_Data_original)
summary(final_model)
resid_final<-residuals(final_model)
resid_final[1:10]
windows()
acf(resid_final,lag.max=10)
arima_model<-arima(resid_final,order=c(1,0,0))
summary(arima_model)
str(arima_model)
pred_res<- predict(arima(arima_model$residuals,order=c(1,0,0)),n.head=12)
str(pred_res)
pred_res$pred
acf(arima_model$residuals)

library(readxl)
View(test_data)
pred_new<-data.frame(predict(final_model,newdata=test_data,interval='predict'))
View(pred_new)
pred_new$fit
plot(pred_new$fit)
write.csv(Airlines_Data_original,file="Airlines.csv",col.names=F,row.names = F)
