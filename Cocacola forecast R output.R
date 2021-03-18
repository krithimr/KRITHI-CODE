library(readr)
CocaColasales <- read_excel("C:/Users/DELL/Desktop/DATA SCIENCE/ASSIGNMENTS/forecasting/CocaCola_Sales_Rawdata.xlsx")
View(CocaColasales)
windows()
library(dplyr)
timet<-data.frame(Date=seq(as.Date("1986-01-01"),length.out =42,by="3 months" ))
timet1<-data.table::dcast(timet,Date~paste0("Q",lubridate::quarter(timet$Date)),length,value.var="Date")
View(timet1)
CocaCola_sales<-cbind(CocaColasales,timet1)
View(CocaCola_sales)
CocaCola_sales["t"]<-c(1:42)
View(CocaCola_sales)
CocaCola_sales["t_square"]<-CocaCola_sales["t"]*CocaCola_sales["t"]
View(CocaCola_sales)
attach(CocaCola_sales)
train_data<-CocaCola_sales[1:30,]
test_data<-CocaCola_sales[31:42,]
CocaCola_sales
library(plotly)
plot_ly(x=t,y=Sales,type="scatter",mode="lines",data=CocaCola_sales)
#Linear model
linear_model<-lm(Sales~t,data=train_data)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval = 'predict',newdata=test_data))
rmse_lm_linear<-sqrt(mean((test_data$Sales-linear_pred$fit)^2,na.rm=T))
rmse_lm_linear

#Exponential model
exponential_model<-lm(log(Sales)~t,data=train_data)
summary(exponential_model)
exponential_pred<-data.frame(predict(exponential_model,interval = 'predict',newdata=test_data))
rmse_lm_expo<-sqrt(mean((test_data$Sales-exponential_pred$fit)^2,na.rm=T))
rmse_lm_expo

#Quadratic model
quadratic_model<-lm(Sales~t+t_square,data=train_data)
summary(quadratic_model)  
quad_pred<-data.frame(predict(quadratic_model,interval = 'predict',newdata=test_data))
rmse_lm_quad<-sqrt(mean((test_data$Sales-quad_pred$fit)^2,na.rm=T))
rmse_lm_quad

#Additive seasonality
additive_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train_data)
summary(additive_model)
additive_pred<-data.frame(predict(additive_model,interval = 'predict',newdata =test_data))
rmse_lm_add<-sqrt(mean((test_data$Sales-additive_pred$fit)^2,na.rm=T))
rmse_lm_add

#Additive seasonality with linear
additive_linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train_data)
summary(additive_linear_model)
additive_lin_pred<-data.frame(predict(additive_linear_model,interval = 'predict',newdata =test_data))
rmse_lm_addlin<-sqrt(mean((test_data$Sales-additive_lin_pred$fit)^2,na.rm=T))
rmse_lm_addlin

#Additive Seasonality with Quadratic 
additive_quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train_data)
summary(additive_quad_model)
additive_quad_pred<-data.frame(predict(additive_quad_model,interval = 'predict',newdata =test_data))
rmse_lm_addquad<-sqrt(mean((test_data$Sales-additive_quad_pred$fit)^2,na.rm=T))
rmse_lm_addquad

#Multiplicative Seasonality
multi_model<-lm(log(Sales)~Q1+Q2+Q3+Q4,data=train_data)
summary(multi_model)
multi_pred<-data.frame(predict(multi_model,interval = 'predict',newdata =test_data))
rmse_multi<-sqrt(mean((test_data$Sales-multi_pred$fit)^2,na.rm=T))
rmse_multi

#multiplicative seasonality with Linear
multi_lin_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_data)
summary(multi_lin_model)
multi_lin_pred<-data.frame(predict(multi_lin_model,interval = 'predict',newdata =test_data))
rmse_multi_lin<-sqrt(mean((test_data$Sales-multi_lin_pred$fit)^2,na.rm=T))
rmse_multi_lin

table_rmse<-data.frame(c("rmse_lm_linear","rmse_lm_expo","rmse_lm_Quad","rmse_lm_add","rmse_lm_addlin","rmse_lm_addquad","rmse_multi","rmse_multi_lin"),c(rmse_lm_linear,rmse_lm_expo,rmse_lm_quad,rmse_lm_add,rmse_lm_addlin,rmse_lm_addquad,rmse_multi,rmse_multi_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
 
#additive seasonality with linear has the lowest RMSE 

final_model<-lm(Sales~Q1+Q2+Q3+Q4,data=CocaCola_sales)
summary(final_model)
resid_final<-residuals(final_model)
resid_final[1:10]
windows()
acf(resid_final,lag.max=10)
arima_model<-arima(resid_final,order=c(1,0,0))
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
write.csv(CocaCola_sales,file="corona_forecast.csv",col.names=F,row.names = F)
