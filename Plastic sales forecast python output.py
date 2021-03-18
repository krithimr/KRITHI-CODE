# -*- coding: utf-8 -*-
"""
Created on Thu Aug 20 18:43:16 2020

@author: DELL
"""
import pandas as pd
import numpy as np
plastic_sales=pd.read_csv("C:\\Users\\DELL\\Desktop\\DATA SCIENCE\\ASSIGNMENTS\\forecasting\\PlasticSales.csv")
month=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec']
p=plastic_sales["Month"][0]
plastic_sales['months']=0
str(plastic_sales)
for i in range (60):
    p=plastic_sales["Month"][i]
    plastic_sales['months'][i]= p[0:3]
month_dum=pd.DataFrame(pd.get_dummies(plastic_sales['months']))
plasticsales1=pd.concat([plastic_sales,month_dum],axis=1)
plasticsales1["t"]=np.arange(1,61)
plasticsales1["t_square"]=plasticsales1["t"]*plasticsales1["t"]
plasticsales1.columns
plasticsales1.Sales.plot()
#the graph shows a cyclical treand with seasonality
test_data=plasticsales1.head(48)
train_data=plasticsales1.tail(12)
#linear model
import statsmodels.formula.api as smf
linear_model=smf.ols('Sales~t',data=train_data).fit()
linear_model.summary()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(test_data['t'])))
rmse_linear = np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(pred_linear))**2))
rmse_linear

#exponential model
exponential_model= smf.ols('np.log(Sales)~t',data=train_data).fit()
exponential_model.summary()
pred_expo= pd.Series(exponential_model.predict(pd.DataFrame(test_data['t'])))
rmse_expo=np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(pred_expo))**2))
rmse_expo

#quadratic model
quadratic_model= smf.ols('Sales~t+t_square',data=train_data).fit()
quadratic_model.summary()
pred_Quad = pd.Series(quadratic_model.predict(test_data[["t","t_square"]]))
rmse_Quad = np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(pred_Quad))**2))
rmse_Quad

#additive seasonality
additive_model = smf.ols('Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=train_data).fit()
additive_model.summary()
pred_additive = pd.Series(additive_model.predict(test_data[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov']]))
rmse_additive = np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(pred_additive))**2))
rmse_additive

#additive seasonality with quadratic
additive_Quad = smf.ols('Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=train_data).fit()
additive_Quad.summary()
pred_add_quad= pd.Series(additive_Quad.predict(test_data[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_square']]))
rmse_additive_quad = np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(pred_add_quad))**2))
rmse_additive_quad

#Multiplicative seasonality
multi_model = smf.ols('np.log(Sales)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = train_data).fit()
multi_model.summary()
pred_multi= pd.Series(multi_model.predict(test_data))
rmse_multi= np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(np.exp(pred_multi)))**2))
rmse_multi

#Multiplicative Additive Seasonality 

multi_add= smf.ols('np.log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = train_data).fit()
multi_add.summary()
pred_multi_add = pd.Series(multi_add.predict(test_data))
rmse_multi_add = np.sqrt(np.mean((np.array(test_data['Sales'])-np.array(np.exp(pred_multi_add)))**2))
rmse_multi_add 

rmse = {"MODEL":pd.Series(["rmse_linear","rmse_expo","rmse_Quad","rmse_additive","rmse_additive_quad","rmse_multi","rmse_multi_add"]),"RMSE_Values":pd.Series([rmse_linear,rmse_expo,rmse_Quad,rmse_additive,rmse_additive_quad,rmse_multi,rmse_multi_add])}
rmse_table=pd.DataFrame(rmse)
rmse_table
 
#rmse is lowest for multiplicative model

final_model = smf.ols('np.log(Sales)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=plasticsales1).fit()
pred_final = pd.Series(final_model.predict(plasticsales1))
pred_final
plasticsales1["forecasted_sales"] = pd.Series(pred_final)
plasticsales1.plot()
