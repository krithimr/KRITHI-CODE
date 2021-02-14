# -*- coding: utf-8 -*-
"""
Created on Thu Aug  6 15:44:56 2020

@author: DELL
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
Salary_Data.columns
salarydata=pd.read_csv("C:\\Users\\DELL\\Desktop\\DATA SCIENCE\\ASSIGNMENTS\\SIMPLE LINEAR REGRESSION\\Salary_Data.csv")
salarydata.columns
plt.boxplot(salarydata.YearsExperience)
plt.hist(salarydata.YearsExperience)
plt.boxplot(salarydata.Salary)
plt.plot(salarydata.YearsExperience,"ro"); plt.xlabel("YearsExperience"); plt.ylabel("Salary")
salarydata.Salary.corr(salarydata.YearsExperience)
import statsmodels.formula.api as smf
model= smf.ols("Salary~YearsExperience",data= salarydata).fit()
model.params
model.summary()
print(model.conf_int(0.05))
pred_model= model.predict(salarydata)
import matplotlib.pyplot as plt
plt.scatter(x=salarydata['YearsExperience'],y=salarydata['Salary'],color='red');plt.plot(salarydata['YearsExperience'],pred_model,color='black');plt.xlabel['YearsExperience'];plt.ylabel('Salary')
pred_model.corr(salarydata.Salary)
model2= smf.ols("Salary~np.log(YearsExperience)",data= salarydata).fit()
model2.summary()
print(model2.conf_int(0.05))
pred_model2= model2.predict(salarydata)
pred_model2.corr(salarydata.Salary)
model3= smf.ols("Salary~np.sqrt(YearsExperience)",data= salarydata).fit()
model3.summary()
print(model3.conf_int(0.05))
pred_model3= model3.predict(salarydata)
pred_model3.corr(salarydata.Salary)
