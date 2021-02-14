creditcard<- read.csv(file.choose())
View(creditcard)
library(dplyr)
library(caret)
library(ggplot2)
library(tseries)
creditcard1<-select(creditcard,-1)
str(creditcard1)
head(creditcard1)
count(card,0)
#plot visualization
summary(creditcard1)
attach(creditcard1)
ggplot(creditcard1)+geom_bar(aes(x=card))
hist(age)
hist(reports)
hist(income)
hist(share)
hist(expenditure)
ggplot(creditcard1)+geom_bar(aes(x=owner))
ggplot(creditcard1)+geom_bar(aes(x=selfemp))
boxplot(creditcard1)
View(creditcard1)
jarque.bera.test(card)
#model
model.lm <- lm(card~.,data=creditcard1)
summary(model.lm)
pred1 <- predict(model.lm,creditcard1)
pred1
plot(pred1)
colnames(creditcard1)
logit <- glm(card~factor(reports)+age+income+share+expenditure+factor(owner)+factor(selfemp)+factor(majorcards)+months+dependents+active,data=creditcard1,family = "binomial")
summary(logit)

exp(coef(logit))
prob <- predict(logit,creditcard1,type="response")
prob
confusion<-table(prob>0.5,creditcard1$card)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
specificity<- 141/(141+155)
specificity
sensitivity<- 1014/(1014+9)
sensitivity
pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
creditcard1[,"prob"] <- prob
creditcard1[,"pred_values"] <- pred_values
creditcard1[,"yes_no"] <- yes_no
View(creditcard1)
table(creditcard1$card,creditcard1$pred_values)

