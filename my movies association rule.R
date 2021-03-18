library(arules)
library(arulesViz)
library(rmarkdown)
library(dplyr)
my_movies <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/ASSOCIATION RULE/my_movies.csv")
View(my_movies)
class(my_movies)
movie<-my_movies %>% select(6:15)
attach(movie)
colnames(movie)
movie$Sixth.Sense<- factor(movie$Sixth.Sense,levels = c("1","0"),labels = c("Sixth.Sense",""))
movie$Gladiator<- factor(movie$Gladiator,levels = c("1","0"),labels = c("gladiator",""))
movie$LOTR1<- factor(movie$LOTR1,levels = c("1","0"),labels = c("LOTR1",""))
movie$Harry.Potter1<- factor(movie$Harry.Potter1,levels = c("1","0"),labels = c("Harry.Potter1",""))
movie$Patriot<- factor(movie$Patriot,levels = c("1","0"),labels = c("Patriot",""))
movie$LOTR2<- factor(movie$LOTR2,levels = c("1","0"),labels = c("LOTR2",""))
movie$Harry.Potter2<- factor(movie$Harry.Potter2,levels = c("1","0"),labels = c("Harry.Potter2",""))
movie$LOTR<- factor(movie$LOTR,levels = c("1","0"),labels = c("LOTR",""))
movie$Braveheart<- factor(movie$Braveheart,levels = c("1","0"),labels = c("Braveheart",""))
movie$Green.Mile<- factor(movie$Green.Mile,levels = c("1","0"),labels = c("Green.Mile",""))
movie_transaction <- as(movie,"transactions")

#N 1
itemFrequencyPlot(movie_transaction,topN=10)
rules1 <- apriori(movie_transaction,parameter=list(support=0.2, confidence = 0.5,minlen=5,maxlen=10))
plot(head(sort(rules1)), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules1, by = "lift")), method = "graph", control = list(cex = 1.0))
plot(rules1,jitter=0)
rules1 <- rules1[!is.redundant(rules1)]
arules::inspect(rules1)
df = data.frame(
  lhs = labels(lhs(rules1)),
  rhs = labels(rhs(rules1)), 
  rules1@quality)
View(df)
rules1 <- apriori(df,parameter=list(support=0.2, confidence = 0.5,minlen=5,maxlen=10))
plot(head(sort(rules1)), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules1, by = "lift")), method = "graph", control = list(cex = 1.0))
#N 2
rules2 <- apriori(movie_transaction,parameter=list(support=0.05, confidence = 0.10,minlen=6,maxlen=15))
plot(head(sort(rules2)), method = "grouped")
plot(head(sort(rules2, by = "lift")), method = "graph", control = list(cex = 1.0))
plot(rules2,jitter=0)
rules2 <- rules2[!is.redundant(rules2)]
arules::inspect(rules2)
df2 = data.frame(
  lhs = labels(lhs(rules2)),
  rhs = labels(rhs(rules2)), 
  rules2@quality)
View(df2)
rules2 <- apriori(df2,parameter=list(support=0.05, confidence = 0.10,minlen=6,maxlen=15))
plot(head(sort(rules2)), method = "grouped")
plot(head(sort(rules2, by = "lift")), method = "graph", control = list(cex = 1.0))
