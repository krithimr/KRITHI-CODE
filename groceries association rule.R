library(arules)
library(arulesViz)
library(grid)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:5])
itemFrequencyPlot(groceries,topN=10)
#N 1
association_rules<-apriori(groceries,parameter = list(support = 0.005,confidence = 0.5,minlen=2,maxlen=4))
plot(association_rules,method = "scatterplot",jitter=0)
plot(association_rules,method = "grouped",control = list(cex = 0.2))
plot(association_rules,method = "graph")
association_rules <-association_rules [!is.redundant(association_rules )]
arules::inspect(association_rules)
df= data.frame(
  lhs = labels(lhs(association_rules)),
  rhs = labels(rhs(association_rules)), 
  association_rules @quality)
View(df)
association_rules_final1<-apriori(df,parameter = list(support = 0.005,confidence = 0.5,minlen=2,maxlen=4))
plot(association_rules_final1,method = "scatterplot",jitter=0)
plot(association_rules_final1,method = "grouped",control = list(cex = 0.2))
plot(association_rules_final1,method = "graph")

 #N 2
association_rules2<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.1,minlen=3,maxlen=6))
plot(association_rules2,method = "scatterplot",jitter=0)
plot(association_rules2,method = "grouped")
plot(association_rules2,method = "graph")
association_rules2 <-association_rules2 [!is.redundant(association_rules2)]
arules::inspect(association_rules2)
df2= data.frame(
  lhs = labels(lhs(association_rules2)),
  rhs = labels(rhs(association_rules2)), 
  association_rules2 @quality)
View(df2)
association_rules_final2<-apriori(df2,parameter = list(support = 0.002,confidence = 0.1,minlen=3,maxlen=6))
plot(association_rules_final2,method = "scatterplot",jitter=0)
plot(association_rules_final2,method = "grouped",control = list(cex = 0.2))
plot(association_rules_final2,method = "graph")
