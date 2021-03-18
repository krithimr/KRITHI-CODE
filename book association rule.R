library(arules)
library(arulesViz)
library(car)
library(carData)
library(mvinfluence)
book <- read.csv("C:/Users/DELL/Desktop/New folder/DATA SCIENCE/ASSIGNMENTS/COMPLETED/ASSOCIATION RULE/book (1).csv")
colnames(book)
book$ChildBks<- factor(book$ChildBks,levels = c("1","0"),labels = c("ChildBks",""))
book$YouthBks<- factor(book$YouthBks,levels = c("1","0"),labels = c("YouthBks",""))
book$CookBks<- factor(book$CookBks,levels = c("1","0"),labels = c("CookBks",""))
book$DoItYBks<- factor(book$DoItYBks,levels = c("1","0"),labels = c("DoItYBks",""))
book$RefBks<- factor(book$RefBks,levels = c("1","0"),labels = c("RefBks",""))
book$ArtBks<- factor(book$ArtBks,levels = c("1","0"),labels = c("ArtBks",""))
book$GeogBks<- factor(book$GeogBks,levels = c("1","0"),labels = c("GeogBks",""))
book$ItalCook<- factor(book$ItalCook,levels = c("1","0"),labels = c("ItalCook",""))
book$ItalAtlas<- factor(book$ItalAtlas,levels = c("1","0"),labels = c("ItalAtlas",""))
book$ItalArt<- factor(book$ItalArt,levels = c("1","0"),labels = c("ItalArt",""))
book$Florence<- factor(book$Florence,levels = c("1","0"),labels = c("Florence",""))
books <- as(book,"transactions")
itemFrequencyPlot(books,topN=15)
#N 1
rules1_book <- apriori(books, parameter = list(support= 0.005, confidence = 0.50, minlen = 2, maxlen = 4))
?apriori
plot(head(sort(rules1_book), n = 10), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules1_book, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0))
plot(rules1_book,jitter=0)
inspect(rules1_book[is.redundant(rules1_book)])
rules1_book <- rules1_book[!is.redundant(rules1_book)]
arules::inspect(rules1_book)
df = data.frame(
  lhs = labels(lhs(rules1_book)),
  rhs = labels(rhs(rules1_book)), 
  rules1_book@quality)
View(df)
rules1_book_final <- apriori(df, parameter = list(support= 0.005, confidence = 0.50, minlen = 2, maxlen = 4))
plot(head(sort(rules1_book_final), n = 10), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules1_book_final, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0))

#N 2
rules2_book <- apriori(books, parameter = list(support= 0.02, confidence = 0.10, minlen = 3, maxlen = 6))
plot(head(sort(rules2_book), n = 12), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules2_book, by = "lift"), n = 12), method = "graph", control = list(cex = 1.0))
plot(rules2_book,jitter=0)
inspect(rules2_book[is.redundant(rules2_book)])
rules2_book <- rules2_book[!is.redundant(rules2_book)]
arules::inspect(rules2_book)
df2 = data.frame(
  lhs = labels(lhs(rules2_book)),
  rhs = labels(rhs(rules2_book)), 
  rules2_book@quality)
View(df2)
rules2_book_final<- apriori(df2, parameter = list(support= 0.02, confidence = 0.10, minlen = 3, maxlen = 6))
plot(head(sort(rules2_book_final), n = 12), method = "grouped", control = list(cex = 0.2))
plot(head(sort(rules2_book_final, by = "lift"), n = 12), method = "graph", control = list(cex = 1.0))

