BuyerRatio <- read.csv("C:/Users/DELL/Desktop/DATA SCIENCE/HYPOTHESIS TESTING/BuyerRatio.csv")
 View(BuyerRatio)
attach(BuyerRatio)
table(Observed.Values,Male,female)
t1<- prop.table(Male)      
t1
t2<-prop.table(female)
t2
chisq.test(table(t1,t2))
