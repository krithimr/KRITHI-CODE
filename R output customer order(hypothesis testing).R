attach(`Costomer+OrderForm`)
table(Country,Defective)
t2 <- prop.table(table(Defective))
t1 <- table(Country)
t2
t1
chisq.test(table(Country,Defective))

