attach(Faltoons)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE,alternative = "less")
