## Package arules

install.packages("arules")
library(arules)

data("Groceries")
head(Groceries)

write(Groceries, file = "groceries.csv", format = "basket")
gb <- read.transactions("groceries.csv",format = "basket")

summary(gb)
inspect(Groceries[1:3, ])

#Find frequent itemsets
itemFrequency(Groceries[,1])
itemFrequency((Groceries[,1:6]))
itemFrequencyPlot(Groceries,support=.10)
itemFrequencyPlot(Groceries,topN=5)
itemFrequencyPlot(Groceries,topN=20)

#Run the apriori algorithm
m1 <- apriori(Groceries,parameter = list(support=.007, confidence=.25,minlen=2))

#Check the rules
inspect(m1[1:2])
inspect(sort(m1,by="lift")[1:4])