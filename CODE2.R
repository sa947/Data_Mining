#install.packages("data.table")
#install.packages("cluster")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("tidyr")
#install.packages("sqldf")
library(data.table)
library (cluster)
library(arules)
library(arulesViz)
library(tidyr)
library(sqldf)


#read data

#dtPlantsRaw <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/plants/plants.data', sep = "\n", sep2 = ",", header = FALSE)
dtPlantsRaw1 <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/plants/plants.data', sep = "\n", sep2 = ",", header = FALSE)
dtPlantsRaw2 <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/plants/plants.data', sep = "\n", sep2 = ",", header = FALSE)

#Question 1
# 1. In the state of CA, which plant has the highest observation?

#creating a column for plant name for Question 1

dtPlantsRaw1$V1 <- sub(",.*", "", dtPlantsRaw1$V1 )
dtPlantsRaw1$V1 <- sub(" .*", "", dtPlantsRaw1$V1 )

# creating a column for state itemset

dtPlantsRaw2$V1 <- sub("^[^,]*,", "", dtPlantsRaw2$V1)

#creating a table with 2 columns: Plants and state itemset

dtPlantsRaw <- data.table (cbind(dtPlantsRaw1$V1, dtPlantsRaw2$V1))


#filtering the records whose itemset have CA

dtPlantsRawCA <- dtPlantsRaw[grep("ca",dtPlantsRaw$V2)]
#dtPlantsRawCA <- sqldf("select * from dtPlantsRaw where V2 like '%ca%' ")

#dtPlantsRawCA <- dtPlantsRaw %>% filter(V2 like '%ca%')

dtPlantsRawCACount <- data.frame(table(dtPlantsRawCA$V1))

dtQ1Index <- which.max(dtPlantsRawCACount$Freq)

dtQ1 <- dtPlantsRawCACount[dtQ1Index,]

#store results in a file
write.csv(dtQ1, file = "Homework3 - Question1.csv")


#Question2
#For the species acalypha,
#find the max patterns of states where they are observed with min-support =10%

#filtering the records with plant name = acalypha

dtPlantsRawAcalypha <- dtPlantsRaw[grep("acalypha",dtPlantsRaw$V1)]
#dtPlantsRawAcalypha <- sqldf("select * from dtPlantsRaw where V1 = 'acalypha'")

## coerce into transactions

dtPlantsRawAcalyphaTrans <- as(strsplit(dtPlantsRawAcalypha$V2, ","), "transactions")

maxPattern <- apriori(dtPlantsRawAcalyphaTrans,
parameter = list(support= 0.1,
target="maximally frequent itemsets",
maxtime = 0,
maxlen = 3
))

#store results in a file
write.csv(inspect(maxPattern), file = "Homework3 - Question2.csv")

#3. for the state of CA, find all the association rules with min-conf 20%

## coerce into transactions

dtPlantsRawCATrans <- as(strsplit(dtPlantsRawCA$V2, ","), "transactions")



rules <- apriori(dtPlantsRawCATrans,
parameter = list(confidence=0.2,
target="rules", maxlen = 5, minlen = 2)) # to mine for rules

#store results in a file sorting according to the lift

write.csv(inspect(sort(rules, by = "lift")), file = "Homework3 - Question3.csv")

