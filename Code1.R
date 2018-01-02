install.packages("data.table")
install.packages("cluster")
library(data.table)
library (cluster)

#read data

dtCars <- fread('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data')

#data transformation for "buying"

dtCars$V1[dtCars$V1 == "vhigh"] <- 1
dtCars$V1[dtCars$V1 == "high"] <- 2
dtCars$V1[dtCars$V1 == "med"] <- 3
dtCars$V1[dtCars$V1 == "low"] <- 4

dtCars <- transform(dtCars, V1 = as.numeric(V1)) #change the column datatype to numeric

#interval scaling
dtCars$V1[dtCars$V1 == 1] <- (1-1)/(4-1)
dtCars$V1[dtCars$V1 == 2] <- (2-1)/(4-1)
dtCars$V1[dtCars$V1 == 3] <- (3-1)/(4-1)
dtCars$V1[dtCars$V1 == 4] <- (4-1)/(4-1)


#data transformation for "maint"

dtCars$V2[dtCars$V2 == "vhigh"] <- 1
dtCars$V2[dtCars$V2 == "high"] <- 2
dtCars$V2[dtCars$V2 == "med"] <- 3
dtCars$V2[dtCars$V2 == "low"] <- 4

dtCars <- transform(dtCars, V2 = as.numeric(V2)) #change the column datatype to numeric

#interval scaling
dtCars$V2[dtCars$V2 == 1] <- (1-1)/(4-1)
dtCars$V2[dtCars$V2 == 2] <- (2-1)/(4-1)
dtCars$V2[dtCars$V2 == 3] <- (3-1)/(4-1)
dtCars$V2[dtCars$V2 == 4] <- (4-1)/(4-1)

#data transformation for "doors"

dtCars$V3[dtCars$V3 == "2"] <- 1
dtCars$V3[dtCars$V3 == "3"] <- 2
dtCars$V3[dtCars$V3 == "4"] <- 3
dtCars$V3[dtCars$V3 == "5more"] <- 4

dtCars <- transform(dtCars, V3 = as.numeric(V3)) #change the column datatype to numeric

#interval scaling
dtCars$V3[dtCars$V3 == 1] <- (1-1)/(4-1)
dtCars$V3[dtCars$V3 == 2] <- (2-1)/(4-1)
dtCars$V3[dtCars$V3 == 3] <- (3-1)/(4-1)
dtCars$V3[dtCars$V3 == 4] <- (4-1)/(4-1)


#data transformation for "persons"

dtCars$V4[dtCars$V4 == "2"] <- 1
dtCars$V4[dtCars$V4 == "4"] <- 2
dtCars$V4[dtCars$V4 == "more"] <- 3

dtCars <- transform(dtCars, V4 = as.numeric(V4)) #change the column datatype to numeric

#interval scaling
dtCars$V4[dtCars$V4 == 1] <- (1-1)/(3-1)
dtCars$V4[dtCars$V4 == 2] <- (2-1)/(3-1)
dtCars$V4[dtCars$V4 == 3] <- (3-1)/(3-1)


#data transformation for "lug_boot"

dtCars$V5[dtCars$V5 == "small"] <- 1
dtCars$V5[dtCars$V5 == "med"] <- 2
dtCars$V5[dtCars$V5 == "big"] <- 3

dtCars <- transform(dtCars, V5 = as.numeric(V5)) #change the column datatype to numeric

#interval scaling
dtCars$V5[dtCars$V5 == 1] <- (1-1)/(3-1)
dtCars$V5[dtCars$V5 == 2] <- (2-1)/(3-1)
dtCars$V5[dtCars$V5 == 3] <- (3-1)/(3-1)

#data transformation for "safety"

dtCars$V6[dtCars$V6 == "low"] <- 1
dtCars$V6[dtCars$V6 == "med"] <- 2
dtCars$V6[dtCars$V6 == "high"] <- 3

dtCars <- transform(dtCars, V6 = as.numeric(V6)) #change the column datatype to numeric

#interval scaling
dtCars$V6[dtCars$V6 == 1] <- (1-1)/(3-1)
dtCars$V6[dtCars$V6 == 2] <- (2-1)/(3-1)
dtCars$V6[dtCars$V6 == 3] <- (3-1)/(3-1)

#data transformation for "class values"

dtCars$V7[dtCars$V7 == "unacc"] <- 1
dtCars$V7[dtCars$V7 == "acc"] <- 2
dtCars$V7[dtCars$V7 == "good"] <- 3
dtCars$V7[dtCars$V7 == "vgood"] <- 4

dtCars <- transform(dtCars, V7 = as.numeric(V7)) #change the column datatype to numeric

#interval scaling
dtCars$V7[dtCars$V7 == 1] <- (1-1)/(4-1)
dtCars$V7[dtCars$V7 == 2] <- (2-1)/(4-1)
dtCars$V7[dtCars$V7 == 3] <- (3-1)/(4-1)
dtCars$V7[dtCars$V7 == 4] <- (4-1)/(4-1)


#calculating dissimilarity matrix

dtCarsDist <- as.matrix (dist(dtCars))


#Question 1 - most similar Cars [2260 pairs]

x <- unique(sort(dtCarsDist, decreasing = FALSE))
x1 <- which(dtCarsDist == x[2], arr.ind = TRUE)

#removing opposite pairs

#first sort each row and then remove duplicates
for (i in 1:nrow(x1))
{
  x1[i, ] = sort(x1[i, ])
}
x1 = x1[!duplicated(x1),]



#store results in a file
write.csv(x1, file = "Question1-SimilarCars.csv")

#Question 2 - Dissimilar Cars  [Cars 1 & 1728 AND Cars 82 & 1647]

x2 <- which(dtCarsDist == max(dtCarsDist), arr.ind = TRUE)

#first sort each row and then remove duplicates
for (i in 1:nrow(x2))
{
  x2[i, ] = sort(x2[i, ])
}
x2 = x2[!duplicated(x2),]



#store results in a file
write.csv(x2, file = "Question2-DissimilarCars.csv")

#Question 3

dtCarsCor <- cor(dtCars)
y <- unique(sort(dtCarsCor, decreasing = TRUE))

#Highest positive correlation [V6 and V7]
y1 <- which(dtCarsCor == y[2], arr.ind = TRUE)

#first sort each row and then remove duplicates
for (i in 1:nrow(y1))
{
  y1[i, ] = sort(y1[i, ])
}
y1 = y1[!duplicated(y1),]



#store results in a file
write.csv(y1, file = "Question3-HighlyCorrelatedAttributes.csv")

#scatterplot

plot(dtCars$V6, dtCars$V7, xlab = "Safety", ylab = "Class Values", main = "Safety vs Class Values (Interval-Scaled)")

#Question 4

dtVGoodCars <- subset(dtCars, V7 == 1) #extracted very good cars off the interval - scaled cars dataset

#calculating dissimilarity matrix of very good cars

dtVGoodCarsDist <- as.matrix (dist(dtVGoodCars))

#store results in a file
write.csv(dtVGoodCarsDist, file = "Question4-DissimiarityMatrix.csv")
















