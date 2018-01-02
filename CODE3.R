

#install.packages("data.table")
#install.packages("dplyr")
#install.packages ("cluster")
#install.packages("Gmedian")
#install.packages("skmeans")
#install.packages("stylo")
#install.packages ("flexclust")
#install.packages(c("factoextra", "fpc", "NbClust"))

library(data.table)
library(dplyr) 
library(cluster)
library(Gmedian)
library(skmeans)
library(stylo)
library(flexclust)
library(factoextra)
library(fpc)
library(NbClust)


#read raw data

dtRaw <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)

#Drop all the attributes that have more than 50% missing values.

removeindex <- c()
for (i in 1:ncol(dtRaw))
{
  j <- dtRaw[dtRaw[i] == "?", ]
  
  if ((nrow(j)/nrow(dtRaw)) > 0.5) #check for 50%
  {
    removeindex <- c(removeindex, i)
  }
  
}

if (length(removeindex) == 1)
{
  dtRaw[removeindex] <- NULL
} else {
  for (l in 1:length(removeindex))
  {
    dtRaw[removeindex[l]] <- NULL
  }
}

#data clean-up for clustering

dtClean <- dtRaw
p <- nrow(dtRaw)


#race (nominal)
for (a in unique(dtRaw$race))
{
  dtClean$race[dtClean$race == a] <- ((p - length(dtRaw$race[dtRaw$race == a]))/p)
}

#gender (nominal)

for (a in unique(dtRaw$gender))
{
  dtClean$gender[dtClean$gender == a] <- ((p - length(dtRaw$gender[dtRaw$gender == a]))/p)
}

#age (ordinal)
dtClean$age[dtClean$age == "[0-10)"] <- (1-1)/(10-1) 
dtClean$age[dtClean$age == "[10-20)"] <- (2-1)/(10-1) 
dtClean$age[dtClean$age == "[20-30)"] <- (3-1)/(10-1) 
dtClean$age[dtClean$age == "[30-40)"] <- (4-1)/(10-1) 
dtClean$age[dtClean$age == "[40-50)"] <- (5-1)/(10-1) 
dtClean$age[dtClean$age == "[50-60)"] <- (6-1)/(10-1) 
dtClean$age[dtClean$age == "[60-70)"] <- (7-1)/(10-1) 
dtClean$age[dtClean$age == "[70-80)"] <- (8-1)/(10-1) 
dtClean$age[dtClean$age == "[80-90)"] <- (9-1)/(10-1) 
dtClean$age[dtClean$age == "[90-100)"] <- (10-1)/(10-1)

#admission_type_id (nominal)

for (a in unique(dtRaw$admission_type_id))
{
  dtClean$admission_type_id[dtClean$admission_type_id == a] <- ((p - length(dtRaw$admission_type_id[dtRaw$admission_type_id == a]))/p)
}

#discharge_disposition_id (nominal)

for (a in unique(dtRaw$discharge_disposition_id))
{
  dtClean$discharge_disposition_id[dtClean$discharge_disposition_id == a] <- ((p - length(dtRaw$discharge_disposition_id[dtRaw$discharge_disposition_id == a]))/p)
}

#admission_source_id (nominal)

for (a in unique(dtRaw$admission_source_id ))
{
  dtClean$admission_source_id [dtClean$admission_source_id  == a] <- ((p - length(dtRaw$admission_source_id [dtRaw$admission_source_id == a]))/p)
}

#payer_code (nominal)

for (a in unique(dtRaw$payer_code))
{
  dtClean$payer_code[dtClean$payer_code == a] <- ((p - length(dtRaw$payer_code[dtRaw$payer_code == a]))/p)
}


#medical_specialty (nominal)

for (a in unique(dtRaw$medical_specialty))
{
  dtClean$medical_specialty[dtClean$medical_specialty == a] <- ((p - length(dtRaw$medical_specialty[dtRaw$medical_specialty == a]))/p)
}

#diag_1 (nominal)

for (a in unique(dtRaw$diag_1))
{
  dtClean$diag_1[dtClean$diag_1 == a] <- ((p - length(dtRaw$diag_1[dtRaw$diag_1 == a]))/p)
}

#diag_2 (nominal)

for (a in unique(dtRaw$diag_2))
{
  dtClean$diag_2[dtClean$diag_2 == a] <- ((p - length(dtRaw$diag_2[dtRaw$diag_2 == a]))/p)
}

#diag_3 (nominal)

for (a in unique(dtRaw$diag_3))
{
  dtClean$diag_3[dtClean$diag_3 == a] <- ((p - length(dtRaw$diag_3[dtRaw$diag_3 == a]))/p)
}

#max_glu_serum (nominal)

for (a in unique(dtRaw$max_glu_serum))
{
  dtClean$max_glu_serum[dtClean$max_glu_serum == a] <- ((p - length(dtRaw$max_glu_serum[dtRaw$max_glu_serum == a]))/p)
}

#A1Cresult (nominal)

for (a in unique(dtRaw$A1Cresult))
{
  dtClean$A1Cresult[dtClean$A1Cresult == a] <- ((p - length(dtRaw$A1Cresult[dtRaw$A1Cresult == a]))/p)
}

#metformin  (nominal)

for (a in unique(dtRaw$metformin))
{
  dtClean$metformin[dtClean$metformin == a] <- ((p - length(dtRaw$metformin[dtRaw$metformin == a]))/p)
}

#repaglinide  (nominal)

for (a in unique(dtRaw$repaglinide))
{
  dtClean$repaglinide[dtClean$repaglinide == a] <- ((p - length(dtRaw$repaglinide[dtRaw$repaglinide == a]))/p)
}

#nateglinide (nominal)

for (a in unique(dtRaw$nateglinide))
{
  dtClean$nateglinide[dtClean$nateglinide == a] <- ((p - length(dtRaw$nateglinide[dtRaw$nateglinide == a]))/p)
}

#chlorpropamide (nominal)

for (a in unique(dtRaw$chlorpropamide))
{
  dtClean$chlorpropamide[dtClean$chlorpropamide == a] <- ((p - length(dtRaw$chlorpropamide[dtRaw$chlorpropamide == a]))/p)
}

#glimepiride (nominal)

for (a in unique(dtRaw$glimepiride))
{
  dtClean$glimepiride[dtClean$glimepiride == a] <- ((p - length(dtRaw$glimepiride[dtRaw$glimepiride == a]))/p)
}

#acetohexamide (nominal)

for (a in unique(dtRaw$acetohexamide))
{
  dtClean$acetohexamide[dtClean$acetohexamide == a] <- ((p - length(dtRaw$acetohexamide[dtRaw$acetohexamide == a]))/p)
}


#glipizide (nominal)

for (a in unique(dtRaw$glipizide))
{
  dtClean$glipizide[dtClean$glipizide == a] <- ((p - length(dtRaw$glipizide[dtRaw$glipizide == a]))/p)
}

#glyburide (nominal)

for (a in unique(dtRaw$glyburide))
{
  dtClean$glyburide[dtClean$glyburide == a] <- ((p - length(dtRaw$glyburide[dtRaw$glyburide == a]))/p)
}


#tolbutamide (nominal)

for (a in unique(dtRaw$tolbutamide))
{
  dtClean$tolbutamide[dtClean$tolbutamide == a] <- ((p - length(dtRaw$tolbutamide[dtRaw$tolbutamide == a]))/p)
}

#pioglitazone (nominal)

for (a in unique(dtRaw$pioglitazone))
{
  dtClean$pioglitazone[dtClean$pioglitazone == a] <- ((p - length(dtRaw$pioglitazone[dtRaw$pioglitazone == a]))/p)
}

#rosiglitazone (nominal)

for (a in unique(dtRaw$rosiglitazone))
{
  dtClean$rosiglitazone[dtClean$rosiglitazone == a] <- ((p - length(dtRaw$rosiglitazone[dtRaw$rosiglitazone == a]))/p)
}

#acarbose (nominal)

for (a in unique(dtRaw$acarbose))
{
  dtClean$acarbose[dtClean$acarbose == a] <- ((p - length(dtRaw$acarbose[dtRaw$acarbose == a]))/p)
}

#miglitol (nominal)

for (a in unique(dtRaw$miglitol))
{
  dtClean$miglitol[dtClean$miglitol == a] <- ((p - length(dtRaw$miglitol[dtRaw$miglitol == a]))/p)
}

#troglitazone (nominal)

for (a in unique(dtRaw$troglitazone))
{
  dtClean$troglitazone[dtClean$troglitazone == a] <- ((p - length(dtRaw$troglitazone[dtRaw$troglitazone == a]))/p)
}

#tolazamide (nominal)

for (a in unique(dtRaw$tolazamide))
{
  dtClean$tolazamide[dtClean$tolazamide == a] <- ((p - length(dtRaw$tolazamide[dtRaw$tolazamide == a]))/p)
}

#examide (nominal)

for (a in unique(dtRaw$examide))
{
  dtClean$examide[dtClean$examide == a] <- ((p - length(dtRaw$examide[dtRaw$examide == a]))/p)
}

#citoglipton (nominal)

for (a in unique(dtRaw$citoglipton))
{
  dtClean$citoglipton[dtClean$citoglipton == a] <- ((p - length(dtRaw$citoglipton[dtRaw$citoglipton == a]))/p)
}

#insulin (nominal)

for (a in unique(dtRaw$insulin))
{
  dtClean$insulin[dtClean$insulin == a] <- ((p - length(dtRaw$insulin[dtRaw$insulin == a]))/p)
}


#glyburide-metformin (nominal)

for (a in unique(dtRaw$glyburide.metformin))
{
  dtClean$glyburide.metformin[dtClean$glyburide.metformin == a] <- ((p - length(dtRaw$glyburide.metformin[dtRaw$glyburide.metformin == a]))/p)
}

#glipizide-metformin (nominal)

for (a in unique(dtRaw$glipizide.metformin))
{
  dtClean$glipizide.metformin[dtClean$glipizide.metformin == a] <- ((p - length(dtRaw$glipizide.metformin[dtRaw$glipizide.metformin == a]))/p)
}

#glimepiride-pioglitazone (nominal)

for (a in unique(dtRaw$glimepiride.pioglitazone))
{
  dtClean$glimepiride.pioglitazone[dtClean$glimepiride.pioglitazone == a] <- ((p - length(dtRaw$glimepiride.pioglitazone[dtRaw$glimepiride.pioglitazone == a]))/p)
}

#metformin-rosiglitazone (nominal)

for (a in unique(dtRaw$metformin.rosiglitazone))
{
  dtClean$metformin.rosiglitazone[dtClean$metformin.rosiglitazone == a] <- ((p - length(dtRaw$metformin.rosiglitazone[dtRaw$metformin.rosiglitazone == a]))/p)
}

#metformin-pioglitazone (nominal)

for (a in unique(dtRaw$metformin.pioglitazone))
{
  dtClean$metformin.pioglitazone[dtClean$metformin.pioglitazone == a] <- ((p - length(dtRaw$metformin.pioglitazone[dtRaw$metformin.pioglitazone == a]))/p)
}

#change (nominal)

for (a in unique(dtRaw$change))
{
  dtClean$change[dtClean$change == a] <- ((p - length(dtRaw$change[dtRaw$change == a]))/p)
}

#diabetesMed (nominal)

for (a in unique(dtRaw$diabetesMed))
{
  dtClean$diabetesMed[dtClean$diabetesMed == a] <- ((p - length(dtRaw$diabetesMed[dtRaw$diabetesMed == a]))/p)
}

#readmitted (nominal)

for (a in unique(dtRaw$readmitted))
{
  dtClean$readmitted[dtClean$readmitted == a] <- ((p - length(dtRaw$readmitted[dtRaw$readmitted == a]))/p)
}


#2) What are the Homogeneity score and Silhouette score of the K-means clustering over the data 
#if we assume that the number of different values of attribute "race" is the number of clusters. 
#Consider missing values as its own category. Is Race a good attribute to cluster this data? (1.5)

dtKMeansRace <- dtClean

dtKMeansRace$encounter_id <- NULL
dtKMeansRace$patient_nbr <- NULL
dtKMeansRace$race <- NULL

dataNumRace <- matrix(data = NA, nrow = dim(dtKMeansRace)[1], ncol = dim(dtKMeansRace)[2])
for (i in 1:dim(dtKMeansRace)[2]) {
  dataNumRace[,i] <- c(as.numeric(dtKMeansRace[[i]]))
}

#Homogeneity score -- used cclust method to find avg distance within clusters -- no other functions found

hs <- cclust(dataNumRace, 6, dist = "euclidean", method = "kmeans")

#Silhouette score

kRaceSS <- skmeans(dataNumRace, 6)
ssRace <- summary(silhouette(kRaceSS))
write.csv(ssRace$avg.width, file = "Homework4-Question2-Silhouette.csv")

#3) which of the three attributes "gender","age", or "readmitted" 
#is better to cluster the data according to it? (2) Describe what you did and show the results. 

#gender
dtKMeansGender <- dtClean
dtKMeansGender$encounter_id <- NULL
dtKMeansGender$patient_nbr <- NULL
dtKMeansGender$gender <- NULL

dataNumGender <- matrix(data = NA, nrow = dim(dtKMeansGender)[1], ncol = dim(dtKMeansGender)[2])
for (i in 1:dim(dtKMeansGender)[2]) {
  dataNumGender[,i] <- c(as.numeric(dtKMeansGender[[i]]))
}

kGenderSS <- skmeans(dataNumGender, 3)

#age
dtKMeansAge <- dtClean
dtKMeansAge$encounter_id <- NULL
dtKMeansAge$patient_nbr <- NULL
dtKMeansAge$age <- NULL

dataNumAge <- matrix(data = NA, nrow = dim(dtKMeansAge)[1], ncol = dim(dtKMeansAge)[2])
for (i in 1:dim(dtKMeansAge)[2]) {
  dataNumAge[,i] <- c(as.numeric(dtKMeansAge[[i]]))
}

kAgeSS <- skmeans(dataNumAge, 10)

#readmitted
dtKMeansReadmitted <- dtClean
dtKMeansReadmitted$encounter_id <- NULL
dtKMeansReadmitted$patient_nbr <- NULL
dtKMeansReadmitted$readmitted <- NULL

dataNumReadmitted <- matrix(data = NA, nrow = dim(dtKMeansReadmitted)[1], ncol = dim(dtKMeansReadmitted)[2])
for (i in 1:dim(dtKMeansReadmitted)[2]) {
  dataNumReadmitted[,i] <- c(as.numeric(dtKMeansReadmitted[[i]]))
}

kReadmittedSS <- skmeans(dataNumReadmitted, 3)

#comparison of silhouette scores

ssGender <- summary(silhouette(kGenderSS))
ssAge <- summary(silhouette(kAgeSS))
ssReadmitted <- summary(silhouette(kReadmittedSS))

ssScoreComp <- c(ssGender$avg.width, ssAge$avg.width, ssReadmitted$avg.width)
write.csv(ssScoreComp, file = "Homework4-Question3-SilhouetteComparisons.csv")

plot(silhouette(kGenderSS), col = c("violet", "red", "blue"))
plot(silhouette(kAgeSS), col = c("violet", "red", "blue", "green", "yellow", "orange", "grey", "black", "pink"))
plot(silhouette(kReadmittedSS), col = c("violet", "green", "blue"))

#4) Which clustering method is better assuming we are interested in clustering the data, 
#base on the readmitted attribute. K-means or K-medians? Give a reason why the method you choose is better? (1)

dtKMedianReadmitted <- dtClean
dtKMedianReadmitted$encounter_id <- NULL
dtKMedianReadmitted$patient_nbr <- NULL
dtKMedianReadmitted$readmitted <- NULL

dataNumReadmitted1 <- matrix(data = NA, nrow = dim(dtKMedianReadmitted)[1], ncol = dim(dtKMedianReadmitted)[2])
for (i in 1:dim(dtKMedianReadmitted)[2]) {
  dataNumReadmitted1[,i] <- c(as.numeric(dtKMedianReadmitted[[i]]))
}

kReadmitted1 <- clara(dataNumReadmitted1, 3, metric = "euclidean")
ssReadmitted1 <- summary(silhouette(kReadmitted1))

methodcomp <- c(ssReadmitted$avg.width, ssReadmitted1$avg.width)
write.csv(methodcomp, file = "Homework4-Question4-MethodComparisons.csv")

#5) Going back to question 2, Run Hierarchical clustering method using Cosine Distance and 
#compare the results to K-means. 
#Calculate the two metrics in question 2. Which one is better ? Why? (2.5)

#data clean-up for clustering

dt5Raw <- dtRaw[sample(nrow(dtRaw), 10000), ]

dt5Clean <- dt5Raw
p <- nrow(dt5Raw)


#race (nominal)
for (a in unique(dt5Raw$race))
{
  dt5Clean$race[dt5Clean$race == a] <- ((p - length(dt5Raw$race[dt5Raw$race == a]))/p)
}

#gender (nominal)

for (a in unique(dt5Raw$gender))
{
  dt5Clean$gender[dt5Clean$gender == a] <- ((p - length(dt5Raw$gender[dt5Raw$gender == a]))/p)
}

#age (ordinal)
dt5Clean$age[dt5Clean$age == "[0-10)"] <- (1-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[10-20)"] <- (2-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[20-30)"] <- (3-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[30-40)"] <- (4-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[40-50)"] <- (5-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[50-60)"] <- (6-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[60-70)"] <- (7-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[70-80)"] <- (8-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[80-90)"] <- (9-1)/(10-1) 
dt5Clean$age[dt5Clean$age == "[90-100)"] <- (10-1)/(10-1)

#admission_type_id (nominal)

for (a in unique(dt5Raw$admission_type_id))
{
  dt5Clean$admission_type_id[dt5Clean$admission_type_id == a] <- ((p - length(dt5Raw$admission_type_id[dt5Raw$admission_type_id == a]))/p)
}

#discharge_disposition_id (nominal)

for (a in unique(dt5Raw$discharge_disposition_id))
{
  dt5Clean$discharge_disposition_id[dt5Clean$discharge_disposition_id == a] <- ((p - length(dt5Raw$discharge_disposition_id[dt5Raw$discharge_disposition_id == a]))/p)
}

#admission_source_id (nominal)

for (a in unique(dt5Raw$admission_source_id ))
{
  dt5Clean$admission_source_id [dt5Clean$admission_source_id  == a] <- ((p - length(dt5Raw$admission_source_id [dt5Raw$admission_source_id == a]))/p)
}

#payer_code (nominal)

for (a in unique(dt5Raw$payer_code))
{
  dt5Clean$payer_code[dt5Clean$payer_code == a] <- ((p - length(dt5Raw$payer_code[dt5Raw$payer_code == a]))/p)
}


#medical_specialty (nominal)

for (a in unique(dt5Raw$medical_specialty))
{
  dt5Clean$medical_specialty[dt5Clean$medical_specialty == a] <- ((p - length(dt5Raw$medical_specialty[dt5Raw$medical_specialty == a]))/p)
}

#diag_1 (nominal)

for (a in unique(dt5Raw$diag_1))
{
  dt5Clean$diag_1[dt5Clean$diag_1 == a] <- ((p - length(dt5Raw$diag_1[dt5Raw$diag_1 == a]))/p)
}

#diag_2 (nominal)

for (a in unique(dt5Raw$diag_2))
{
  dt5Clean$diag_2[dt5Clean$diag_2 == a] <- ((p - length(dt5Raw$diag_2[dt5Raw$diag_2 == a]))/p)
}

#diag_3 (nominal)

for (a in unique(dt5Raw$diag_3))
{
  dt5Clean$diag_3[dt5Clean$diag_3 == a] <- ((p - length(dt5Raw$diag_3[dt5Raw$diag_3 == a]))/p)
}

#max_glu_serum (nominal)

for (a in unique(dt5Raw$max_glu_serum))
{
  dt5Clean$max_glu_serum[dt5Clean$max_glu_serum == a] <- ((p - length(dt5Raw$max_glu_serum[dt5Raw$max_glu_serum == a]))/p)
}

#A1Cresult (nominal)

for (a in unique(dt5Raw$A1Cresult))
{
  dt5Clean$A1Cresult[dt5Clean$A1Cresult == a] <- ((p - length(dt5Raw$A1Cresult[dt5Raw$A1Cresult == a]))/p)
}

#metformin  (nominal)

for (a in unique(dt5Raw$metformin))
{
  dt5Clean$metformin[dt5Clean$metformin == a] <- ((p - length(dt5Raw$metformin[dt5Raw$metformin == a]))/p)
}

#repaglinide  (nominal)

for (a in unique(dt5Raw$repaglinide))
{
  dt5Clean$repaglinide[dt5Clean$repaglinide == a] <- ((p - length(dt5Raw$repaglinide[dt5Raw$repaglinide == a]))/p)
}

#nateglinide (nominal)

for (a in unique(dt5Raw$nateglinide))
{
  dt5Clean$nateglinide[dt5Clean$nateglinide == a] <- ((p - length(dt5Raw$nateglinide[dt5Raw$nateglinide == a]))/p)
}

#chlorpropamide (nominal)

for (a in unique(dt5Raw$chlorpropamide))
{
  dt5Clean$chlorpropamide[dt5Clean$chlorpropamide == a] <- ((p - length(dt5Raw$chlorpropamide[dt5Raw$chlorpropamide == a]))/p)
}

#glimepiride (nominal)

for (a in unique(dt5Raw$glimepiride))
{
  dt5Clean$glimepiride[dt5Clean$glimepiride == a] <- ((p - length(dt5Raw$glimepiride[dt5Raw$glimepiride == a]))/p)
}

#acetohexamide (nominal)

for (a in unique(dt5Raw$acetohexamide))
{
  dt5Clean$acetohexamide[dt5Clean$acetohexamide == a] <- ((p - length(dt5Raw$acetohexamide[dt5Raw$acetohexamide == a]))/p)
}


#glipizide (nominal)

for (a in unique(dt5Raw$glipizide))
{
  dt5Clean$glipizide[dt5Clean$glipizide == a] <- ((p - length(dt5Raw$glipizide[dt5Raw$glipizide == a]))/p)
}

#glyburide (nominal)

for (a in unique(dt5Raw$glyburide))
{
  dt5Clean$glyburide[dt5Clean$glyburide == a] <- ((p - length(dt5Raw$glyburide[dt5Raw$glyburide == a]))/p)
}


#tolbutamide (nominal)

for (a in unique(dt5Raw$tolbutamide))
{
  dt5Clean$tolbutamide[dt5Clean$tolbutamide == a] <- ((p - length(dt5Raw$tolbutamide[dt5Raw$tolbutamide == a]))/p)
}

#pioglitazone (nominal)

for (a in unique(dt5Raw$pioglitazone))
{
  dt5Clean$pioglitazone[dt5Clean$pioglitazone == a] <- ((p - length(dt5Raw$pioglitazone[dt5Raw$pioglitazone == a]))/p)
}

#rosiglitazone (nominal)

for (a in unique(dt5Raw$rosiglitazone))
{
  dt5Clean$rosiglitazone[dt5Clean$rosiglitazone == a] <- ((p - length(dt5Raw$rosiglitazone[dt5Raw$rosiglitazone == a]))/p)
}

#acarbose (nominal)

for (a in unique(dt5Raw$acarbose))
{
  dt5Clean$acarbose[dt5Clean$acarbose == a] <- ((p - length(dt5Raw$acarbose[dt5Raw$acarbose == a]))/p)
}

#miglitol (nominal)

for (a in unique(dt5Raw$miglitol))
{
  dt5Clean$miglitol[dt5Clean$miglitol == a] <- ((p - length(dt5Raw$miglitol[dt5Raw$miglitol == a]))/p)
}

#troglitazone (nominal)

for (a in unique(dt5Raw$troglitazone))
{
  dt5Clean$troglitazone[dt5Clean$troglitazone == a] <- ((p - length(dt5Raw$troglitazone[dt5Raw$troglitazone == a]))/p)
}

#tolazamide (nominal)

for (a in unique(dt5Raw$tolazamide))
{
  dt5Clean$tolazamide[dt5Clean$tolazamide == a] <- ((p - length(dt5Raw$tolazamide[dt5Raw$tolazamide == a]))/p)
}

#examide (nominal)

for (a in unique(dt5Raw$examide))
{
  dt5Clean$examide[dt5Clean$examide == a] <- ((p - length(dt5Raw$examide[dt5Raw$examide == a]))/p)
}

#citoglipton (nominal)

for (a in unique(dt5Raw$citoglipton))
{
  dt5Clean$citoglipton[dt5Clean$citoglipton == a] <- ((p - length(dt5Raw$citoglipton[dt5Raw$citoglipton == a]))/p)
}

#insulin (nominal)

for (a in unique(dt5Raw$insulin))
{
  dt5Clean$insulin[dt5Clean$insulin == a] <- ((p - length(dt5Raw$insulin[dt5Raw$insulin == a]))/p)
}


#glyburide-metformin (nominal)

for (a in unique(dt5Raw$glyburide.metformin))
{
  dt5Clean$glyburide.metformin[dt5Clean$glyburide.metformin == a] <- ((p - length(dt5Raw$glyburide.metformin[dt5Raw$glyburide.metformin == a]))/p)
}

#glipizide-metformin (nominal)

for (a in unique(dt5Raw$glipizide.metformin))
{
  dt5Clean$glipizide.metformin[dt5Clean$glipizide.metformin == a] <- ((p - length(dt5Raw$glipizide.metformin[dt5Raw$glipizide.metformin == a]))/p)
}

#glimepiride-pioglitazone (nominal)

for (a in unique(dt5Raw$glimepiride.pioglitazone))
{
  dt5Clean$glimepiride.pioglitazone[dt5Clean$glimepiride.pioglitazone == a] <- ((p - length(dt5Raw$glimepiride.pioglitazone[dt5Raw$glimepiride.pioglitazone == a]))/p)
}

#metformin-rosiglitazone (nominal)

for (a in unique(dt5Raw$metformin.rosiglitazone))
{
  dt5Clean$metformin.rosiglitazone[dt5Clean$metformin.rosiglitazone == a] <- ((p - length(dt5Raw$metformin.rosiglitazone[dt5Raw$metformin.rosiglitazone == a]))/p)
}

#metformin-pioglitazone (nominal)

for (a in unique(dt5Raw$metformin.pioglitazone))
{
  dt5Clean$metformin.pioglitazone[dt5Clean$metformin.pioglitazone == a] <- ((p - length(dt5Raw$metformin.pioglitazone[dt5Raw$metformin.pioglitazone == a]))/p)
}

#change (nominal)

for (a in unique(dt5Raw$change))
{
  dt5Clean$change[dt5Clean$change == a] <- ((p - length(dt5Raw$change[dt5Raw$change == a]))/p)
}

#diabetesMed (nominal)

for (a in unique(dt5Raw$diabetesMed))
{
  dt5Clean$diabetesMed[dt5Clean$diabetesMed == a] <- ((p - length(dt5Raw$diabetesMed[dt5Raw$diabetesMed == a]))/p)
}

#readmitted (nominal)

for (a in unique(dt5Raw$readmitted))
{
  dt5Clean$readmitted[dt5Clean$readmitted == a] <- ((p - length(dt5Raw$readmitted[dt5Raw$readmitted == a]))/p)
}


dt5KMeansRace <- dt5Clean

dt5KMeansRace$encounter_id <- NULL
dt5KMeansRace$patient_nbr <- NULL
dt5KMeansRace$race <- NULL

data5NumRace <- matrix(data = NA, nrow = dim(dt5KMeansRace)[1], ncol = dim(dt5KMeansRace)[2])
for (i in 1:dim(dt5KMeansRace)[2]) {
  data5NumRace[,i] <- c(as.numeric(dt5KMeansRace[[i]]))
}

clusnum <- length(unique(dt5Raw$race))

# K-means clustering
km.race <- eclust(data5NumRace, "kmeans", k = clusnum, graph = FALSE)


#Hierarchial Clustering using cosine distance
distCosine <- dist.cosine(data5NumRace)
hr.race <- eclust(data5NumRace, "hclust", k = clusnum, hc_method = "ward.D2", graph = FALSE)
sil.hr.race <- summary(silhouette(hr.race$cluster, distCosine))


#compare silhouette score between kmeans and hierarchial

silComp <- c(km.race$silinfo$avg.width, sil.hr.race$avg.width)

write.csv(silComp, file = "Homework4-Question5-ClusteringComparison.csv")

#Homogeneity score -- used cclust method to find avg distance within clusters -- no other functions found

hs5 <- cclust(data5NumRace, clusnum, dist = "euclidean", method = "kmeans")

sink('homogeneity1-5.txt')
summary(hs)
summary(hs5)







