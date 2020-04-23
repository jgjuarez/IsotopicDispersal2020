library(dplyr)
library(stringr)
library(psych)
library(reshape2)

#We need to transforme the variables container and isotope into factors, and un.enrichment to character

container <- as.factor(enrichment$container.id)
isotope <- as.factor(enrichment$isotope)
un <- as.character(enrichment$un.enrichment)

#Now we need to check for duplicates with the function duplicated or unique
dup <- as.character(enrichment[which(duplicated(as.character(enrichment$un.enrichment))), "un.enrichment"])
str(dup)

enrichment[which(enrichment$un.enrichment %in% dup),]

dup <- duplicated(enrichment$un.enrichment)

unique(enrichment(enrichment$un.enrichment))

#If we need a new Id for a variable we can do it by using the paste function
id.un <- as.factor(paste(container, isotope, sep = ""))

#Now we need a new data frame that has the id for container and isotope
df1 <- data.frame(unique = id.un, enrichment[,])
View(df1)

#Select the unique values and make a new data frame for only the unique container id
#any of the next methods will do the selection of data frame
df2 <- unique(df1$unique)
df2 <- df1[!duplicated(df1$unique),]
df2 <- df1 %>% distinct(unique, .keep_all = TRUE)
View(df2)

#Save the new df2, with the unique container data so we can use it for Qgis
write.csv(df2, file="QgisEnrich.csv", row.names = TRUE)

#we need to select the unique variables for the point of BG surveillance
arreglo <- nitrogen_to_point_of_enrichment[!duplicated(nitrogen_to_point_of_enrichment$ID),]

#Adjusting the table with transpose for switching the variables
df3 <- t(arreglo)

write.csv(df3, file="point_to_BG.csv", row.names = TRUE)

#After saving the document you will need to remove the first row found in the
#file, since when saving it the first row is not recognized as a title. 
#MAKE SURE THE AMOUNT OF THE VARIABLE IS CORRECT

ajuste.metros <- point_to_BG[,2:22]*100000
View(ajuste.metros)

base <- describe(ajuste.metros)

base <- data.frame(base)

View(base)

LowCI <- c(base$mean-(1.96*sqrt(base$se)))
HiCI <- c(base$mean+(1.96*sqrt(base$se)))
titulos <- cbind(LowCI, HiCI)
View(titulos)

write.csv(base, file="nitrogen_distance.csv", row.names = TRUE)

#Now we want to merge the data table with the marked pools with the summary statistic found for the dispersion
nitrogen_distance$house.id <- as.factor(nitrogen_distance$house.id)
nitrogen_surveillance$house.id <- as.factor(nitrogen_surveillance$house.id)

df.combined <- merge(nitrogen_distance, nitrogen_surveillance, by= "house.id", all = TRUE)
write.csv(df.combined, file="nitrogen_final.csv", row.names = TRUE)

#Redo for the remaining isotopes just changing the names for the files.