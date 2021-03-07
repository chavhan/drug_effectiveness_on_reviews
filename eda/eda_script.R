str(drug)
df1 <- drug
head(df1)
names(df1)
df1 <- df1[,-1]
df1 <- drop_na(df1)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
status(df1)
dim(drug)
dim(df1)
describe(drug)
glimpse(drug)
names(drug)
unique(df1$drugName)
count(as.data.frame(unique(drug$condition)))
?count

library(modeest)
mfv(df1$drugName)



unique(drug$Condition)
count(as.data.frame(unique(drug$Condition)))

rm(drugsComTrain_raw)
table(df1$drugName)

freq(df1$drugName)
?freq
df2 <- as.data.frame(freq(df1$drugName))
head(df2)

### taking data into file 
write.csv(df2,"drugfrequency1.csv", row.names = FALSE)
?sort
table(df1$drugName,df1$condition)

######################### removing na values ###########################
df2 <- df1(df1)
names(df1)
head(df3)
df4 <- df3[,-3]
head(df4)
summary(df4)

by_condition <- group_by(df4,condition)
summarise(by_condition, sum_drug=sum(df4$drugName,na.rm = T))

########################## Total Records ######################
totalDurg <- rbind(drug,drug1)
?merge
head(totalDurg)
dim(totalDurg)
names(drug1)
names(df)

class(drug1)
status(totalDurg)
str(totalDurg)
df <- totalDurg[,-1]
df <- drop_na(df)
library(plyr)
(unique_drugs <- count(as.data.frame(unique(df$drugName))))
status(df)
write.csv(as.data.frame(freq(df$drugName)),"drugfrequency1.csv", row.names = FALSE)
write.csv(as.data.frame(freq(df$condition)),"drugcondition.csv", row.names = FALSE)
write.csv(df,"totalreview.csv", row.names = FALSE)
hist(df)

toptenDrug <- as.data.frame(freq(df$drugName))
toptenDrug <- toptenDrug[1:10,c(-3,-4)]
toptenDrug
toptenCondition <- as.data.frame(freq(df$condition))
toptenCondition <- toptenCondition[1:10,c(-3,-4)]
toptenCondition
toptenRating <- as.data.frame(freq(df$rating))
toptenRating <- toptenRating[1:10,c(-3,-4)]
toptenRating

table(toptenDrug)
barplot(table(toptenDrug))
library(ggplot2)
library(dplyr)
library(ggpubr)
########################## Graph of Drugs ########################################
ggplot(toptenDrug, aes(x = toptenDrug$var, y = toptenDrug$frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = toptenDrug$frequency), vjust = -0.3) + 
  theme_pubclean() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

########################## Graph of Condition ########################################
ggplot(toptenCondition, aes(x = toptenCondition$var, y = toptenCondition$frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = toptenCondition$frequency), vjust = -0.3) + 
  theme_pubclean() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

########################## Graph of rating ########################################
ggplot(toptenRating, aes(x = toptenRating$var, y = toptenRating$frequency)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = toptenRating$frequency), vjust = -0.3) + 
  theme_pubclean() +
  theme(axis.text.x=element_text(angle=45, hjust=1))
########################## Replacing the long name into short #####################

change_med_name <- function(x)
{
  lenName <-  as.data.frame(explode(x, sep = '/'))
  if(lenName > 1)
  {
    return(trimws(lenName[1,], which = "both"))
  }
}

testval <- trimws(testval,which = "both")
testval
countval <- count(explode(myval, sep = '/'))
write.csv(df,"cleandrug.csv", row.names = FALSE)

################################### End of cleaning name no sucess #####################
library(gmodels)
testdf <- df[1:500,]
(CrossTable(df$drugName))
?CrossTable

############################### clustering on drug dataset #################################
drug_test <- drug[1:1000,]
dim(drug_test)
head(drug_test)
drug_test$durgNameNumeric <- as.numeric(as.factor(drug_test$drugName))
class(drug_test$durgNameNumeric)
drug_test$conditionNumeric <- as.numeric(as.factor(drug_test$condition))
myvar <- count(as.data.frame(unique(drug_test$durgNameNumeric)))
dim(myvar)
?count
length(myvar$unique.drug_test.durgNameNumeric.)
status(drug_test)
mydata <- drug_test[,c(6,7)]
head(mydata)
plot(mydata)
text(mydata,row.names(data))
km <- kmeans(mydata,4)
str(km)
View(km$centers)


library(animation)
km <- kmeans.ani(mydata,4)
library(factoextra)
fviz_nbclust(mydata,kmeans,method = "wss")+labs(subtitle = 'Elbow Method')
library(kselection)
k <- kselection(data,parallel = TRUE)
fviz_nbclust(normalized_data,kmeans,method = "wss")+labs(subtitle = 'Elbow Method')
fit <- kmeans(normalized_data,6)
aggregate(mydata,by=list(km$cluster),FUN = 'mean')
########################### end of code ####################################################

########################## Removing false condition ########################################
status(df1)
dim(df)
df1 <- df[- grep("</span>", df$condition),]
dim(df1)
###################################### combine histogram ###################################
str(df1)
df1 <- df1[,c(-3,-5)]
head(df2)
df2 <- df1
df2$drugName <- as.numeric(as.factor(df1$drugName))
df2$condition <- as.numeric(as.factor(df1$condition))
head(freq(df2$drugName))
freq(df2)
plot_num(df2)
ggplot(data = df2) +
  geom_point(mapping = aes(x = drugName, y = condition))