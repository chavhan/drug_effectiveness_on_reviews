setwd("D:\\DataScience\\Final_Project")

library(readr)
library(dbplyr)
#library(plyr)
library(dplyr)
library(tm)
library(caret)
library(SnowballC)
library(data.table)
# library(ggplot2)
# library(ggthemes)

train_data <- train_data[,-1]
test_data <- test_data[,-1]

dim(train_data)
dim(test_data)

data <- rbind(train_data,test_data)
sum(is.na(data))
str(data)

data <- na.omit(data)
data <- data[-grep('users found this comment helpful.',data$condition),]
data <- data.frame(data)
str(data)
## top 10 condition data
top10cond <- as.data.frame(sort(table(data$condition),decreasing=TRUE)[1:10])
names(top10cond)[c(1,2)] <- c("condition","total_count")
top10CondData <- data[data$condition %in% top10cond[,"condition"],] 
View(top10CondData[order(top10CondData$condition),])
str(top10CondData)
#top10CondData20k <- top10CondData[1:20000,]
#str(top10CondData20k)
#top10CondData <- top10CondData20k
## converting date from character to date format.
library(lubridate)
##packageDescription('SnowballC')
class(top10CondData$date)
top10CondData$date <- as.Date(top10CondData$date)
#top10CondData$date <- mdy(top10CondData$date)
str(top10CondData)
## taking only data from year 2015.
top10CondData <-  subset(top10CondData,date > '2014-12-31')
View(top10CondData1)
head(top10CondData)

## making category class 
y <- ifelse(top10CondData$rating>5,1,0)

table(y)
# category_class
# 0     1 
# 21581 33725 

prop.table(table(y))
# category_class
# 0         1 
# 0.3902108 0.6097892 

top10CondData <- cbind(top10CondData,y)
head(top10CondData)
##top10CondData$category_class <- as.factor(top10CondData$category_class)

dim(top10CondData)
status(top10CondData)
top10CondDataFinal <- top10CondData

stop <- scan(file.choose(),what="character",comment.char = ";")

combine_corpus <- VCorpus(VectorSource(top10CondDataFinal$review))
combine_corpus <- tm_map(combine_corpus,tolower)
combine_corpus <- tm_map(combine_corpus,removePunctuation)
combine_corpus <- tm_map(combine_corpus,removeNumbers)
combine_corpus <- tm_map(combine_corpus,removeWords,c(stopwords("en"),stop))
combine_corpus <- tm_map(combine_corpus,stripWhitespace)
combine_corpus <- tm_map(combine_corpus,stemDocument)
combine_corpus <- tm_map(combine_corpus,PlainTextDocument)

dtm <- DocumentTermMatrix(combine_corpus)
combine_corpus <- removeSparseTerms(dtm, 0.8)

dtm_matrix <- as.matrix(combine_corpus)
dtm_matrix <- cbind(dtm_matrix, top10CondDataFinal$y)
str(dtm_matrix)
head(dtm_matrix)
colnames(dtm_matrix)[ncol(dtm_matrix)] <- "y"
str(dtm_matrix)
colnames(dtm_matrix)
head(dtm_matrix)
ncol(dtm_matrix)
combine_set_top10Cond <- as.data.frame(dtm_matrix)
combine_set_top10Cond$y <- as.factor(combine_set_top10Cond$y)
##write.csv(combine_set_top10Cond,'all_data.csv', row.names = FALSE)

train <- sample(nrow(combine_set_top10Cond), .7 * nrow(combine_set_top10Cond), replace = FALSE)
test  <- sample(nrow(combine_set_top10Cond), .3 * nrow(combine_set_top10Cond), replace = FALSE)
dim(test_dat)
train_dat <- combine_set_top10Cond[train,]
test_dat <- combine_set_top10Cond[test,]
str(test_dat)
### SVM Model ###
review_top10Cond_model <- train(y ~., data = train_dat, method = 'svmLinear3')
head(review_top10Cond_model)

table(test_dat$y)
# 0     1 
# 6455 10136 
prop.table(table(test_dat$y))
#   0         1 
# 0.3890664 0.6109336
# Make predictions
model_top10Cond_result <- predict(review_top10Cond_model, newdata = test_dat)

model_top10Cond_result1 <- predict(review_top10Cond_model, newdata = testdata)
model_top10Cond_result1
table(model_top10Cond_result)
# model_top10Cond_result
# 0     1 
# 1250 15341 
prop.table(table(model_top10Cond_result))
# model_top10Cond_result
# 0          1 
# 0.07534205 0.92465795 

colnames(test_dat)
tab <- table(model_top10Cond_result , test_dat$y)
tab
acc <- sum(diag(tab))/sum(tab)
acc  ### 0.6258212
# model_top10Cond_result    0    1
#                       0  692  558
#                       1 5650 9691

library("gmodels")
CrossTable(x=test_dat$y,y=model_top10Cond_result,prop.chisq = FALSE)

# Model accuracy
mean(model_top10Cond_result == test_dat$y)
##[1] 0.6258212

####################### Checking roc curve ####################
library(ROCR)
pred1 <- prediction(model_top10Cond_result,test_dat$y)

check_accuracy <- as.data.frame(cbind(prediction = model_top10Cond_result, 
                                      rating = test_dat$category_class))
check_accuracy <- check_accuracy %>% mutate(prediction = as.integer(prediction) - 1)
check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)
round(prop.table(table(check_accuracy$accuracy)), 3)


#### Regression model ####
model <- nnet::multinom(y ~., data = train_dat)
summary(model)
# Make predictions

predicted.classes <- model %>% predict(test_dat)

predicted.classes1 <- model %>% predict(testdata)

head(predicted.classes1)
# Model accuracy
mean(predicted.classes == test_dat$y)
### [1] 0.626424

#### Decision Tree #####
library(C50)

train.tree <- C5.0(y ~ .,data = train_dat)  
train.tree1 <- C5.0(y ~ .,data = train_dat,na.action = na.roughfix ,importance = TRUE) 

pred.train <- predict(train.tree , as.data.frame(test_dat))

pred.train1 <- predict(train.tree, as.data.frame(testdata))
pred.train1
## Accuraccy model
mean(pred.train == test_dat$y)
## [1] 0.6721717

library(gmodels)
CrossTable(pred.train , test_dat$y)

#### Random Forest ####

library(randomForest)

fit.forest <- randomForest(y~.,data=train_dat,na.action = na.roughfix ,importance = TRUE)
pred.trn <- predict(fit.forest , as.data.frame(test_dat))
head(fit.forest)
############################## test model alone by supply dummy data #######
str(test_dat)
str(testdata1)
testdata1 <- test_dat[1,]
pred.trn1 <- predict(review_top10Cond_model , as.data.frame(testdata))
algo.result <- attributes(pred.trn1)
class(pred.trn1)
pred.trn
finalresult <- ifelse(algo.result$names==1,'effective','ineffective')
print(finalresult)
mean(pred.trn1 == testdata$y)
########################## end of code #####################################
## Accuraccy model
mean(pred.trn == test_dat$y)
##[1] 0.7894642
