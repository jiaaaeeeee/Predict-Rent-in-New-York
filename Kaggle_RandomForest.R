# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
# Read data and construct a simple model

data = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/analysisData.csv')

# Data Clean ----
## Impute N/A by median ----
library(readr)
data$square_feet[is.na(data$square_feet)] <- median(data$square_feet, na.rm=TRUE)
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms, na.rm=TRUE)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms, na.rm=TRUE)
data$beds[is.na(data$beds)] <- median(data$beds, na.rm=TRUE)
data$cleaning_fee[is.na(data$cleaning_fee)] <- median(data$cleaning_fee, na.rm=TRUE)

## ZipCode ----
library(readr)
data$zipcode <- gsub("[^0-9]", "", data$zipcode, perl=TRUE)
data$zipcode = substr(data$zipcode, 1, 5)
zipcode_freq = data.frame(table(data$zipcode))
low_zipcide_freq = subset(zipcode_freq,Freq < 50)
data$zipcode = ifelse(data$zipcode %in% low_zipcide_freq$Var1, "other", data$zipcode)
data$zipcode = ifelse(data$zipcode == "","other", data$zipcode)
data$zipcode = as.factor(data$zipcode)


## RoomType ----
table(data$room_type)
data$room_type = ifelse(data$room_type == "Entire home/apt", 1, data$room_type)
data$room_type = ifelse(data$room_type == "Hotel room", 2, data$room_type)
data$room_type = ifelse(data$room_type == "Private room", 3, data$room_type)
data$room_type = ifelse(data$room_type == "Shared room",4, data$room_type)
data$room_type = as.factor(data$room_type)

##accommodates ----
accommodates_freq = data.frame(table(data$accommodates))
low_accommodates_freq = subset(accommodates_freq,Freq < 50)
data$accommodates =  ifelse(data$accommodates %in% low_accommodates_freq$Var1,0,data$accommodates)



library(rpart);library(randomForest);library(ipred);library(gbm);library(randomForest);library(caret)

set.seed(1731)
split = sample(x = 1:nrow(data),size = 0.8*nrow(data))
train = data[split,]
test = data[-split,]



### tune----
trControl=trainControl(method="cv",number=5)
tuneGrid = expand.grid(mtry=1:ncol(train)-1, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5))
set.seed(1031)
cvModel = train(price~bathrooms+bedrooms+zipcode+room_type+accommodates+host_response_rate,
                data=train,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid)
cvModel$bestTune


### model ranger----
library(ranger)
set.seed(1031)
forest_ranger_tune = ranger(price~cleaning_fee+availability_90+availability_60+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                       data = train,
                       mtry=5, 
                       min.node.size = 18,
                       splitrule = 'maxstat')
pred_train = predict(forest_ranger_tune,data = train)
rmsetrain = sqrt(mean((pred_train$predictions-train$price)^2)); rmsetrain
pred = predict(forest_ranger_tune,data = test)
rmsetest = sqrt(mean((pred$predictions-test$price)^2)); rmsetest

set.seed(1031)
forest_ranger = ranger(price~bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                       data = train)
pred_train = predict(forest_ranger,data = train)
rmsetrain = sqrt(mean((pred_train$predictions-train$price)^2)); rmsetrain
pred = predict(forest_ranger,data = test)
rmsetest = sqrt(mean((pred$predictions-test$price)^2)); rmsetest

### model linear regression----
library(ranger)
set.seed(1031)
model = lm(price~cleaning_fee+availability_90+availability_60+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                       data = train)
pred_train = predict(model,data = train)
rmsetrain = sqrt(mean((pred_train-train$price)^2)); rmsetrain
pred = predict(model,newdata = test)
rmsetest = sqrt(mean((pred-test$price)^2)); rmsetest


# Model ----
library(ranger)
set.seed(1031)
forest_ranger_data = ranger(price~cleaning_fee+availability_90+availability_60+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                            data = data)


#summary(model)
# Read scoring data and apply model to generate predictions
#Validation ----
scoringData = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/scoringData.csv')
## Imupute N/A ----
scoringData$square_feet[is.na(scoringData$square_feet)] <- median(scoringData$square_feet, na.rm=TRUE)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <- median(scoringData$bathrooms, na.rm=TRUE)
scoringData$bedrooms[is.na(scoringData$bedrooms)] <- median(scoringData$bedrooms, na.rm=TRUE)
scoringData$beds[is.na(scoringData$beds)] <- median(scoringData$beds, na.rm=TRUE)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- median(scoringData$cleaning_fee, na.rm=TRUE)

## Zipcode ----
scoringData$zipcode <- gsub("[^0-9]", "", scoringData$zipcode, perl=TRUE)
scoringData$zipcode = substr(scoringData$zipcode, 1, 5)
occurance_test = data.frame(table(scoringData$zipcode))
occurance_low_test = subset(occurance_test,Freq < 100)
scoringData$zipcode = ifelse(scoringData$zipcode %in% occurance_low_test$Var1, "other", scoringData$zipcode)
scoringData$zipcode = ifelse(scoringData$zipcode == "","other", scoringData$zipcode)

scoringData$room_type = ifelse(scoringData$room_type == "Entire home/apt", 1, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Hotel room", 2, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Private room", 3, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Shared room",4, scoringData$room_type)
scoringData$room_type = as.factor(scoringData$room_type)

accommodates_freq_scoring = data.frame(table(scoringData$accommodates))
low_accommodates_freq_scoring = subset(accommodates_freq_scoring,Freq < 100)
scoringData$accommodates =  ifelse(scoringData$accommodates %in% low_accommodates_freq_scoring$Var1,0,scoringData$accommodates)

pred = predict(forest_ranger_data,data=scoringData)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred$predictions)
write.csv(submissionFile, '/Users/jiayihuang/Documents/APAN5200/rentlala2021/sample_submission4.csv',row.names = F)
