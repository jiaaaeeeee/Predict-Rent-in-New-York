# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
# Read data and construct a simple model
# Data Clean ----
## Train ----
data = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/analysisData.csv')

### Impute N/A by median ----
library(readr)
data$square_feet[is.na(data$square_feet)] <- median(data$square_feet, na.rm=TRUE)
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms, na.rm=TRUE)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms, na.rm=TRUE)
data$beds[is.na(data$beds)] <- median(data$beds, na.rm=TRUE)
data$cleaning_fee[is.na(data$cleaning_fee)] <- median(data$cleaning_fee, na.rm=TRUE)
data$security_deposit[is.na(data$security_deposit)] <- mean(data$security_deposit, na.rm=TRUE)
### ZipCode ----
library(readr)
data$zipcode <- gsub("[^0-9]", "", data$zipcode, perl=TRUE)
data$zipcode = substr(data$zipcode, 1, 5)
zipcode_freq = data.frame(table(data$zipcode))
low_zipcode_freq = subset(zipcode_freq,Freq < 10)
data$zipcode = ifelse(data$zipcode %in% low_zipcode_freq$Var1, "other", data$zipcode)
data$zipcode = ifelse(data$zipcode == "","other", data$zipcode)
### RoomType ----
# data$room_type = ifelse(data$room_type == "Entire home/apt", 1, data$room_type)
# data$room_type = ifelse(data$room_type == "Hotel room", 2, data$room_type)
# data$room_type = ifelse(data$room_type == "Private room", 3, data$room_type)
# data$room_type = ifelse(data$room_type == "Shared room",4, data$room_type)
### accommodates ----
accommodates_freq = data.frame(table(data$accommodates))
low_accommodates_freq = subset(accommodates_freq,Freq < 100)
data$accommodates =  ifelse(data$accommodates %in% low_accommodates_freq$Var1,0,data$accommodates)
### property type ----
proptype_freq = data.frame(table(data$property_type))
low_proptype_freq = subset(proptype_freq,Freq < 10)
data$property_type =  ifelse(data$property_type %in% low_proptype_freq$Var1,'other',data$property_type)
## Validation ----
scoringData = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/scoringData.csv')
### Impute N/A by median ----
scoringData$square_feet[is.na(scoringData$square_feet)] <- median(scoringData$square_feet, na.rm=TRUE)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <- median(scoringData$bathrooms, na.rm=TRUE)
scoringData$bedrooms[is.na(scoringData$bedrooms)] <- median(scoringData$bedrooms, na.rm=TRUE)
scoringData$beds[is.na(scoringData$beds)] <- median(scoringData$beds, na.rm=TRUE)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- median(scoringData$cleaning_fee, na.rm=TRUE)
scoringData$security_deposit[is.na(scoringData$security_deposit)] <- mean(scoringData$security_deposit, na.rm=TRUE)
### Zipcode ----
scoringData$zipcode <- gsub("[^0-9]", "", scoringData$zipcode, perl=TRUE)
scoringData$zipcode = substr(scoringData$zipcode, 1, 5)
occurance_test = data.frame(table(scoringData$zipcode))
occurance_low_test = subset(occurance_test,Freq < 10)
scoringData$zipcode = ifelse(scoringData$zipcode %in% occurance_low_test$Var1, "other", scoringData$zipcode)
scoringData$zipcode = ifelse(scoringData$zipcode == "","other", scoringData$zipcode)
### RoomType ----
# scoringData$room_type = ifelse(scoringData$room_type == "Entire home/apt", 1, scoringData$room_type)
# scoringData$room_type = ifelse(scoringData$room_type == "Hotel room", 2, scoringData$room_type)
# scoringData$room_type = ifelse(scoringData$room_type == "Private room", 3, scoringData$room_type)
# scoringData$room_type = ifelse(scoringData$room_type == "Shared room",4, scoringData$room_type)
### accommodates ----
accommodates_freq_scoring = data.frame(table(scoringData$accommodates))
low_accommodates_freq_scoring = subset(accommodates_freq_scoring,Freq < 100)
scoringData$accommodates =  ifelse(scoringData$accommodates %in% low_accommodates_freq_scoring$Var1,0,scoringData$accommodates)
### property type ----
proptype_freq_test = data.frame(table(scoringData$property_type))
low_proptype_freq_test = subset(proptype_freq_test,Freq < 10)
scoringData$property_type =  ifelse(scoringData$property_type %in% low_proptype_freq_test$Var1,'other',scoringData$property_type)
#Train and Test ----
library(rpart);library(randomForest);library(ipred);library(gbm);library(randomForest);library(caret)

set.seed(1731)
split = sample(x = 1:nrow(data),size = 0.8*nrow(data))
train = data[split,]
test = data[-split,]

## model ranger----
library(ranger)
set.seed(1031)
forest_ranger = ranger(price~guests_included+cleaning_fee+availability_90+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                       data = train)
pred_train = predict(forest_ranger,data = train)
rmsetrain = sqrt(mean((pred_train$predictions-train$price)^2)); rmsetrain
pred = predict(forest_ranger,data = test)
rmsetest = sqrt(mean((pred$predictions-test$price)^2)); rmsetest

forest_ranger = ranger(price~guests_included+cleaning_fee+availability_90+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                       data = train,
                       num.trees = 1000)
pred_train = predict(forest_ranger,data = train)
rmsetrain = sqrt(mean((pred_train$predictions-train$price)^2)); rmsetrain
pred = predict(forest_ranger,data = test)
rmsetest = sqrt(mean((pred$predictions-test$price)^2)); rmsetest

forest_ranger = randomForest(price~guests_included+cleaning_fee+availability_90+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,
                             data = train,
                             ntree = 1000,
                             mtry = 5)
pred_train = predict(forest_ranger,data = train)
rmsetrain = sqrt(mean((pred_train-train$price)^2)); rmsetrain
pred = predict(forest_ranger,newdata = test)
rmsetest = sqrt(mean((pred-test$price)^2)); rmsetest
# Model ----

library(randomForest)
rf = randomForest(price~zipcode+property_type+room_type+accommodates+bathrooms+bedrooms+cleaning_fee+guests_included+extra_people+availability_60+availability_365+review_scores_rating+review_scores_cleanliness+review_scores_location+instant_bookable,
                  data = data,
                  ntree = 1000)

# Scoring ----
# Read scoring data and apply model to generate predictions
pred = predict(rf,newdata=scoringData)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, '/Users/jiayihuang/Documents/APAN5200/rentlala2021/sample_submission7.csv',row.names = F)
