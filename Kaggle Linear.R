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
data$security_deposit[is.na(data$security_deposit)] <- mean(data$security_deposit, na.rm=TRUE)
## ZipCode ----
library(readr)
data$zipcode <- gsub("[^0-9]", "", data$zipcode, perl=TRUE)
data$zipcode = substr(data$zipcode, 1, 5)
zipcode_freq = data.frame(table(data$zipcode))
low_zipcide_freq = subset(zipcode_freq,Freq < 300)
data$zipcode = ifelse(data$zipcode %in% low_zipcide_freq$Var1, "00000", data$zipcode)
data$zipcode = ifelse(data$zipcode == "","00000", data$zipcode)
data$zipcode = parse_number(data$zipcode)
## RoomType ----
table(data$room_type)
data$room_type = ifelse(data$room_type == "Entire home/apt", 1, data$room_type)
data$room_type = ifelse(data$room_type == "Hotel room", 2, data$room_type)
data$room_type = ifelse(data$room_type == "Private room", 3, data$room_type)
data$room_type = ifelse(data$room_type == "Shared room",4, data$room_type)
## accommodates ----
accommodates_freq = data.frame(table(data$accommodates))
low_accommodates_freq = subset(accommodates_freq,Freq < 100)
data$accommodates =  ifelse(data$accommodates %in% low_accommodates_freq$Var1,0,data$accommodates)

proptype_freq = data.frame(table(data$property_type))
low_proptype_freq = subset(proptype_freq,Freq < 10)
data$property_type =  ifelse(data$property_type %in% low_proptype_freq$Var1,'00000',data$property_type)

data$property_type = ifelse(data$property_type == "Aparthotel",1,data$property_type)
data$property_type = ifelse(data$property_type == "Apartment",2,data$property_type)
data$property_type = ifelse(data$property_type == "Boutique hotel",3,data$property_type)
data$property_type = ifelse(data$property_type == "Bungalow",4,data$property_type)
data$property_type = ifelse(data$property_type == "Camper/RV",5,data$property_type)
data$property_type = ifelse(data$property_type == "Condominium",6,data$property_type)
data$property_type = ifelse(data$property_type == "Guest suite",7,data$property_type)
data$property_type = ifelse(data$property_type == "Guesthouse",8,data$property_type)
data$property_type = ifelse(data$property_type == "Hostel",9,data$property_type)
data$property_type = ifelse(data$property_type == "Hotel",10,data$property_type)
data$property_type = ifelse(data$property_type == "House",11,data$property_type)
data$property_type = ifelse(data$property_type == "Loft",12,data$property_type)
data$property_type = ifelse(data$property_type == "Other",13,data$property_type)
data$property_type = ifelse(data$property_type == "Resort",14,data$property_type)
data$property_type = ifelse(data$property_type == "Service apartment",15,data$property_type)
data$property_type = ifelse(data$property_type == "Tiny house",16,data$property_type)
data$property_type = ifelse(data$property_type == "Townhouse",17,data$property_type)
data$property_type = ifelse(data$property_type == "Villa",18,data$property_type)
data$property_type = parse_number(data$property_type)

data$instant_bookable = ifelse(data$instant_bookable == "t",1,0)
data$host_response_rate=as.character(data$host_response_rate)
data$host_acceptance_rate=as.character(data$host_acceptance_rate)
data$host_response_rate=parse_number(data$host_response_rate)
data$host_response_rate=as.numeric(data$host_response_rate)
data$host_acceptance_rate=parse_number(data$host_acceptance_rate)
data$host_acceptance_rate=as.numeric(data$host_acceptance_rate)
#data$requires_licenset<- as.numeric(data$requires_license)

table(data$requires_license)
library(rpart);library(randomForest);library(ipred);library(gbm);library(randomForest);library(caret)

set.seed(1731)
split = sample(x = 1:nrow(data),size = 0.8*nrow(data))
train = data[split,]
test = data[-split,]
#price~bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+square_feet+beds
library(glmnet);library(dplyr)
x = model.matrix(price~host_since+minimum_nights_avg_ntm+minimum_nights+maximum_nights+review_scores_rating+security_deposit+guests_included+cleaning_fee+bathrooms+bedrooms+zipcode+room_type+accommodates+review_scores_accuracy+beds,data=train)
y = train$price
set.seed(617)
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')
coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)


library(xgboost);library(dplyr)
price_label <- data %>%
  select(price)
select_par <- data.frame(data$minimum_nights,
                         data$minimum_nights_avg_ntm,
                         data$minimum_minimum_nights,
                         data$zipcode,
                         data$property_type,
                         data$room_type,
                         data$accommodates,
                         data$bathrooms,
                         data$bedrooms,
                         data$cleaning_fee,
                         data$guests_included,
                         data$extra_people,
                         data$availability_60,
                         data$availability_365,
                         data$instant_bookable,
                         data$review_scores_cleanliness,
                         data$review_scores_location,
                         data$review_scores_rating,
                         data$host_response_rate)
select_par_matrix = data.matrix(select_par)
numberOfTrainingSamples <- round(nrow(data) * .7)
train_data <- select_par_matrix[1:numberOfTrainingSamples,]
train_labels <- price_label[1:numberOfTrainingSamples,]
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
test_data <- select_par_matrix[-(1:numberOfTrainingSamples),]
test_labels <- price_label[-(1:numberOfTrainingSamples),]
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
xgboost = xgboost(data = dtrain, 
                  nrounds=182,
                  verbose = 1, 
                  max_depth = 6, 
                  eta = 0.161)
pred <- predict(xgboost, dtrain)
rmse_xgboost = sqrt(mean((pred - train_labels)^2)); rmse_xgboost
pred <- predict(xgboost, newdata = dtest)
rmse_xgboost = sqrt(mean((pred - test_labels)^2)); rmse_xgboost









# Model ----
library(ranger)
set.seed(1031)
forest_ranger_data = ranger(price~bathrooms+bedrooms+zipcode+room_type+accommodates,
                            data = data, 
                            num.trees = 1000)



#summary(model)
# Read scoring data and apply model to generate predictions
#Validation ----
scoringData = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/scoringData.csv')
## Imupute N/A ----
scoringData$square_feet[is.na(scoringData$square_feet)] <- median(scoringData$square_feet, na.rm=TRUE)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <- median(scoringData$bathrooms, na.rm=TRUE)
scoringData$bedrooms[is.na(scoringData$bedrooms)] <- median(scoringData$bedrooms, na.rm=TRUE)
scoringData$beds[is.na(scoringData$beds)] <- median(scoringData$beds, na.rm=TRUE)
## Zipcode ----
scoringData$zipcode <- gsub("[^0-9]", "", scoringData$zipcode, perl=TRUE)
scoringData$zipcode = substr(scoringData$zipcode, 1, 5)

occurance_test = data.frame(table(scoringData$zipcode))
occurance_low_test = subset(occurance_test,Freq < 10)


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
write.csv(submissionFile, '/Users/jiayihuang/Documents/APAN5200/rentlala2021/sample_submission3.csv',row.names = F)
