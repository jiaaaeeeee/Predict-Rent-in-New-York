# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
# Read data and construct a simple model

data = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/analysisData.csv')
# Data Clean ----
## Impute N/A by median ----
data$square_feet[is.na(data$square_feet)] <- median(data$square_feet, na.rm=TRUE)
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms, na.rm=TRUE)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms, na.rm=TRUE)

## ZipCode ----
data$zipcode <- gsub("[^\\d]", "", data$zipcode, perl=TRUE)
data$zipcode = substr(data$zipcode, 1, 5)
occurance = data.frame(table(data$zipcode))
occurance_low = subset(occurance,Freq < 10)
occurance_low = unique(occurance_low)
data$zipcode = ifelse(data$zipcode %in% occurance_low$Var1, "other", data$zipcode)
#data$zipcode <- as.factor(data$zipcode)


# model = lm(price~minimum_nights+review_scores_accuracy+square_feet+zipcode,data)
library(rpart);library(randomForest)
set.seed(1731)
split = sample(x = 1:nrow(data),size = 0.75*nrow(data))
train = data[split,]
test = data[-split,]

tuneGrid = expand.grid(cp = seq(0,0.4,0.001))
library(caret)
trControl = trainControl(method = 'cv',number = 5)
set.seed(1031)
tree_cv = train(price~review_scores_accuracy+square_feet+bathrooms+bedrooms+zipcode,
                data = train,
                method = 'rpart',
                trControl = trControl,
                tuneGrid = tuneGrid)
model = rpart(price~review_scores_accuracy+square_feet+bathrooms+bedrooms+zipcode,
           data = train,
           method = "anova",
           cp = 0)
summary(model)
pred = predict(model,newdata = test)
rmsetest = sqrt(mean((pred-test$price)^2)); rmsetest


# Model ----
library(rpart); library(rpart.plot)
tuneGrid = expand.grid(cp = seq(0,0.4,0.001))
library(caret)
trControl = trainControl(method = 'cv',number = 5)
set.seed(1031)
tree_cv = train(price~review_scores_accuracy+square_feet+bathrooms+bedrooms+zipcode,
                data = data,
                method = 'rpart',
                trControl = trControl,
                tuneGrid = tuneGrid)
model = rpart(price~review_scores_accuracy+square_feet+bathrooms+bedrooms+zipcode,
              data = data,
              method = "anova",
              cp = 0)
#summary(model)
# Read scoring data and apply model to generate predictions
#Validation ----
scoringData = read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/scoringData.csv')
scoringData$square_feet[is.na(scoringData$square_feet)] <- median(scoringData$square_feet, na.rm=TRUE)
scoringData$bathrooms[is.na(scoringData$bathrooms)] <- median(scoringData$bathrooms, na.rm=TRUE)
scoringData$bedrooms[is.na(scoringData$bedrooms)] <- median(scoringData$bedrooms, na.rm=TRUE)


scoringData$zipcode <- gsub("[^\\d]", "", scoringData$zipcode, perl=TRUE)
scoringData$zipcode = substr(scoringData$zipcode, 1, 5)

occurance_test = data.frame(table(scoringData$zipcode))
occurance_low_test = subset(occurance_test,Freq < 10)
occurance_low_test = unique(occurance_low_test)


scoringData$zipcode = ifelse(scoringData$zipcode %in% occurance_low_test$Var1, "other", scoringData$zipcode)
#data$zipcode <- as.factor(data$zipcode)
table(scoringData$zipcode)


pred = predict(model,newdata=scoringData)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, '/Users/jiayihuang/Documents/APAN5200/rentlala2021/sample_submission1.csv',row.names = F)
