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
data$zipcode = ifelse(data$zipcode %in% low_zipcode_freq$Var1, "00000", data$zipcode)
data$zipcode = ifelse(data$zipcode == "","00000", data$zipcode)
data$zipcode = parse_number(data$zipcode)
### RoomType ----
data$room_type = ifelse(data$room_type == "Entire home/apt", 1, data$room_type)
data$room_type = ifelse(data$room_type == "Hotel room", 2, data$room_type)
data$room_type = ifelse(data$room_type == "Private room", 3, data$room_type)
data$room_type = ifelse(data$room_type == "Shared room",4, data$room_type)
### accommodates ----
accommodates_freq = data.frame(table(data$accommodates))
low_accommodates_freq = subset(accommodates_freq,Freq < 100)
data$accommodates =  ifelse(data$accommodates %in% low_accommodates_freq$Var1,0,data$accommodates)
### property type ----
proptype_freq = data.frame(table(data$property_type))
low_proptype_freq = subset(proptype_freq,Freq < 10)
data$property_type =  ifelse(data$property_type %in% low_proptype_freq$Var1,'other',data$property_type)
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
### instant bookable ----
data$instant_bookable = ifelse(data$instant_bookable == "t",1,0)
data$host_response_rate=as.character(data$host_response_rate)
data$host_acceptance_rate=as.character(data$host_acceptance_rate)
data$host_response_rate=parse_number(data$host_response_rate)
data$host_response_rate=as.numeric(data$host_response_rate)
data$host_acceptance_rate=parse_number(data$host_acceptance_rate)
data$host_acceptance_rate=as.numeric(data$host_acceptance_rate)

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
scoringData$zipcode = ifelse(scoringData$zipcode %in% occurance_low_test$Var1, "00000", scoringData$zipcode)
scoringData$zipcode = ifelse(scoringData$zipcode == "","00000", scoringData$zipcode)
scoringData$zipcode = parse_number(scoringData$zipcode)
### RoomType ----
scoringData$room_type = ifelse(scoringData$room_type == "Entire home/apt", 1, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Hotel room", 2, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Private room", 3, scoringData$room_type)
scoringData$room_type = ifelse(scoringData$room_type == "Shared room",4, scoringData$room_type)
### accommodates ----
accommodates_freq_scoring = data.frame(table(scoringData$accommodates))
low_accommodates_freq_scoring = subset(accommodates_freq_scoring,Freq < 100)
scoringData$accommodates =  ifelse(scoringData$accommodates %in% low_accommodates_freq_scoring$Var1,0,scoringData$accommodates)
### property type ----
proptype_freq_test = data.frame(table(scoringData$property_type))
low_proptype_freq_test = subset(proptype_freq_test,Freq < 10)
scoringData$property_type =  ifelse(scoringData$property_type %in% low_proptype_freq_test$Var1,'other',scoringData$property_type)
scoringscoringData$property_type =  ifelse(scoringData$property_type %in% low_proptype_freq$Var1,'other',scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Aparthotel",1,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Apartment",2,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Boutique hotel",3,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Bungalow",4,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Camper/RV",5,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Condominium",6,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Guest suite",7,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Guesthouse",8,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Hostel",9,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Hotel",10,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "House",11,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Loft",12,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Other",13,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Resort",14,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Service apartment",15,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Tiny house",16,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Townhouse",17,scoringData$property_type)
scoringData$property_type = ifelse(scoringData$property_type == "Villa",18,scoringData$property_type)
scoringData$property_type = parse_number(scoringData$property_type)
### instant bookable ----
scoringData$instant_bookable = ifelse(scoringData$instant_bookable == "t",1,0)
scoringData = cbind(scoringData, price = 0)
scoringData$host_response_rate=as.character(scoringData$host_response_rate)
scoringData$host_acceptance_rate=as.character(scoringData$host_acceptance_rate)
scoringData$host_response_rate=parse_number(scoringData$host_response_rate)
scoringData$host_response_rate=as.numeric(scoringData$host_response_rate)
scoringData$host_acceptance_rate=parse_number(scoringData$host_acceptance_rate)
scoringData$host_acceptance_rate=as.numeric(scoringData$host_acceptance_rate)

library(xgboost);library(caret)
set.seed(1031)
select_par_data <- data.frame(data$minimum_nights,
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
select_par_data_matrix = data.matrix(select_par_data)

select_par_scoringData <- data.frame(scoringData$minimum_nights,
                                     scoringData$minimum_nights_avg_ntm,
                                     scoringData$minimum_minimum_nights,
                                     scoringData$zipcode,
                                     scoringData$property_type,
                                     scoringData$room_type,
                                     scoringData$accommodates,
                                     scoringData$bathrooms,
                                     scoringData$bedrooms,
                                     scoringData$cleaning_fee,
                                     scoringData$guests_included,
                                     scoringData$extra_people,
                                     scoringData$availability_60,
                                     scoringData$availability_365,
                                     scoringData$instant_bookable,
                                     scoringData$review_scores_cleanliness,
                                     scoringData$review_scores_location,
                                     scoringData$review_scores_rating,
                                     scoringData$host_response_rate)
select_par_scoringData_matrix = data.matrix(select_par_scoringData)
colnames(select_par_data_matrix) <- NULL
xgboost = xgboost(data = select_par_data_matrix, label= data$price, 
                  nrounds=182,
                  verbose = 1, 
                  max_depth = 6, 
                  eta = 0.161)

pred <- predict(xgboost,select_par_scoringData_matrix)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, '/Users/jiayihuang/Documents/APAN5200/rentlala2021/sample_submission10.csv',row.names = F)

