### Some data cleaning examples for the Kaggle project ###

## @author Zachary D Kurtz

data <- read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/analysisData.csv')
scoringData <- read.csv('/Users/jiayihuang/Documents/APAN5200/rentlala2021/scoringData.csv')

dim(data)
dim(scoringData)
head(data$price)

hist(data$price)

## 
setdiff(
  unique(data$zipcode),
  unique(scoringData$zipcode)  
)

unique(scoringData$zipcode)


library(dplyr)

data <- data %>% mutate(type = "analysis")
scoringData <- scoringData %>% mutate(type = "scoring")

combinedData <- bind_rows(data, scoringData)
dim(combinedData)

## Zip code example
unique(combinedData$zipcode)

library(readr)
unique(parse_number(combinedData$zipcode))

as.character(parse_number(combinedData$zipcode))
unique(combinedData$zipcode)

# regex
## \d -> any number
combinedData$zipcode <- gsub("[^\\d]", "", combinedData$zipcode, perl=TRUE)
unique(combinedData$zipcode)
combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)

table(nchar(combinedData$zipcode))
combinedData$zipcode[nchar(combinedData$zipcode) < 5] <- NA_character_
combinedData$zipcode <- as.factor(combinedData$zipcode)


## City ##
unique(combinedData$city)
unique(tolower(combinedData$city))

## 'tokenize'
## remove commas, dashes, newlines, parens and periods
combinedData$city <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$city))
unique(tolower(combinedData$city))
## trim whitespace
combinedData$city <- stringr::str_trim(gsub("\\s+", " ", combinedData$city))
unique(tolower(combinedData$city))


## Fix property types

# Replace properties with fewer than 10 examples
sort(table(combinedData$property_type))


library(forcats)
?fct_lump_min
levels(fct_lump_min(combinedData$property_type, min=11))

combinedData %>%
  mutate(
    property_type = ifelse(
      property_type %in%  names(which(table(property_type) <= 10)),
      NA_character_,
      property_type
    )
  ) -> combinedData


## NA types
sum(combinedData=="", na.rm=TRUE)
sum(combinedData=="N/A", na.rm=TRUE)
sum(combinedData=="NA", na.rm=TRUE)

?case_when
char2na <- function(x) {
  x <- as.character(x)
  return(case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  ))
}

char2na(c("foo", "bar", "", "N/A"))

?mutate_if
combinedData <- combinedData %>%
                    mutate_if(is.character, char2na) %>%
                    mutate_if(is.factor, char2na)
sum(combinedData=="", na.rm=TRUE)
## make detailed notes about this code
### arrayInd(which(combinedData==""), dim(combinedData))
## combinedData[,32]

char_ind <-  which(sapply(combinedData, class) == "character")

sum(table(combinedData[,char_ind[1]])==1)
sum(table(combinedData[,char_ind[2]])==1)


#
library(caret)
zero_var_table <- nearZeroVar(combinedData, saveMetrics= TRUE)
combinedData <- combinedData[, !zero_var_table$nzv]


## Imputation ##
## no cleaning fee, maybe?
combinedData$cleaning_fee[is.na(combinedData$cleaning_fee)] <- 0

## median replacement
combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)


numeric_predictors <- which(colnames(combinedData) != "price" & 
                                sapply(combinedData, is.numeric))

?preProcess
imp_model_med <- preProcess(combinedData[,numeric_predictors], 
                              method = 'medianImpute')
imp_model_bag <- preProcess(combinedData[,numeric_predictors], 
                             method = 'bagImpute')

set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_bag, newdata=combinedData[,numeric_predictors])


data <- combinedData[!is.na(combinedData$price),]
scoringData <- combinedData[is.na(combinedData$price),]


