library(dplyr)
library(ggplot2)

load(file = "Google_Analytics_Customer_Revenue_Prediction/preprocessed.Rdata")

#------------- Drop the constant columns -------------#
const_col <- which(apply(train, 2, FUN = function(x) length(unique(x)) == 1))
const_col_names <- colnames(train)[const_col]

train <- select(train, -const_col_names)
test <- select(test, -const_col_names)


#------------- Drop the columns that contained in train but not in test (except transactionRevenue)-------------#
diff_col <- colnames(train)[!colnames(train) %in% c(colnames(test), "transactionRevenue")]
train <- select(train, -diff_col)

#------------- Drop the columns with no use (sessionId & visitId)-------------#
train <- select(train, -sessionId, -visitId, -targetingCriteria)
test <- select(test, -sessionId, -visitId, -targetingCriteria)

#------------- Convert necessary columns into numeric -------------#
# transaction revenue
train$transactionRevenue <- unlist(lapply(train$transactionRevenue, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))

# hits
train$hits <- as.numeric(train$hits)
test$hits <- as.numeric(test$hits)

# pageviews (contain NA)
train$pageviews <- unlist(lapply(train$pageviews, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))
test$pageviews <- unlist(lapply(test$pageviews, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))

# bounces
train$bounces <- unlist(lapply(train$bounces, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))
test$bounces <- unlist(lapply(test$bounces, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))

# new visits
train$newVisits <- unlist(lapply(train$newVisits, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))
test$newVisits <- unlist(lapply(test$newVisits, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))


#------------- Convert necessary columns into characters of factors -------------#
train$continent <- as.factor(train$continent)
test$continent <- as.factor(test$continent)

train$subContinent <- as.factor(train$subContinent)
test$subContinent <- as.factor(test$subContinent)

train$country <- as.factor(train$country)
test$country <- as.factor(test$country)

train$region <- as.factor(train$region)
test$region <- as.factor(test$region)

train$city <- as.factor(train$city)
test$city <- as.factor(test$city)

train$metro <- as.factor(train$metro)
test$metro <- as.factor(test$metro)

train$medium <- as.factor(train$medium)
test$medium <- as.factor(test$medium)

train$networkDomain <- as.factor(train$networkDomain)
test$networkDomain <- as.factor(test$networkDomain)

train$browser <- as.factor(train$browser)
test$browser <- as.factor(test$browser)

train$operatingSystem <- as.factor(train$operatingSystem)
test$operatingSystem <- as.factor(test$operatingSystem)

train$deviceCategory <- as.factor(train$deviceCategory)
test$deviceCategory <- as.factor(test$deviceCategory)

train$campaign <- as.factor(train$campaign)
test$campaign <- as.factor(test$campaign)

train$source <- as.factor(train$source)
test$source <- as.factor(test$source)

train$keyword <- as.factor(train$keyword)
test$keyword <- as.factor(test$keyword)

train$referralPath <- as.factor(train$referralPath)
test$referralPath <- as.factor(test$referralPath)

train$page <- as.factor(train$page)
test$page <- as.factor(test$page)

train$slot <- as.factor(train$slot)
test$slot <- as.factor(test$slot)

train$gclId <- as.factor(train$gclId)
test$gclId <- as.factor(test$gclId)

train$adNetworkType <- as.factor(train$adNetworkType)
test$adNetworkType <- as.factor(test$adNetworkType)

train$adContent <- as.factor(train$adContent)
test$adContent <- as.factor(test$adContent)

#------------- Save the intermediate step train and test -------------#

save(train, test, file = "Google_Analytics_Customer_Revenue_Prediction/intermediate.Rdata")
