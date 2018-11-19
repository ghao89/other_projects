library(dplyr)
library(ggplot2)
library(forcats)

load(file = "Google_Analytics_Customer_Revenue_Prediction/preprocessed.Rdata")

#------------- Drop the constant columns -------------#
const_col <- which(apply(train, 2, FUN = function(x) length(unique(x)) == 1))
const_col_names <- colnames(train)[const_col]

train <- select(train, -const_col_names)
test <- select(test, -const_col_names)


#------------- Columns with NA's-------------#
na_col <- which(apply(train, 2, FUN = function(x) sum(is.na(x)) != 0))
na_col_names <- colnames(train)[na_col]

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


#------------- Convert necessary columns from characters to factors -------------#
train$continent <- fct_explicit_na(train$continent, "We don't know!")
test$continent <- fct_explicit_na(test$continent, "We don't know!")

train$subContinent <- fct_explicit_na(train$subContinent, "We don't know!")
test$subContinent <- fct_explicit_na(test$subContinent, "We don't know!")

train$country <- fct_explicit_na(train$country, "We don't know!")
test$country <- fct_explicit_na(test$country, "We don't know!")

train$region <- fct_explicit_na(train$region, "We don't know!")
test$region <- fct_explicit_na(test$region, "We don't know!")

train$city <- fct_explicit_na(train$city, "We don't know!")
test$city <- fct_explicit_na(test$city, "We don't know!")

train$metro <- fct_explicit_na(train$metro, "We don't know!")
test$metro <- fct_explicit_na(test$metro, "We don't know!")

train$medium <- fct_explicit_na(train$medium, "We don't know!")
test$medium <- fct_explicit_na(test$medium, "We don't know!")

train$networkDomain <- fct_explicit_na(train$networkDomain, "We don't know!")
test$networkDomain <- fct_explicit_na(test$networkDomain, "We don't know!")

train$browser <- fct_explicit_na(train$browser, "We don't know!")
test$browser <- fct_explicit_na(test$browser, "We don't know!")

train$operatingSystem <- fct_explicit_na(train$operatingSystem, "We don't know!")
test$operatingSystem <- fct_explicit_na(test$operatingSystem, "We don't know!")

train$deviceCategory <- fct_explicit_na(train$deviceCategory, "We don't know!")
test$deviceCategory <- fct_explicit_na(test$deviceCategory, "We don't know!")

train$campaign <- fct_explicit_na(train$campaign, "We don't know!")
test$campaign <- fct_explicit_na(test$campaign, "We don't know!")

train$source <- fct_explicit_na(train$source, "We don't know!")
test$source <- fct_explicit_na(test$source, "We don't know!")

train$keyword <- fct_explicit_na(train$keyword, "We don't know!")
test$keyword <- fct_explicit_na(test$keyword, "We don't know!")

train$referralPath <- fct_explicit_na(train$referralPath, "We don't know!")
test$referralPath <- fct_explicit_na(test$referralPath, "We don't know!")

train$page <- fct_explicit_na(train$page, "We don't know!")
test$page <- fct_explicit_na(test$page, "We don't know!")

train$slot <- fct_explicit_na(train$slot, "We don't know!")
test$slot <- fct_explicit_na(test$slot, "We don't know!")

train$gclId <- fct_explicit_na(train$gclId, "We don't know!")
test$gclId <- fct_explicit_na(test$gclId, "We don't know!")

train$adNetworkType <- fct_explicit_na(train$adNetworkType, "We don't know!")
test$adNetworkType <- fct_explicit_na(test$adNetworkType, "We don't know!")

train$adContent <- fct_explicit_na(train$adContent, "We don't know!")
test$adContent <- fct_explicit_na(test$adContent, "We don't know!")

#------------- Columns with NA's-------------#
na_col <- which(apply(train, 2, FUN = function(x) sum(is.na(x)) != 0))
na_col_names <- colnames(train)[na_col]
# "isTrueDirect" and "isVideoAd"

#------------- Convert necessary columns from logical to factors -------------#
logical_col <- which(sapply(train, typeof) == "logical")
logical_col_names <- colnames(train)[logical_col]

for (name in logical_col_names) {
  train[, name] <- fct_explicit_na(factor(train[, name]), "We don't know!")
  test[, name] <- fct_explicit_na(factor(test[, name]), "We don't know!")
}

#------------- Save the intermediate step train and test -------------#

save(train, test, file = "Google_Analytics_Customer_Revenue_Prediction/intermediate.Rdata")
