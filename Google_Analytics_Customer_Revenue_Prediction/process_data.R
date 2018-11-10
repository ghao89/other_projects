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

#------------- Drop the columns with no use (sessionId)-------------#
train <- select(train, -sessionId)
test <- select(test, -sessionId)

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


#------------- Make certain columns character or factor -------------#
train$fullVisitorId <- as.character(train$fullVisitorId)
test$fullVisitorId <- as.character(test$fullVisitorId)


#------------- Take a subsample to explore -------------#
subtrain <- data.frame(train[sample(nrow(train), 10000), ])
visitNum <- aggregate(subtrain$visitNumber, by = list(subtrain$fullVisitorId), FUN = function(x) length(x))
maxVisitNum <- aggregate(subtrain$visitNumber, by = list(subtrain$fullVisitorId), FUN = function(x) max(x))

totalRevenue <- aggregate(train$transactionRevenue, by = list(train$fullVisitorId), sum)
