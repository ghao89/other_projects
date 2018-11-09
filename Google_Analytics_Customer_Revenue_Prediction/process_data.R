library(dplyr)
library(ggplot2)

load(file = "Google_Analytics_Customer_Revenue_Prediction/preprocessed.Rdata")

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



#------------- Remove the constant columns -------------#
const_col <- which(apply(train, 2, FUN = function(x) length(unique(x)) == 1))
const_col_names <- colnames(train)[const_col]

train <- select(train, -const_col_names)
test <- select(test, -const_col_names)

#------------- Remove the constant columns -------------#

#------------- Take a subsample to explore -------------#
subtrain <- data.frame(train[sample(nrow(train), 10000), ])
visitNum <- aggregate(subtrain$visitNumber, by = list(subtrain$fullVisitorId), FUN = function(x) length(x))
maxVisitNum <- aggregate(subtrain$visitNumber, by = list(subtrain$fullVisitorId), FUN = function(x) max(x))

totalRevenue <- aggregate(train$transactionRevenue, by = list(train$fullVisitorId), sum)
