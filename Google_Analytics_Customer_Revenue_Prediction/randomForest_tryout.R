library(randomForest)
library(dplyr)

load(file = "Google_Analytics_Customer_Revenue_Prediction/intermediate.Rdata")

#------------- Deal with predictors with too many categories -------------#

#----- Geographical predictors -----#
geo_names <- c("continent", "subContinent", "country", "region", "metro", "city")

geo_features <- select(train, geo_names)
sapply(geo_features, nlevels)

withRevenue <- train[train$transactionRevenue != 0, ]



#------------- Take a subsample to explore -------------#
subtrain <- train[sample(nrow(train), 10000), ]

num_names <- names(subtrain)[which(sapply(select(subtrain, -transactionRevenue), class) == "numeric")]

rf_1 <- randomForest(x = select(subtrain, channelGrouping, isMobile, isTrueDirect, isVideoAd),
                     y = subtrain$transactionRevenue,
                     ntree = 500)
rf_2 <- randomForest(x = select(subtrain, c(visitNumber, continent, subContinent, num_names)),
                     y = subtrain$transactionRevenue,
                     ntree = 400)

t <- proc.time()
rf <- randomForest(x = select(train, -transactionRevenue, -country, -region, -metro, -city, -networkDomain, -browser, -gclId, -source, -keyword, -referralPath), 
                   y = train$transactionRevenue,
                   ntree = 5000,
                   mtry = 5,
                   sampsize = 10000,
                   replace = TRUE)
proc.time() - t
for (name in names(subtrain)) {
  if (class(subtrain[, name]) == "factor" && sum(is.na(subtrain[, name])) != 0) {
    subtrain[, name] <- fct_explicit_na(subtrain[, name], "placeholder")
  } else if (class(subtrain[, name == "logical"])) {
    subtrain[which(is.na(subtrain[, name])), name] <- 
  }
}