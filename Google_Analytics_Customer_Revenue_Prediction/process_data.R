train$transactionRevenue <- unlist(lapply(train$transactionRevenue, FUN = function(x)
  ifelse(is.na(x), 0, as.numeric(x))))
