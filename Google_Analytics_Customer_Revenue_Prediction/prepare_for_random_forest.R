library(randomForest)
library(dplyr)

# load(file = "Google_Analytics_Customer_Revenue_Prediction/intermediate.Rdata")
# 
# #------------- Deal with date -------------#
# train_ref_date <- 20170101
# test_ref_date <- 20180101
# 
# train$date <- unlist(lapply(train$date, FUN = function(x) 
#   as.double(strptime(x, format = "%Y%m%d") - strptime(train_ref_date, format = "%Y%m%d"))))
# test$date <- unlist(lapply(test$date, FUN = function(x)
#   as.double(strptime(x, format = "%Y%m%d") - strptime(test_ref_date, format = "%Y%m%d"))))
# 
# #------------- Deal with factors -------------#
# train$y <- as.numeric(train$transactionRevenue != 0)
# 
# var_class <- sapply(train, class)
# var_names_tr <- names(train)
# var_names_te <- names(test)
# 
# factor_names <- NULL
# 
# for (i in 1:length(var_class)) {
#   var <- var_class[i]
#   var_name <- var_names_tr[i]
#   if (var != "numeric" && var != "integer") {
#     agg <- aggregate(train$y, by = list(train[,i]), FUN = function(x) sum(x)/length(x))
#     colnames(agg) <- c(var_names_tr[i], paste0(var_names_tr[i], "_x"))
#     train <- left_join(train, agg, by = var_name)
#     test <- left_join(test, agg, by = var_name)
#     
#     if (var_name != "fullVisitorId") {
#       factor_names <- c(factor_names, var_name)
#     }
#     print(var_name)
#   }
# }

load(file = "Google_Analytics_Customer_Revenue_Prediction/prepared.Rdata")

train <- select(train, -factor_names)
test <- select(test, -factor_names)

#----------RandomForest tryout----------#

x <- select(train, -transactionRevenue, -y, -fullVisitorId)
x <- data.frame(sapply(x, FUN = function(x) x/max(x)))
y <- train$transactionRevenue

num_epoch <- 1
batch_size <- 25000
val_size <- nrow(train) - 30*batch_size

val_idx <- sample(nrow(train), val_size, replace = F)

val_x <- x[val_idx, ]
val_y <- y[val_idx]

tr_x <- x[-val_idx, ]
tr_y <- y[-val_idx]

nr <- nrow(tr_x)

batch_x <- tr_x[1:batch_size, ]
batch_y <- tr_y[1:batch_size]
rf_all <- randomForest(x = batch_x,
                       y = batch_y,
                       ntree = 50)

for (j in 1:num_epoch) {
  i <- ifelse(j == 1, 1, 0)
  while((i + 1)*batch_size <= nr) {
    batch_x <- tr_x[i*batch_size + (1:batch_size),]
    batch_y <- tr_y[i*batch_size + (1:batch_size)]
    rf_batch <- randomForest(x = batch_x,
                             y = batch_y,
                             ntree = 50)
    rf_all <- randomForest::combine(rf_all, rf_batch)  
    print(i)
    i <- i + 1
  }
  # 
  # batch_x <- tr_x[(i*batch_size + 1):nr, ]
  # batch_y <- tr_y[(i*batch_size + 1):nr]
  # rf_batch <- randomForest(x = batch_x,
  #                          y = batch_y,
  #                          ntree = 50)
  # rf_all <- randomForest::combine(rf_all, rf_batch)

}
pred_tr_y <- NULL
i <- 0
tse <- 0

while ((i + 1)*batch_size <= nr) {
  batch_x <- tr_x[i*batch_size + (1:batch_size),]
  batch_y <- tr_y[i*batch_size + (1:batch_size)]
  pred_y <- predict(rf_all, batch_x)
  tse <- tse + (pred_y - batch_y)^2
  pred_tr_y <- c(pred_tr_y, pred_y)
  print(i)
  i <- i + 1
}

rmse <- sqrt(mean(tse))

#------------
te_x <- select(test, -y, -fullVisitorId)
te_x$fullVisitorId_x <- ifelse(is.na())

test$fullVisitorId_x

