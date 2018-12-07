load(file = "Google_Analytics_Customer_Revenue_Prediction/prepared.Rdata")
source(file = "Google_Analytics_Customer_Revenue_Prediction/nn.R")

train <- select(train, -factor_names)
test <- select(test, -factor_names)

#----------prepare for NN----------#

x <- select(train, -y, -fullVisitorId)
x <- data.frame(sapply(x, FUN = function(x) x/max(x)))

y_name <- "transactionRevenue"

nr <- nrow(x)
tr_size <- 100000
val_size <- 10000

tr_idx <- sample(nr, tr_size, replace = F)
val_idx <- sample((1:nr)[-tr_idx], val_size, replace = F)

val <- x[val_idx, ]
tr <- x[tr_idx, ]

nn_res <- FuncValidation_LOOCV_NN_all(y_name = y_name,
                                      train = tr,
                                      valid = val,
                                      n_neurons = 200,
                                      dp = 0.5, 
                                      batchsize = 1000,
                                      iter = 100)
