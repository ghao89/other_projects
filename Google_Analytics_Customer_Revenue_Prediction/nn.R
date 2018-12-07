library(dplyr)
library(randomForest)
library(ROCR)
library(hash)
library(VGAM)
library(gridExtra)


FuncValidation_LOOCV_NN_all <- function(y_name, 
                                        train, 
                                        valid, 
                                        test = NULL, 
                                        n_neurons = 16L,
                                        dp = 0.5,
                                        iter = 10000,
                                        batchsize = 50,
                                        trace_gap = 100,
                                        centerScale = FALSE,
                                        lr = 1e-4,
                                        beta = 0.1,
                                        reg_method = "L2",
                                        log = TRUE) {
  
  ### Prepare the training data and validation data and test data
  tr_y <- train[, which(names(train) == y_name)]
  vd_y <- valid[, which(names(valid) == y_name)]
  
  if (log) {
    tr_y <- log10(tr_y)
    vd_y <- log10(vd_y)
  }
  
  tr_x <- as.matrix(train[, which(names(train) != y_name)])
  vd_x <- as.matrix(valid[, which(names(valid) != y_name)])
  if (!is.null(test)) {
    te_x <- as.matrix(test[, which(names(test) != y_name)])
  }
  
  n_input_col <- ncol(tr_x)
  n_output_col <- 1L
  
  if (centerScale) {
    
    ### Format the training and validation data by centering and standardizing
    tr_x <- apply(tr_x, 2, FUN = function(x) x - mean(x))/apply(tr_x, 2, sd)
    vd_x <- apply(vd_x, 2, FUN = function(x) x - mean(x))/apply(vd_x, 2, sd)
    if(!is.null(test)) {
      te_x <- apply(te_x, 2, FUN = function(x) x - mean(x))/apply(te_x, 2, sd)
    }
    
  }
  
  ### Trace of training error and validation error
  tr_trace <- vector("numeric", floor(iter/trace_gap))
  vd_trace <- vector("numeric", floor(iter/trace_gap))
  
  ### Implement the nn
  require(tensorflow)
  sess <- tf$InteractiveSession()
  
  x <- tf$placeholder(tf$float32, shape(NULL, n_input_col))
  y_ <- tf$placeholder(tf$float32, shape= NULL)
  
  weight_variable <- function(shape) {
    initial <- tf$truncated_normal(shape, stddev=0.1)
    tf$Variable(initial)
  }
  
  bias_variable <- function(shape) {
    initial <- tf$constant(0.1, shape=shape)
    tf$Variable(initial)
  }
  
  ### Input layer to 1st hidden layer (32 neurons)
  W_fc1 <- weight_variable(shape(n_input_col, n_neurons))
  b_fc1 <- bias_variable(shape(n_neurons))
  
  h_fc1 <- tf$nn$relu(tf$matmul(x, W_fc1) + b_fc1)
  
  keep_prob <- tf$placeholder(tf$float32)
  h_fc1_drop <- tf$nn$dropout(h_fc1, keep_prob)
  
  ### 1st hidden layer to 2nd hidden layer (8 neurons)
  W_fc2 <- weight_variable(shape(n_neurons, n_neurons))
  b_fc2 <- bias_variable(shape(n_neurons))
  
  h_fc2 <- tf$nn$relu(tf$matmul(h_fc1, W_fc2) + b_fc2)
  
  keep_prob <- tf$placeholder(tf$float32)
  h_fc2_drop <- tf$nn$dropout(h_fc2, keep_prob)
  
  # ### 2nd hidden layer to 3rd hidden layer
  #
  # W_fc3 <- weight_variable(shape(n_neurons, n_neurons))
  # b_fc3 <- bias_variable(shape(n_neurons))
  #
  # h_fc3<- tf$nn$relu(tf$matmul(h_fc2, W_fc3) + b_fc3)
  #
  # keep_prob <- tf$placeholder(tf$float32)
  # h_fc3_drop <- tf$nn$dropout(h_fc3, keep_prob)
  
  ### 2nd hidden layer to output layer
  
  W_fc3 <- weight_variable(shape(n_neurons, n_output_col))
  b_fc3 <- bias_variable(shape(n_output_col))
  
  h_fc_output <- tf$nn$relu(tf$matmul(h_fc2_drop, W_fc3) + b_fc3)
  
  ### Objective function, using cross-entropy
  
  # cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(h_fc_output), reduction_indices=1L))
  mse <- tf$losses$mean_squared_error(y_, h_fc_output)
  if (reg_method == "L2") {
    regularizer <- tf$nn$l2_loss(W_fc1) + tf$nn$l2_loss(W_fc2) + tf$nn$l2_loss(W_fc3)
  } else if(reg_method == "L1") {
    regularizer <- tf$reduce_sum(tf$abs(W_fc1)) + tf$reduce_sum(tf$abs(W_fc2)) + tf$reduce_sum(tf$abs(W_fc3))
  }
  
  train_step <- tf$train$AdamOptimizer(lr)$minimize(mse + beta * regularizer)
  a <- tf$cast(tf$argmax(h_fc_output, 1L), tf$float32)
  b <- tf$cast(tf$argmax(y_, 1L), tf$float32)
  
  ### Metrics to evaluate the result
  correct_prediction <- tf$equal(a, b)
  accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))
  ### auc <- tf$contrib$metrics$streaming_auc(a, b)
  
  sess$run(tf$global_variables_initializer())
  sess$run(tf$local_variables_initializer())
  
  ### Training and validating
  
  if(nrow(tr_x) < batchsize) {
    for (i in 1:iter) {
      if (i %% trace_gap == 0) {
        train_accuracy <- accuracy$eval(feed_dict = dict(
          x = tr_x, y_ = tr_y, keep_prob = 0.999))
        # cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
        tr_trace[i/trace_gap] <- train_accuracy
        
        validation_accuracy <- accuracy$eval(feed_dict = dict(x = vd_x, y_ = vd_y, keep_prob = 0.999))
        # cat(sprintf("step %d, validation accuracy %g\n", i, validation_accuracy))
        vd_trace[i/trace_gap] <- validation_accuracy
      }
      train_step$run(feed_dict = dict(
        x = tr_x, y_ = tr_y, keep_prob = dp))
    }
  } else {
    for (i in 1:iter) {
      #idx <- 1:batchsize
      if (i %% trace_gap == 0) {
        train_accuracy <- accuracy$eval(feed_dict = dict(
          x = tr_x, y_ = tr_y, keep_prob = 0.999))
        # cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
        tr_trace[i/trace_gap] <- train_accuracy
        
        validation_accuracy <- accuracy$eval(feed_dict = dict(x = vd_x, y_ = vd_y, keep_prob = 0.999))
        # cat(sprintf("step %d, validation accuracy %g\n", i, validation_accuracy))
        vd_trace[i/trace_gap] <- validation_accuracy
      }
      idx <- sample(nrow(tr_x), batchsize)
      train_step$run(feed_dict = dict(x = tr_x[idx, ], y_ = tr_y[idx], keep_prob = dp))
      #idx <- (batchsize + idx) %% nrow(tr_x)
    }
  }
  
  ### Validating
  
  validation_accuracy <- accuracy$eval(feed_dict = dict(
    x = vd_x, y_ = vd_y, keep_prob = 0.999))
  
  ### test_auc <- sess$run(auc, feed_dict = dict(x = te_x, y_ = te_y, keep_prob = 1.0))
  
  pred <- sess$run(h_fc_output, feed_dict = dict(x = te_x, keep_prob = 0.999))
  pred <- apply(pred, 2, FUN = function(x) (x - min(x))/(max(x) - min(x)))
  W1 <- sess$run(W_fc1, feed_dict = dict(x = tr_x, y_ = tr_y)) 
  W2 <- sess$run(W_fc2, feed_dict = dict(x = tr_x, y_ = tr_y)) 
  W3 <- sess$run(W_fc3, feed_dict = dict(x = tr_x, y_ = tr_y)) 
  b1 <- sess$run(b_fc1, feed_dict = dict(x = tr_x, y_ = tr_y))
  b2 <- sess$run(b_fc2, feed_dict = dict(x = tr_x, y_ = tr_y))
  b3 <- sess$run(b_fc3, feed_dict = dict(x = tr_x, y_ = tr_y))
  h1 <- sess$run(h_fc1, feed_dict = dict(x = tr_x, y_ = tr_y))
  h2 <- sess$run(h_fc2, feed_dict = dict(x = tr_x, y_ = tr_y))
  
  ### Return a list of prediction results, AUC and ROC curve
  res <- list(pred = pred, validation_accuracy = validation_accuracy, tr_trace = tr_trace, vd_trace = vd_trace, W1 = W1, W2 = W2, W3 = W3, b1 = b1, b2 = b2, b3 = b3, h1 = h1, h2 = h2)
  
  print("This cycle is done!")
  tf$reset_default_graph()
  sess$close()
  return(res)
}
