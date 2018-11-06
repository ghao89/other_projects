library(dplyr)
library(tidyverse)
library(jsonlite)

t <- proc.time()

train <- read.csv("Google_Analytics_Customer_Revenue_Prediction//train.csv")

# # Use a subset of train data set to preprocess
# train <- train[sample(1:nrow(train), 10000), ]

geoNetwork <- bind_rows(lapply(train$geoNetwork, FUN = function(x) fromJSON(as.character(x))))
device <- bind_rows(lapply(train$device, FUN = function(x) fromJSON(as.character(x))))
totals <- bind_rows(lapply(train$totals, FUN = function(x) fromJSON(as.character(x))))
trafficSource <- bind_rows(lapply(train$trafficSource, FUN = function(x) 
  as.character(x) %>%
    fromJSON() %>%
    purrr::flatten() %>%
    map_if(is_list, as_tibble) %>%
    map_if(is_tibble, list) %>%
    bind_cols()
))

train <- select(train, -geoNetwork, -device, -totals, -trafficSource)
train <- bind_cols(train, geoNetwork, device, totals, trafficSource)

proc.time() - t
