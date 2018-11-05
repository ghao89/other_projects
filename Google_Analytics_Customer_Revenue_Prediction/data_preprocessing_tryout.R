library(dplyr)

train <- read.csv("Google_Analytics_Customer_Revenue_Prediction//train.csv")

# Use a subset of train data set to preprocess
subtrain <- train[sample(1:nrow(train), 10000), ]

############################################################
######      Subtract information from geoNetwork      ######
############################################################

subtrain$geoList <- lapply(subtrain$geoNetwork, FUN = function(x)
  unlist(lapply(unlist(strsplit(as.character(x), split = ",")), FUN = function(y)
    unlist(strsplit(y, "[:]"))
  )
  )
)

subtrain$continent <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[2])))
subtrain$subContinent <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[4])))
subtrain$country <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[6])))
subtrain$region <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[8])))
subtrain$metro <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[10])))
subtrain$city <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[12])))
subtrain$cityId <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[14])))
subtrain$networtDomain <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[16])))
subtrain$latitude <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[18])))
subtrain$longitude <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[20])))
subtrain$networkLocation <- as.factor(unlist(lapply(subtrain$geoList, FUN = function(x) x[22])))

# Drop the original geoNetwork column and temporary geoList column
subtrain <- select(subtrain, -geoNetwork, -geoList)

############################################################
######        Subtract information from device        ######
############################################################

subtrain$deviceList <- lapply(subtrain$device, FUN = function(x)
  unlist(lapply(unlist(strsplit(as.character(x), split = ",")), FUN = function(y)
    unlist(strsplit(y, "[:]"))
  )
  )
)

subtrain$brower <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[2])))
subtrain$browerVersion <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[4])))
subtrain$browerSize <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[6])))
subtrain$operatingSystem <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[8])))
subtrain$operatingSystemVersion <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[10])))
subtrain$isMobile <- unlist(lapply(subtrain$deviceList, FUN = function(x) x[12]))
subtrain$mobileDeviceBranding <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[14])))
subtrain$mobileDeviceModel <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[16])))
subtrain$mobileInputSelector <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[18])))
subtrain$mobileDeviceInfo <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[20])))
subtrain$mobileDeviceMarketingName <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[22])))
subtrain$flashVersion <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[24])))
subtrain$language <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[26])))
subtrain$screenColors <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[28])))
subtrain$screenResolution <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[30])))
subtrain$deviceCategory <- as.factor(unlist(lapply(subtrain$deviceList, FUN = function(x) x[32])))

subtrain <- select(subtrain, -device, -deviceList)

############################################################
######        Subtract information from totals        ######
############################################################

subtrain$totalList <- lapply(subtrain$totals, FUN = function(x)
  unlist(lapply(unlist(strsplit(as.character(x), split = ",")), FUN = function(y)
    unlist(strsplit(y, "[:]"))
  )
  )
)

subtrain$visits <- unlist(lapply(subtrain$deviceList, FUN = function(x) x[2]))
subtrain$hits <- unlist(lapply(subtrain$deviceList, FUN = function(x) x[4]))
subtrain$pageViews <- unlist(lapply(subtrain$deviceList, FUN = function(x) x[6]))
subtrain$newVisits <- unlist(lapply(subtrain$deviceList, FUN = function(x) x[8]))
