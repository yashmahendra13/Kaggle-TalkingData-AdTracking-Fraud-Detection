##################################################################################################
## TIME FEATURE.
## Based on click time the data is bucketed into groups of 1 min, 5 min, 1 hr, 3 hr and 6 hrs
## Then the mean, max, variance and skewnwss of these bucket's frequencies are prepared
## Finally these features are added to the existing dataset and returned to caller as 
## added features

#Round down the minutes to a mutliple of mnslot
bucketInMins <- function (dtList, mnslot) { 
  str <- strptime(dtList,"%Y-%m-%d %H:%M:%S")
  min_st <- as.numeric(format(str, "%M"))
  roundedmins <- floor(min_st/mnslot) * mnslot
  base <- strptime(dtList, "%Y-%m-%d %H")
  bucket <- base + (roundedmins * 60)
  return (data.frame(table(bucket)))
}

#Round down the hours to a mutliple of hrslot
bucketInHrs <- function (dtList, hrslot) { 
  str <- strptime(dtList,"%Y-%m-%d %H:%M:%S")
  hr_st <- as.numeric(format(str, "%H"))
  roundedhrs <- floor(hr_st/hrslot) * hrslot
  base <- strptime(dtList, "%Y-%m-%d")
  bucket <- base + (roundedhrs * 60 * 60)
  return (data.frame(table(bucket)))
}

featuresByTime <- function(ds, min1, min5, hr1, hr3, hr6) {
  ds$min1_mean <- mean(min1)
  ds$min1_max <- max(min1)
  ds$min1_var <- safeVar(min1)
  #ds$min1_skew <- skewness(min1, na.rm = FALSE, type = 1)
  
  ds$min5_mean <- mean(min5)
  ds$min5_max <- max(min5)
  ds$min5_var <- safeVar(min5)
  #ds$min5_skew <- skewness(min5, na.rm = FALSE, type = 1)
  
  ds$hr1_mean <- mean(hr1)
  ds$hr1_max <- max(hr1)
  ds$hr1_var <- safeVar(hr1)
  #ds$hr1_skew <- skewness(hr1, na.rm = FALSE, type = 1)
  
  ds$hr3_mean <- mean(hr3)
  ds$hr3_max <- max(hr3)
  ds$hr3_var <- safeVar(hr3)
  #ds$hr3_skew <- skewness(hr3, na.rm = FALSE, type = 1)
  
  ds$hr6_mean <- mean(hr6)
  ds$hr6_max <- max(hr6)
  ds$hr6_var <- safeVar(hr6)
  #ds$hr6_skew <- skewness(hr6, na.rm = FALSE, type = 1)
  
  return (ds)
}
##################################################################################################

## IP FEATURE.
## Compute the required features for the dataset
## grouping it by the same IP

## add new IP basedfeature columns to the given dataset
## and add computed values to those columns
## note that, the feature value for each of these columns will be
## same for the provided dataset
featuresByIP <- function(ds) {
  ipClickFreq <- data.frame(table(ds$ip))
  
  ds$maxClicksIp <- max(ipClickFreq$Freq)
  ds$noOfIPs <- nrow(ipClickFreq)
  ds$clickToIpRatio <- nrow(ds)/ds$noOfIPs
  ds$clickVarianceIp <- safeVar(ipClickFreq$Freq)
  ds$entropyIp <- shannon.entropy(ipClickFreq$Freq)
  
  return (ds)
}
##################################################################################################

## CHANNEL FEATURE.
featuresByChannel <- function(ds) {
  channelClickFreq <- data.frame(table(ds$channel))
  
  ds$maxClicksChannel <- max(channelClickFreq$Freq)
  ds$noOfChannel <- nrow(channelClickFreq)
  ds$clickToChannelRatio <- nrow(ds)/ds$noOfChannel
  ds$clickVarianceChannel <- safeVar(channelClickFreq$Freq)
  ds$entropyChannel <- shannon.entropy(channelClickFreq$Freq)
  
  return (ds)
}
##################################################################################################

## DEVICE FEATURE.
featuresByDevice <- function(ds) {
  deviceClickFreq <- data.frame(table(ds$device))
  
  ds$maxClicksDevice <- max(deviceClickFreq$Freq)
  ds$noOfDevices <- nrow(deviceClickFreq)
  ds$clickToDeviceRatio <- nrow(ds)/ds$noOfDevices
  ds$clickVarianceDevice <- safeVar(deviceClickFreq$Freq)
  ds$entropyDevice <- shannon.entropy(deviceClickFreq$Freq)
  
  return (ds)
}
##################################################################################################

#### utilities
safeVar <- function (x) {
  return (ifelse(is.na(var(x)),0,var(x)))
}

## compute entropy
shannon.entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}