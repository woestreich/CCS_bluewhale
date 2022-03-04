#### 
# Call Index 
# Calculated by Will Oestreich Comparision between Monterey Bay and Cordell Bank
###

## clear variables and load packages
rm(list = ls())
library(tidyverse)
library(patchwork)
library(RColorBrewer) 
library(zoo)
library(lubridate)
library(directlabels)
library(ggplot2)
library(mosaic)

## load Call Index data 
# Monterey Bay 
monterey_daily <- read.csv("data/acoustics/CI_daily.csv")
monterey_daily$date <- as.Date(monterey_daily$date, "%Y-%m-%d")
monterey_daily$year <- year(monterey_daily$date)
monterey_daily <- monterey_daily %>% filter(monterey_daily$all != "NA" & monterey_daily$year >= 2015 & monterey_daily$year <= 2018)

# Cordell Bank 
cordell_hourly <- read.csv("data/acoustics/CB_CI.csv")
cordell_hourly$Time <- as.POSIXct(cordell_hourly$Time, tz = "America/Los_Angeles", "%m/%d/%Y %H:%M")
cordell_hourly$Date <- date(cordell_hourly$Time) 
cordell_hourly$Hour <- hour(cordell_hourly$Time)
cordell_hourly$year <- year(cordell_hourly$Time)
cordell_hourly$month <- month(cordell_hourly$Time)
cordell_hourly$day <- day(cordell_hourly$Time)
cordell_hourly <- cordell_hourly %>% filter(cordell_hourly$year >= 2015 & cordell_hourly$year <= 2018)

## aggregate Cordell Bank hourly data 
cordell_daily <- data.frame(matrix(nrow = 1310, ncol = 2))
cordell_daily <- aggregate(CI ~ Date, cordell_hourly, mean)


## Monterey Bay Call Index 
monterey_raw <- ggplot(data = monterey_daily, aes(x = date, y = all)) + 
  geom_line() + 
  ylab("Call Index - Daily Resolution") + 
  ggtitle("Monterey Bay Call Index Daily Resolution (unsmoothed)") 

## Cordell Bank Call Index 
cordell_raw <- ggplot(data = cordell_daily, aes(x = Date, y = CI)) + 
  geom_line() + 
  ylab("Call Index - Daily Resolution") + 
  ggtitle("Cordell Bank Call Index Daily Resolution (unsmoothed)") 

monterey_raw/cordell_raw
