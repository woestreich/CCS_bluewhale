## chl_a rebounded 
# used QGIS to bound chl_a region w/ 50 km radius around each hydrophone location: 
# Monterey Bay: 36.713ºN x 122.186ºW 
# Cordell Bank: 37.8ºN x 123.4ºW 
# and upon the shelf break (depth <= 1000 m)
# apply the same avg chl_a methodology with these more specified dataset
## 

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

## load in chl_a dataset 
chla_bounded <- read.csv("data/oceanography/chla_monthly_bounded.csv", header = TRUE)
chla_bounded$time <- as.Date(chla_bounded$time, "%m/%d/%y")
chla_bounded$month <- month(chla_bounded$time) 
chla_bounded$year <- year(chla_bounded$time)
chla_bounded$day <- day(chla_bounded$time)

## MONTEREY BAY ## 

## filter for Monterey Bay bubble and exclude coastline/NULL values
Monterey <- chla_bounded %>% filter(chla_bounded$latitude >= 36.2 & chla_bounded$latitude <= 37.15 
                              & chla_bounded$productivity != "NaN") 

## compile monthly averages for Monterey  
monterey_avgs <- data.frame()

for (y in 2015:2018) {
  for (m in 1:12) {
      mont <- Monterey %>% filter(year == y & month == m) 
      average <- mean(mont$productivity)
      currAverage <- data.frame(y, m,"16", average)
      monterey_avgs <- rbind(monterey_avgs,currAverage)}
    }
colnames(monterey_avgs) <- c("year", "month", "day", "avg")
monterey_avgs$date <- as.Date(with(monterey_avgs, paste(year, month, day, sep = "-")), "%Y-%m-%d") 

## CORDELL BANK ## 

## filter for Cordell Bank bubble and exclude coastline/NULL values
Cordell <- chla_bounded %>% filter(chla_bounded$latitude >= 37.4 & chla_bounded$latitude <= 38.3 & 
                              chla_bounded$productivity != "NaN")

## compile monthly averages for Cordell Bank 
cordell_avgs <- data.frame()

for (y in 2015:2018) {
  for (m in 1:12) { 
    cord <- Cordell %>% filter(year == y & month == m) 
    average <- mean(cord$productivity)
    currAverage <- data.frame(y, m, "16", average)
    cordell_avgs <- rbind(cordell_avgs,currAverage)
  }
}
colnames(cordell_avgs) <- c("year", "month", "day", "avg")
cordell_avgs$date <- as.Date(with(cordell_avgs, paste(year, month, day, sep = "-")), "%Y-%m-%d") 

## PLOTS ## 

# Monterey Bay 
monterey <- ggplot(data = monterey_avgs, aes(x = date, y = avg)) + 
  geom_col() + 
  ylim(NA,28000) + 
  ylab("Primary Productivity (chl_a)") + 
  ggtitle("Monthly Average Primary Productivity in Monterey Bay") 

#Cordell Bank 
cordell_bank <- ggplot(data = cordell_avgs, aes(x = date, y = avg)) + 
  geom_col() + 
  ylim(NA,28000) + 
  ylab("Primary Productivity (chl_a)") + 
  ggtitle("Monthly Average Primary Productivity in Cordell Bank") 

monterey + cordell_bank
