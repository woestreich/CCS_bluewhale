## chl_a analysis 
# from MODIS AQUA satellite 
# bounded in 50km x 50km boxes around hydrophone locations: 
# Monterey Bay: 36.713ºN x 122.186ºW 
# Cordell Bank: 37.8ºN x 123.4ºW 
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
chl_a <- read.csv("data/oceanography/chla_monthly.csv", header = TRUE)
chl_a$time <- as.Date(chl_a$time, "%m/%d/%y")
chl_a$month <- month(chl_a$time) 
chl_a$year <- year(chl_a$time)


## MONTEREY BAY ## 

## filter range for Monterey Bay (50 x 50 km square, excluding coastline/NULL values)
Monterey <- chl_a %>% filter(chl_a$latitude >= 36.263 & chl_a$latitude <= 37.163 & 
                            chl_a$longitude <= -121.626 & chl_a$longitude >= -122.746 &
                            chl_a$productivity != "NaN") 

## compile monthly averages for Monterey  
monterey_avgs <- data.frame()
colnames(monterey_avgs) <- c("month", "year", "avg")

for (y in 2015:2018) {
  for (m in 1:12) { 
    mont <- Monterey %>% filter(year == y & month == m) 
    average <- mean(mont$productivity)
    currAverage <- data.frame(m, y, average)
    monterey_avgs <- rbind(monterey_avgs,currAverage)
  }
}
monterey_avgs$date <- paste(monterey_avgs$y,"-",monterey_avgs$m)
monterey_avgs$date <- as.Date(monterey_avgs$date, "%Y-%b")

## CORDELL BANK ## 

## filter range for Cordell Bank (50 x 50 km range, excluding coastline/NULL values)

Cordell <- chl_a %>% filter(chl_a$latitude >= 37.35 & chl_a$latitude <= 38.25 & 
                            chl_a$longitude <= -122.832 & chl_a$longitude >= -123.968 & 
                            chl_a$productivity != "NaN")

## compile monthly averages for Cordell Bank 
cordell_avgs <- data.frame()
colnames(cordell_avgs) <- c("month", "year", "avg")

for (y in 2015:2018) {
  for (m in 1:12) { 
    cord <- Cordell %>% filter(year == y & month == m) 
    average <- mean(cord$productivity)
    currAverage <- data.frame(m, y, average)
    cordell_avgs <- rbind(cordell_avgs,currAverage)
  }
}
cordell_avgs$date <- paste(cordell_avgs$y, "-", cordell_avgs$m)

## PLOTS ## 

#Monterey Bay 
monterey <- ggplot(data = monterey_avgs, aes(x = date, y = average)) + 
      geom_col() + 
      ylab("Primary Productivity (chl_a)") + 
      ggtitle("Monthly Average Primary Productivity in Monterey Bay") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Cordell Bank 
cordell_bank <- ggplot(data = cordell_avgs, aes(x = date, y = average)) + 
  geom_col() + 
  ylab("Primary Productivity (chl_a)") + 
  ggtitle("Monthly Average Primary Productivity in Cordell Bank") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

monterey + cordell_bank


