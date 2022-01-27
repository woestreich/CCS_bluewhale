## cuti_anomaly
## same code as cuti_anomaly analysis
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

## set running mean window size for smoothing of daily cuti data
windowsize = 10

## load in daily cuti (from Jacox et al., 2018)
cuti_daily <- read.csv("data/oceanography/cuti_daily_nov2021.csv",header = TRUE) 
cuti_daily$date <- as.Date(with(cuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## split into seasons 
## 1 = Jan 1 - Mar 31
## 2 = Apr 1 - June 30 
## 3 = July 1 - Sep 30 
## 4 = Oct 1 - Dec 31 

cuti_daily <- mutate(cuti_daily, season = derivedFactor(
  "1" = (month == 1 | month == 2 | month == 3), 
  "2" = (month == 4 | month == 5 | month == 6), 
  "3" = (month == 7 | month == 8 | month == 9), 
  "4" = (month == 10 | month == 11 | month == 12), 
  .method = "first", 
  .default = NA 
))

## create new dataframe for anomalies specifically at 37N and 38N 
## 136 rows becuase it's a 34 year time span, 1988-2021, with four seasons per year 
cuti_anomaly <- data.frame(matrix(ncol = 8, nrow = 136))
colnames(cuti_anomaly) <- c("year", "season", "csum_37", "avg_csum_37", "anomaly_37", "csum_38", "avg_csum_38", "anomaly_38")

## set years 
cuti_anomaly$year <- rep(1988:2021, each = 4)

## set seasons 
cuti_anomaly$season <- rep(c(1, 2, 3, 4), times=34)

## calculate cumulative sum for each season 
## 37N ## 
cuti_daily$yday <- yday(cuti_daily$date)
cuti_daily$csum_37 <- 0
i1 <- 1
for (y in 1988:2021) { 
  for (s in 1:4) {
    b <- cuti_daily %>% filter(year == y & season == s)
    bcsum <- cumsum(b$X37N) 
    i2 <- i1 + length(bcsum) - 1
    cuti_daily$csum_37[i1:i2] <- bcsum #this line stores the cumulative sume of cuti for each day/year in the original cuti_daily data frame
    i1 <- i2 + 1
  }
}

## 38N ## 

cuti_daily$yday <- yday(cuti_daily$date)
cuti_daily$csum_38 <- 0
i1 <- 1
for (y in 1988:2021) { 
  for (s in 1:4) {
    b <- cuti_daily %>% filter(year == y & season == s)
    bcsum <- cumsum(b$X38N) 
    i2 <- i1 + length(bcsum) - 1
    cuti_daily$csum_38[i1:i2] <- bcsum #this line stores the cumulative sume of cuti for each day/year in the original cuti_daily data frame
    i1 <- i2 + 1
  }
}

## pull out each seasonal value  
sum <- cuti_daily %>% filter((season == 1 & month == 3 & day == 31) | 
                                (season == 2 & month == 6 & day == 30) | 
                                (season == 3 & month == 9 & day == 30) |
                                (season == 4 & month == 12 & day == 31) |
                                ## account for the slightly incomplete dataset in 2021 (ends in nov instead of dec)
                                (year == 2021 & season == 4 & month == 11 & day == 30)) 

## transfer over to anomaly data frame 
cuti_anomaly$csum_37 <- sum$csum_37
cuti_anomaly$csum_38 <- sum$csum_38 

## calcuate the average culmulative sums (baselines from which anomalies can be calcuated) 
## 37N ## 
## season 1 
s1 = 1
s1 = sum %>% filter(season == s1)
s1$csum <- cumsum(s1$csum_37) 
s1_37_avg <- (s1$csum[34])/34 

## season 2
s2 = 2
s2 = sum %>% filter(season == s2)
s2$csum <- cumsum(s2$csum_37) 
s2_37_avg <- (s2$csum[34])/34 

#season 3
s3 = 3
s3 = sum %>% filter(season == s3)
s3$csum <- cumsum(s3$csum_37) 
s3_37_avg <- (s3$csum[34])/34 

#season 4
s4 = 4
s4 = sum %>% filter(season == s4)
s4$csum <- cumsum(s4$csum_37) 
s4_37_avg <- (s4$csum[34])/34 

## 38N ## 
## season 1 
s1 = 1
s1 = sum %>% filter(season == s1)
s1$csumm <- cumsum(s1$csum_38) 
s1_38_avg <- (s1$csumm[34])/34 

## season 2
s2 = 2
s2 = sum %>% filter(season == s2)
s2$csumm <- cumsum(s2$csum_38) 
s2_38_avg <- (s2$csumm[34])/34 

#season 3
s3 = 3
s3 = sum %>% filter(season == s3)
s3$csumm <- cumsum(s3$csum_38) 
s3_38_avg <- (s3$csumm[34])/34 

#season 4
s4 = 4
s4 = sum %>% filter(season == s4)
s4$csumm <- cumsum(s4$csum_38) 
s4_38_avg <- (s4$csumm[34])/34 

## transfer over to anomaly data frame 
cuti_anomaly$avg_csum_37 <- rep(c(s1_37_avg, s2_37_avg, s3_37_avg, s4_37_avg), times = 34)
cuti_anomaly$avg_csum_38 <- rep(c(s1_38_avg, s2_38_avg, s3_38_avg, s4_38_avg), times = 34)

## calculate anomaly 
cuti_anomaly$anomaly_37 <- (cuti_anomaly$csum_37) - (cuti_anomaly$avg_csum_37) 
cuti_anomaly$anomaly_38 <- (cuti_anomaly$csum_38) - (cuti_anomaly$avg_csum_38) 




