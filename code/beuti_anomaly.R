###
# beuti_anomaly
# two locations: 37N (Monterey Bay) and 38N (Cordell Bank)
# on seasonal scale in order to be comparable to ssha 
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

## set running mean window size for smoothing of daily BEUTI data
windowsize = 10

## load in daily beuti (from Jacox et al., 2018)
beuti_daily <- read.csv("data/oceanography/beuti_daily_nov2021.csv",header = TRUE) 
beuti_daily$date <- as.Date(with(beuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## split into seasons 
## 1 = Jan 1 - Mar 31
## 2 = Apr 1 - June 30 
## 3 = July 1 - Sep 30 
## 4 = Oct 1 - Dec 31 

beuti_daily <- mutate(beuti_daily, season = derivedFactor(
  "1" = (month == 1 | month == 2 | month == 3), 
  "2" = (month == 4 | month == 5 | month == 6), 
  "3" = (month == 7 | month == 8 | month == 9), 
  "4" = (month == 10 | month == 11 | month == 12), 
  .method = "first", 
  .default = NA 
))

## create new dataframe for anomalies specifically at 37N and 38N 
## 136 rows because it's a 34 year time span, 1988-2021, with four seasons per year 
beuti_anomaly <- data.frame(matrix(ncol = 8, nrow = 136))
colnames(beuti_anomaly) <- c("year", "season", "csum_37", "avg_csum_37", "anomaly_37", "csum_38", "avg_csum_38", "anomaly_38")

## set years 
beuti_anomaly$year <- rep(1988:2021, each = 4)

## set seasons 
beuti_anomaly$season <- rep(c(1, 2, 3, 4), times=34)

## combine year and season columns 
beuti_anomaly$year_season <- paste(beuti_anomaly$year, "-", beuti_anomaly$season)

## calculate cumulative sum for each season 
## 37N ## 
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum_37 <- 0
i1 <- 1
for (y in 1988:2021) { 
  for (s in 1:4) {
    b <- beuti_daily %>% filter(year == y & season == s)
    bcsum <- cumsum(b$X37N) 
    i2 <- i1 + length(bcsum) - 1
    beuti_daily$csum_37[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
    i1 <- i2 + 1
  }
}

## 38N ## 

beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum_38 <- 0
i1 <- 1
for (y in 1988:2021) { 
  for (s in 1:4) {
    b <- beuti_daily %>% filter(year == y & season == s)
    bcsum <- cumsum(b$X38N) 
    i2 <- i1 + length(bcsum) - 1
    beuti_daily$csum_38[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
    i1 <- i2 + 1
  }
}

## pull out each seasonal value  
sum <- beuti_daily %>% filter((season == 1 & month == 3 & day == 31) | 
                                (season == 2 & month == 6 & day == 30) | 
                                (season == 3 & month == 9 & day == 30) |
                                (season == 4 & month == 12 & day == 31) |
                                ## account for the slightly incomplete dataset in 2021 (ends in nov instead of dec)
                                (year == 2021 & season == 4 & month == 11 & day == 30)) 

## transfer over to anomaly data frame 
beuti_anomaly$csum_37 <- sum$csum_37
beuti_anomaly$csum_38 <- sum$csum_38 

## calcuate the average culmulative sums (baselines from which anomalies can be calcuated) 
## 37N ## 
## season 1 
s1 = 1
s1 = sum %>% filter(season == s1)
s1_37_avg <- mean(s1$csum_37)

## season 2
s2 = 2
s2 = sum %>% filter(season == s2)
s2_37_avg <- mean(s2$csum_37)

#season 3
s3 = 3
s3 = sum %>% filter(season == s3)
s3_37_avg <- mean(s3$csum_37)

#season 4
s4 = 4
s4 = sum %>% filter(season == s4)
s4_37_avg <- mean(s4$csum_37)


## 38N ## 
## season 1 
s1 = 1
s1 = sum %>% filter(season == s1)
s1_38_avg <- mean(s1$csum_38)

## season 2
s2 = 2
s2 = sum %>% filter(season == s2)
s2_38_avg <- mean(s2$csum_38)

#season 3
s3 = 3
s3 = sum %>% filter(season == s3)
s3_38_avg <- mean(s3$csum_38)

#season 4
s4 = 4
s4 = sum %>% filter(season == s4)
s4_38_avg <- mean(s4$csum_38)

## transfer over to anomaly data frame 
beuti_anomaly$avg_csum_37 <- rep(c(s1_37_avg, s2_37_avg, s3_37_avg, s4_37_avg), times = 34)
beuti_anomaly$avg_csum_38 <- rep(c(s1_38_avg, s2_38_avg, s3_38_avg, s4_38_avg), times = 34)

## calculate anomaly 
beuti_anomaly$anomaly_37 <- (beuti_anomaly$csum_37) - (beuti_anomaly$avg_csum_37) 
beuti_anomaly$anomaly_38 <- (beuti_anomaly$csum_38) - (beuti_anomaly$avg_csum_38) 


## plot of beuti anomalies 1988 - 2021  
beuti_anomaly %>%
  pivot_longer(cols = starts_with("anomaly"), names_to = "latitude", names_prefix = "anomaly_", values_to = "anomaly") %>%
  ggplot(aes(x = year_season, y = anomaly, fill = latitude)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_discrete(name = "year", breaks = 34, labels = c("1988:2021"))

## plot of beuti anomalies 2015 - 2018
beuti_anomaly %>% filter(year >= 2015 & year <= 2018) %>%
  pivot_longer(cols = starts_with("anomaly"), names_to = "latitude", names_prefix = "anomaly_", values_to = "anomaly") %>%
  ggplot(aes(x = year_season, y = anomaly, fill = as.factor(latitude))) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("BEUTI upwelling index anomaly") + 
  xlab("Season") + 
  scale_fill_manual(name = "Region", labels = c("Monterey Bay", "Cordell Bank"), values = c("#00BFC4", "#F8766D")) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  ggtitle("BEUTI Anomaly Comparision") 


