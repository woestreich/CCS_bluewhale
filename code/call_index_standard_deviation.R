## call index standard deviation 

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
monterey_daily$month <- month(monterey_daily$date)
monterey_daily <- monterey_daily %>% filter(monterey_daily$all != "NA" & monterey_daily$date <= '2019-05-31' & monterey_daily$date >= '2015-10-01') 

# Cordell Bank 
cordell_hourly <- read.csv("data/acoustics/CB_CI.csv")
cordell_hourly$Time <- as.POSIXct(cordell_hourly$Time, tz = "America/Los_Angeles", "%m/%d/%Y %H:%M")
cordell_hourly$Date <- date(cordell_hourly$Time) 
cordell_hourly$Hour <- hour(cordell_hourly$Time)
cordell_hourly$year <- year(cordell_hourly$Time)
cordell_hourly$month <- month(cordell_hourly$Time)
cordell_hourly$day <- day(cordell_hourly$Time)

## aggregate Cordell Bank hourly data 
cordell_daily <- data.frame(matrix(nrow = 1310, ncol = 2))
cordell_daily <- aggregate(CI ~ Date, cordell_hourly, mean)
cordell_daily$month <- month(cordell_daily$Date)
cordell_daily$year <- year(cordell_daily$Date)

## create new variable for whale season (rather than year)
monterey_daily <- mutate(monterey_daily, season = derivedFactor(
  "2015/2016" = ((year == 2015 & month >= 10 & month <= 12) | (year == 2016 & month >= 1 & month <= 6)), 
  "2016/2017" = ((year == 2016 & month >= 7 & month <= 12) | (year == 2017 & month >= 1 & month <= 6)),
  "2017/2018" = ((year == 2017 & month >= 7 & month <= 12) | (year == 2018 & month >= 1 & month <= 6)),
  "2018/2019" = ((year == 2018 & month >= 7 & month <= 12) | (year == 2019 & month >= 1 & month <= 6)),
  .method = "first", 
  .default = "NA"
))

cordell_daily <- mutate(cordell_daily, season = derivedFactor(
  "2015/2016" = ((year == 2015 & month >= 10 & month <= 12) | (year == 2016 & month >= 1 & month <= 6)), 
  "2016/2017" = ((year == 2016 & month >= 7 & month <= 12) | (year == 2017 & month >= 1 & month <= 6)),
  "2017/2018" = ((year == 2017 & month >= 7 & month <= 12) | (year == 2018 & month >= 1 & month <= 6)),
  "2018/2019" = ((year == 2018 & month >= 7 & month <= 12) | (year == 2019 & month >= 1 & month <= 6)),
  .method = "first", 
  .default = "NA"
))

## set whale variable = TRUE if above CI threshold of 1.01 
monterey_daily <- monterey_daily %>% mutate(whale = (all >= 1.01))
cordell_daily <- cordell_daily %>% mutate(whale = (CI >= 1.01))

## find coefficient of variance based on standard deviation/mean 
monterey <- monterey_daily %>% filter(monterey_daily$whale == TRUE) %>% group_by(season) %>% summarise(m_sd = sd(all, na.rm = TRUE), m_mean = mean(all, na.rm = TRUE))
monterey$cv <- (monterey$m_sd) / (monterey$m_mean) * 100 
cordell <- cordell_daily %>% filter(cordell_daily$whale == TRUE) %>% group_by(season) %>% summarise(c_sd = sd(CI, na.rm = TRUE), c_mean = mean(CI, na.rm = TRUE))
cordell$cv <- (cordell$c_sd) / (cordell$c_mean) * 100 

## PLOT 
sd <- data.frame(cbind(monterey$season, monterey$m_sd, cordell$c_sd)) 
colnames(sd) <- c('season', 'sd_monterey', 'sd_cordell')
sd %>%
  pivot_longer(cols = starts_with("sd"), names_to = "region", names_prefix = "sd_", values_to = "sd") %>%
  ggplot(aes(x = season, y = sd, fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_classic() + 
  ylab("Standard Deviation") + 
  xlab("Season") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) +
  ggtitle("Standard Deviation of Call Index Values Within Each Song Season") 

cv <- data.frame(cbind(monterey$season, monterey$cv, cordell$cv)) 
colnames(cv) <- c('season', 'cv_monterey', 'cv_cordell')
cv %>%
  pivot_longer(cols = starts_with("cv"), names_to = "region", names_prefix = "cv_", values_to = "cv") %>%
  ggplot(aes(x = season, y = cv, fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_classic() + 
  ylab("Coefficient of Variance") + 
  xlab("Season") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) +
  ggtitle("Coefficient of Variance of Call Index Values Within Each Song Season") 

