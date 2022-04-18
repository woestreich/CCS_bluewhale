###
# callindex_smoothed 
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

## find threshold (max value in spring - Mar:May) 
## actually set threshold to 1.01 (rounded up, also used by Will/John)
#m_spring <- monterey_daily %>% filter(month >= 3 & month <= 5 & all != 'NA')
#m_thres <- max(m_spring$all)

#c_spring <- cordell_daily %>% filter(month >= 3 & month <= 5 & CI != 'NA')
#c_thres <- max(c_spring$CI)

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

## quantify # of days above the thresholds 
monterey_daily <- monterey_daily %>% mutate(whale = (all >= 1.01))
cordell_daily <- cordell_daily %>% mutate(whale = (CI >= 1.01))

monterey <- monterey_daily %>% group_by(season) %>% summarise(m_sum = sum(whale, na.rm = TRUE))
cordell <- cordell_daily %>% group_by(season) %>% summarise(c_sum = sum(whale, na.rm = TRUE))

## plot comparison the # of days above the thresholds 
whales_present <- cbind(monterey, cordell$c_sum)
colnames(whales_present) <- c('season', 'sum_monterey', 'sum_cordell')
whales_present %>%
  slice(-c(1)) %>%
  pivot_longer(cols = starts_with("sum"), names_to = "region", names_prefix = "sum_", values_to = "sum") %>%
  ggplot(aes(x = season, y = sum, fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_classic() + 
  ylab("Number of Days Above Call Index Threshold") + 
  xlab("Season") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) +
  ggtitle("Song Season Duration as Measured by Blue Whale Acoustic Presence") 

## find standard deviation above the threshold

monterey_sd <- monterey_daily %>% filter(monterey_daily$whale == TRUE) %>% group_by(season) %>% summarise(m_sd = sd(all, na.rm = TRUE))
cordell_sd <- cordell_daily %>% filter(cordell_daily$whale == TRUE) %>% group_by(season) %>% summarise(c_sd = sd(CI, na.rm = TRUE))

sd <- cbind(monterey_sd, cordell_sd$c_sd)
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



## smooth data @ 5 day resolution 
windowsize = 5
monterey_daily$all <- rollapply(monterey_daily$all,windowsize,mean,fill=NA,na.rm = TRUE)
cordell_daily$CI <- rollapply(cordell_daily$CI, windowsize, mean, fill = NA, na.rm = TRUE)

## find the peak for each year/location 
monterey_clean <- monterey_daily %>% filter(monterey_daily$all != 'NA') 
m_peak <- monterey_clean %>% group_by(season) %>% slice(which.max(all))

cordell_clean <- cordell_daily %>% filter(cordell_daily$CI != 'NA') 
c_peak <- cordell_clean %>% group_by(season) %>% slice(which.max(CI))

## PLOT Smoothed Data 

## Monterey Bay Call Index 
monterey_smoothed <- ggplot(data = monterey_daily, aes(x = date, y = all)) + 
  geom_line(color = "#3e7bad") + 
  geom_hline(yintercept = 1.01, color = "black") + 
  ## highlight peak days 
  geom_point(data = m_peak, aes(x = date, y = all), color = "black") + 
  geom_text(data = m_peak, aes(label = format(date, "%b %d,%Y")), nudge_y = 0.015) + 
  theme_classic() + 
  scale_x_date(limits = as.Date(c("2015-10-01", "2019-05-31"))) + 
  ylab("Call Index") + 
  xlab("Year") + 
  ggtitle("Monterey Bay Call Index (Smoothed at 5-day Resolution)") 

## Cordell Bank Call Index 
cordell_smoothed <- ggplot(data = cordell_daily, aes(x = Date, y = CI)) + 
  geom_line(color = "#ffa360") + 
  geom_hline(yintercept = 1.01, colour = "black") + 
  ## highlight peak days 
  geom_point(data = c_peak, aes(x = Date, y = CI), colour = "black") + 
  geom_text(data = c_peak, aes(label = format(Date, "%b %d,%Y")), nudge_y = 0.015) + 
  scale_x_date(limits = as.Date(c("2015-10-01", "2019-05-31"))) + 
  theme_classic() + 
  ylab("Call Index") + 
  xlab("Year") + 
  ggtitle("Cordell Bank Call Index (Smoothed at 5-day Resolution)") 

monterey_smoothed/cordell_smoothed


##fancy colors 
## Monterey Bay Call Index 
#monterey_smoothed <- ggplot(data = monterey_daily, aes(x = date, y = all, color = season)) + 
 # geom_line() + 
#  scale_color_manual(values = c("#ffbd8c", "#ffa360", "#d86024", "#c93900")) + 
 # geom_hline(yintercept = 1.01, color = "black") + 
  ## highlight peak days 
  #geom_point(data = m_peak, aes(x = date, y = all), color = "black") + 
  #geom_text(data = c_peak, aes(label = format(Date, "%b %d,%Y")), nudge_y = 0.015) + 
  #scale_x_date(limits = as.Date(c("2015-10-01", "2019-05-31"))) + 
  #theme_classic() + 
  #xlab("Year") + 
  #ylab("Call Index - Daily Resolution") + 
  #ggtitle("Monterey Bay Call Index (Smoothed at 5-day Resolution)") 

## Cordell Bank Call Index 
#cordell_smoothed <- ggplot(data = cordell_daily, aes(x = Date, y = CI, color = season)) + 
 # geom_line() + 
  #geom_hline(yintercept = 1.01, colour = "black") + 
  #scale_color_manual(values = c("#b6d3df", "#55abcd", "#3e7bad", "#204d8d")) + 
  ## highlight peak days 
  #geom_point(data = c_peak, aes(x = Date, y = CI), colour = "black") + 
  #geom_text(data = c_peak, aes(label = format(Date, "%b %d,%Y")), nudge_y = 0.015) + 
  #scale_x_date(limits = as.Date(c("2015-10-01", "2019-05-31"))) + 
  #theme_classic() + 
  #xlab("Year") + 
  #ylab("Call Index") + 
  #ggtitle("Cordell Bank Call Index (Smoothed at 5-day Resolution)") 

#monterey_smoothed/cordell_smoothed 

  