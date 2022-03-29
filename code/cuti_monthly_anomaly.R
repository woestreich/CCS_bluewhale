###
# monthly scale cuti anomalies 
# comparison between 37N (Monterey Bay) + 38N (Cordell Bank)
# monthly scale to be less arbitrary 
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
cuti_daily$month <- month(cuti_daily$date)
cuti_daily$year <- year(cuti_daily$date)


## calculate monthly sums for each location 

sums <- data.frame()
for (y in 1988:2021) {
  for (m in 1:12) {
    c <- cuti_daily %>% filter(year == y & month == m) 
    monterey <- sum(c$X37N) 
    cordell <- sum(c$X38N)
    curr <- data.frame(y, m, monterey, cordell)
    sums <- rbind(sums,curr)}
}
colnames(sums) <- c("year", "month", "monterey", "cordell")
sums$date <- as.yearmon(with(sums, paste(year, month, sep = "-")), "%Y-%m") 
sums$month <- month(sums$date)


## calculate monthly averages of sums for anomaly comparison 
monthly_avgs <- data.frame()
for (m in 1:12) { 
  sm <- sums %>% filter (month == m)
  monterey_avg <- mean(sm$monterey)
  cordell_avg <- mean(sm$cordell)
  cu <- data.frame(monterey_avg, cordell_avg)
  monthly_avgs <- rbind(monthly_avgs, cu)}

sums <- cbind(sums, monthly_avgs)

## calculate anomalies 
sums$anomaly_monterey <- (sums$monterey) - (sums$monterey_avg)
sums$anomaly_cordell <- (sums$cordell) - (sums$cordell_avg)

## plot of cuti monthly anomalies 1988 - 2021  
#sums %>%
# pivot_longer(cols = starts_with("anomaly"), names_to = "region", names_prefix = "anomaly_", values_to = "anomaly") %>%
#ggplot(aes(x = date, y = anomaly, fill = region)) + 
# geom_bar(stat = "identity", position = "dodge") + 
#  scale_x_discrete(name = "year", breaks = 34, labels = c("1988:2021"))

## plot of cuti anomalies 2015 - 2018
sums %>% filter(year >= 2015 & year <= 2018) %>%
  pivot_longer(cols = starts_with("anomaly"), names_to = "region", names_prefix = "anomaly_", values_to = "anomaly") %>%
  ggplot(aes(x = date, y = anomaly, fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Cuti upwelling index anomaly") + 
  xlab("Month") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#F8766D", "#00BFC4")) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  ggtitle("Cuti Monthly Anomaly Comparision") 



