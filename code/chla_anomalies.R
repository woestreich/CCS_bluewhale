## 
# chla_anomalies 
# entire MODIS-AQUA dataset 01-16-2003 to 01-16-2022 
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
library(stats)

## load in entire chl_a dataset 
## pre-bounded based on the ranges in QGIS 
chla_everything <- read.csv("data/oceanography/chla_everything_rebounded.csv", header = TRUE) 
chla_everything$time <- as.Date(chla_everything$time, "%m/%d/%y")
chla_everything$month <- month(chla_everything$time) 
chla_everything$year <- year(chla_everything$time)
chla_everything$day <- day(chla_everything$time)


## MONTEREY BAY ## 

## filter for Monterey Bay bubble and exclude coastline/NULL values
Monterey <- chla_everything %>% filter(chla_everything$latitude >= 36.2 & chla_everything$latitude <= 37.15 
                                    & chla_everything$productivity != "NaN") 

## calculate overall avgs for Monterey 
monterey_chla_avgs <- data.frame()
for (m in 1:12) {
  month <- Monterey %>% filter (month == m)
  average <- mean(month$productivity)
  sd <- sd(month$productivity)
  currAverage <- data.frame(m, average, sd)
  monterey_chla_avgs <- rbind(monterey_chla_avgs, currAverage)
}
colnames(monterey_chla_avgs) <- c("month", "m_avg", "m_sd")

#calculate monthly avgs for the range of interest (2015 - 2018)
monterey_avg <- data.frame()
for (y in 2015:2018) {
  for (m in 1:12) {
    mont <- Monterey %>% filter(year == y & month == m) 
    avg <- mean(mont$productivity)
    sd <- sd(mont$productivity)
    curr <- data.frame(y, m,"16", avg, sd)
    monterey_avg <- rbind(monterey_avg,curr)}
}
colnames(monterey_avg) <- c("year", "month", "day", "avg", "sd")
monterey_avg$date <- as.Date(with(monterey_avg, paste(year, month, day, sep = "-")), "%Y-%m-%d") 

#calculate anomalies 
monterey_avg$baseline <- rep(monterey_chla_avgs$m_avg, 4)
monterey_avg$anomaly <- (monterey_avg$avg) - (monterey_avg$baseline)


## CORDELL BANK ## 

## filter for Cordell Bank bubble and exclude coastline/NULL values
Cordell <- chla_everything %>% filter(chla_everything$latitude >= 37.4 & chla_everything$latitude <= 38.3 & 
                                     chla_everything$productivity != "NaN")

## calculate overall avgs for Cordell Bank 
cordell_chla_avgs <- data.frame()
for (m in 1:12) {
  month <- Cordell %>% filter (month == m)
  average <- mean(month$productivity)
  sd <- sd(month$productivity)
  currAverage <- data.frame(m, average, sd)
  cordell_chla_avgs <- rbind(cordell_chla_avgs, currAverage)
}
colnames(cordell_chla_avgs) <- c("month", "c_avg", "sd")

#calculate monthly avgs for the range of interest (2015 - 2018)
cordell_avg <- data.frame()
for (y in 2015:2018) {
  for (m in 1:12) {
    cord <- Cordell %>% filter(year == y & month == m) 
    avg <- mean(cord$productivity)
    sd <- sd(cord$productivity)
    curr <- data.frame(y, m,"16", avg, sd)
    cordell_avg <- rbind(cordell_avg,curr)}
}
colnames(cordell_avg) <- c("year", "month", "day", "avg", "sd")
cordell_avg$date <- as.Date(with(cordell_avg, paste(year, month, day, sep = "-")), "%Y-%m-%d") 


#calculate anomalies 
cordell_avg$baseline <- rep(cordell_chla_avgs$c_avg, 4)
cordell_avg$anomaly <- (cordell_avg$avg) - (cordell_avg$baseline)


### PLOTS ###

## combine baseline averages for both regions 
baselines <- cbind(monterey_chla_avgs, cordell_chla_avgs$c_avg, cordell_chla_avgs$sd)
colnames(baselines) <- c("month", "monterey_avg", "m_sd", "cordell_avg", "c_sd")

## graph the averages compared between Cordell Bank + Monterey 
baseline <- ggplot(data = baselines, aes(x = month, y = c(monterey_avg, cordell_avg))) + 
  #line for monterey avg primary productivity 
  geom_line(aes(x = month, y = monterey_avg), color = "#00BFC4") + 
  #line for cordell avg primary productivity 
  geom_line(aes(x = month, y = cordell_avg), color = "#F8766D") + 
  scale_x_continuous(breaks = c(1:12)) +
  xlab("Month") + 
  ylab("Monthly Average Primary Productivity") + 
  theme_classic() + 
  geom_segment(aes(x=1.1, y=16000, xend= 1.5, yend=16000),size=1,color="#00BFC4") + 
  annotate("text", x = 2.1, y = 16000, label = "Monterey Bay") + 
  geom_segment(aes(x=1.1, y=15300, xend=1.5, yend=15300),size=1,color="#F8766D") + 
  annotate("text", x = 2.1, y = 15300, label = "Cordell Bank") + 
  ggtitle("Average Monthly Primary Productivity Comparision Monterey Bay vs. Cordell Bank")  
baseline

#graph Monterey anomalies 
m_anomaly <- ggplot(data = monterey_avg, aes(x = date, y = anomaly)) + 
  geom_col() + 
  ylab("Monthly Primary Productivity Anomaly from Monthly Averages") + 
  ggtitle("Monterey Bay Monthly Primary Productivity Anomalies") 

#graph Cordell anomalies 
c_anomaly <- ggplot(data = cordell_avg, aes(x = date, y = anomaly)) + 
  geom_col() + 
  ylab("Monthly Primary Productivity Anomaly from Monthly Averages") + 
  ggtitle("Cordell Bank Monthly Primary Productivity Anomalies")

#graph anomaly comparison 
anomalies <- data.frame(matrix(nrow = 48, ncol = 3)) 
colnames(anomalies) <- c("date", "anomaly_monterey", "anomaly_cordell")
anomalies$date <- monterey_avg$date
anomalies$anomaly_monterey <- monterey_avg$anomaly
anomalies$anomaly_cordell <- cordell_avg$anomaly

anomalies %>% 
  pivot_longer(cols = starts_with("anomaly"), names_to = "region", names_prefix = "anomaly_", values_to = "anomaly") %>%
  ggplot(aes(x = date, y = anomaly, fill = as.factor(region))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Region", labels = c("Monterey Bay", "Cordell Bank"), values = c("#00BFC4", "#F8766D")) + 
  ylab("Monthly Primary Productivity Anomaly from Monthly Averages") + 
  ggtitle("Monthly Primary Productivity Anomalies") 

