### 
# chla spring avg 
# average comparisions for March - June compared between the two locations of interest 
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
library(stats)

## load in entire chl_a dataset 
## pre-bounded based on the ranges in QGIS 
chla_everything <- read.csv("data/oceanography/chla_everything_rebounded.csv", header = TRUE) 
chla_everything$time <- as.Date(chla_everything$time, "%m/%d/%y")
chla_everything$month <- month(chla_everything$time) 
chla_everything$year <- year(chla_everything$time)
chla_everything$day <- day(chla_everything$time)

## filter for Monterey Bay bubble and exclude coastline/NULL values and specify for spring months (march - june)
Monterey <- chla_everything %>% filter(chla_everything$latitude >= 36.2 & chla_everything$latitude <= 37.15 
                                       & chla_everything$productivity != "NaN" & month >=3 & month <= 6) 
## find monterey average
monterey_avg = data.frame()
  for (y in 2015:2018) {
    mont <- Monterey %>% filter(year == y) 
    m_avg <- mean(mont$productivity)
    sd <- sd(mont$productivity)
    curr <- data.frame(y, m_avg, sd)
    monterey_avg <- rbind(monterey_avg,curr)}
colnames(monterey_avg) <- c("year", "avg_m", "sd")


## filter for Cordell Bank bubble and exclude coastline/NULL values and specify for spring months (march - june)
Cordell <- chla_everything %>% filter(chla_everything$latitude >= 37.4 & chla_everything$latitude <= 38.3 & 
                                        chla_everything$productivity != "NaN" & month >=3 & month <= 6)

## find cordell average
cordell_avg <- data.frame()
for (y in 2015:2018) {
    cord <- Cordell %>% filter(year == y) 
    c_avg <- mean(cord$productivity)
    sd <- sd(cord$productivity)
    curr <- data.frame(y, c_avg, sd)
    cordell_avg <- rbind(cordell_avg,curr)}
colnames(cordell_avg) <- c("year", "avg_c", "sd")
                            
averages <- cbind(monterey_avg, cordell_avg$avg_c)
colnames(averages) <- c("year", "avg_m", "sd_m", "avg_c")
averages %>% pivot_longer(cols = starts_with("avg"), names_to = "region", names_prefix = "avg_", values_to = "averages") %>%
  ggplot(aes(x = year, y = averages, fill = as.factor(region))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#F8766D", "#00BFC4")) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  ggtitle("Average Spring (March - June) Primary Productivity Comparision") + 
  ylab("Average Primary Productivity")
  
