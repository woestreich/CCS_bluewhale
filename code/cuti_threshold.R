###
# CUTI THRESHOLD 
# calculate percentage of days CUTI is above a significant threshold 
# threshold = 0.5 m/s 
# time range = July 1st --> December 31st (months when CUTI is most influential)
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

## filter to only include July - December CUTI data, as that's the time region in which CUTI is most influential for the aggregation of prey
cuti_fallwinter <- cuti_daily %>% filter(month >= 7 & month <= 12)

## create new columns that indicate if the CUTI value is above the 0.5 significance threshold, for both Monterey Bay (37N) and Cordell Bank (38N)
cuti_fallwinter <- cuti_fallwinter %>% mutate(monterey=(X37N>=0.5))
cuti_fallwinter <- cuti_fallwinter %>% mutate(cordell=(X38N>=0.5))

## find percentile above 0.5 threshold for each location for each year 
monterey <- cuti_fallwinter %>% group_by(year) %>% summarise(m_percent = 100*mean(monterey))
cordell <- cuti_fallwinter %>% group_by(year) %>% summarise(c_percent = 100*mean(cordell))

## plot these percentiles
percents <- cbind(monterey, cordell$c_percent)
colnames(percents) <- c('year', 'percent_monterey', 'percent_cordell')
percents %>% filter(year >= 2015 & year <= 2018) %>%
  pivot_longer(cols = starts_with("percent"), names_to = "region", names_prefix = "percent_", values_to = "percent") %>%
  ggplot(aes(x = year, y = percent, fill = as.factor(region))) + 
  geom_bar(stat = "identity", position = "dodge") + xxxxx
  ylab("Percent of Days (July-December) Above CUTI Threshold") + 
  xlab("Year") + 
  scale_fill_manual(name = "Region", labels = c("Cordell Bank", "Monterey Bay"), values = c("#ffa360", "#3e7bad")) +
  ggtitle("Significant CUTI Value (above threshold of 0.5) Percentages") 

cuti_fallwinter$X37N <- rollapply(cuti_fallwinter$X37N,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_fallwinter$X38N <- rollapply(cuti_fallwinter$X38N,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_2015 <- cuti_fallwinter %>% filter(year == 2015)
cuti_2016 <- cuti_fallwinter %>% filter(year == 2016)
cuti_2017 <- cuti_fallwinter %>% filter(year == 2017)
cuti_2018 <- cuti_fallwinter %>% filter(year == 2018)
  
## plot CUTI trend w/ threshold line 
windowsize = 10
# Monterey Bay 
monterey_thres <- ggplot(cuti_fallwinter, aes(x = date, y = X37N)) + 
  geom_line(data = cuti_2015, aes(date, X37N), color = "#3e7bad") + 
  geom_line(data = cuti_2016, aes(date, X37N), color = "#3e7bad") + 
  geom_line(data = cuti_2017, aes(date, X37N), color = "#3e7bad") + 
  geom_line(data = cuti_2018, aes(date, X37N), color = "#3e7bad") + 
  geom_hline(yintercept = 0.5, colour = "black") + 
  scale_y_continuous(breaks = c(-1.0:2.0)) +
  theme_classic() + 
  ylab("CUTI Daily Value") + 
  ggtitle("Monterey Bay CUTI Trends (July-December, 2015-2018)")

#Cordell Bank
cordell_thres <- ggplot(cuti_fallwinter, aes(x = date, y = X388N)) + 
  geom_line(data = cuti_2015, aes(date, X38N), color = "#ffa360") + 
  geom_line(data = cuti_2016, aes(date, X38N), color = "#ffa360") + 
  geom_line(data = cuti_2017, aes(date, X38N), color = "#ffa360") + 
  geom_line(data = cuti_2018, aes(date, X38N), color = "#ffa360") +
  geom_hline(yintercept = 0.5, colour = "black") + 
  scale_y_continuous(breaks = c(-1.0:2.0)) +
  theme_classic() + 
  ylab("CUTI Daily Value") + 
  ggtitle("Cordell Bank CUTI Trends (July-December, 2015-2018)")

monterey_thres / cordell_thres

