## plot #2 of primary productivity comparision

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

windowsize = 10

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

## calculate avg chl for each year 2003 - 2021 
monterey_chla_avgs <- data.frame()
for (y in 2003:2021) { 
  for (m in 1:12) {
    month <- Monterey %>% filter (year == y & month == m)
    average <- mean(month$productivity)
    currAverage <- data.frame(y, m, average)
    monterey_chla_avgs <- rbind(monterey_chla_avgs, currAverage)
  }}
colnames(monterey_chla_avgs) <- c("year", "month", "m_avg")

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative beuti curves 
m_clim <- data.frame(matrix(ncol = 4, nrow = 12))
colnames(m_clim) <- c("month", "avgmean","avg5pctl","avg95pctl")
for (i in 1:12) {
  m <- monterey_chla_avgs %>% filter(i == month) 
  m_clim$month[i] <- i
  m_clim$avgmean[i] <- mean(m$m_avg,na.rm = TRUE)
  m_clim$avg5pctl[i] <- quantile(m$m_avg,.05,na.rm = TRUE)
  m_clim$avg95pctl[i] <- quantile(m$m_avg,.95,na.rm = TRUE)
} 

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

m_2015 <- monterey_avg %>% filter(year == 2015) 
m_2016 <- monterey_avg %>% filter(year == 2016) 
m_2017 <- monterey_avg %>% filter(year == 2017)
m_2018 <- monterey_avg %>% filter(year == 2018) 

### PLOT 
monterey_plot <- ggplot(m_clim, aes(month,avgmean)) +   
  geom_ribbon(aes(ymin=avg5pctl,ymax=avg95pctl),fill = "grey90") +
  #2015 line
  geom_line(data = m_2015, aes(month,avg), color="#b6d3df", size=1) +
  #2016 line
  geom_line(data = m_2016, aes(month,avg), color="#55abcd", size=1) +
  #2017 line
  geom_line(data = m_2017, aes(month,avg), color="#3e7bad", size=1) +
  #2018 line
  geom_line(data = m_2018, aes(month,avg), color="#204d8d", size=1) +
  ylab("Monthly Average Primary Productivity") +
  xlab("Month") +
  theme_classic() +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = c(1:12)) + 
  ggtitle("Monterey Bay Primary Productivity Trends") + 
  geom_segment(aes(x=1, y=58000, xend=2, yend=58000),size=1,color="#b6d3df") + 
  annotate("text", x = 2.5, y = 58000, label = "2015") + 
  geom_segment(aes(x=1, y=56000, xend=2, yend=56000),size=1,color="#55abcd") + 
  annotate("text", x = 2.5, y = 56000, label = "2016") +
  geom_segment(aes(x=1, y=54000, xend=2, yend=54000),size=1,color="#3e7bad") + 
  annotate("text", x = 2.5, y = 54000, label = "2017") +
  geom_segment(aes(x=1, y=52000, xend=2, yend=52000),size=1,color="#204d8d") +
  annotate("text", x = 2.5, y = 52000, label = "2018") +
  geom_rect(aes(xmin=1,xmax=2,ymin=48000,ymax=50000),color="grey90",fill="grey90") + 
  annotate("text", x = 4, y = 49000, label = "Climatological 5th-95th pctl")


## CORDELL BANK ## 

## filter for Cordell Bank bubble and exclude coastline/NULL values
Cordell <- chla_everything %>% filter(chla_everything$latitude >= 37.4 & chla_everything$latitude <= 38.3 & 
                                        chla_everything$productivity != "NaN")

## calculate avg chl for each year 2003 - 2021 
cordell_chla_avgs <- data.frame()
for (y in 2003:2021) { 
  for (m in 1:12) {
    month <- Cordell %>% filter (year == y & month == m)
    average <- mean(month$productivity)
    currAverage <- data.frame(y, m, average)
    cordell_chla_avgs <- rbind(cordell_chla_avgs, currAverage)
  }}
colnames(cordell_chla_avgs) <- c("year", "month", "c_avg")

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative beuti curves 
c_clim <- data.frame(matrix(ncol = 4, nrow = 12))
colnames(c_clim) <- c("month", "avgmean","avg5pctl","avg95pctl")
for (i in 1:12) {
  c <- cordell_chla_avgs %>% filter(i == month) 
  c_clim$month[i] <- i
  c_clim$avgmean[i] <- mean(c$c_avg,na.rm = TRUE)
  c_clim$avg5pctl[i] <- quantile(c$c_avg,.05,na.rm = TRUE)
  c_clim$avg95pctl[i] <- quantile(c$c_avg,.95,na.rm = TRUE)
} 

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

c_2015 <- cordell_avg %>% filter(year == 2015) 
c_2016 <- cordell_avg %>% filter(year == 2016) 
c_2017 <- cordell_avg %>% filter(year == 2017)
c_2018 <- cordell_avg %>% filter(year == 2018) 

### PLOT 
cordell_plot <- ggplot(c_clim, aes(month,avgmean)) +   
  geom_ribbon(aes(ymin=avg5pctl,ymax=avg95pctl),fill = "grey90") +
  #2015 line
  geom_line(data = c_2015, aes(month,avg), color="#ffbd8c", size=1) +
  #2016 line
  geom_line(data = c_2016, aes(month,avg), color="#ffa360", size=1) +
  #2017 line
  geom_line(data = c_2017, aes(month,avg), color="#d86024", size=1) +
  #2018 line
  geom_line(data = c_2018, aes(month,avg), color="#c93900", size=1) +
  ylab("Monthly Average Net Primary Productivity") +
  xlab("Month") +
  theme_classic() +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = c(1:12)) + 
  ggtitle("Cordell Bank Primary Productivity Trends") + 
  geom_segment(aes(x=1, y=58000, xend=2, yend=58000),size=1,color="#ffbd8c") + 
  annotate("text", x = 2.5, y = 58000, label = "2015") + 
  geom_segment(aes(x=1, y=56000, xend=2, yend=56000),size=1,color="#ffa360") + 
  annotate("text", x = 2.5, y = 56000, label = "2016") +
  geom_segment(aes(x=1, y=54000, xend=2, yend=54000),size=1,color="#d86024") + 
  annotate("text", x = 2.5, y = 54000, label = "2017") +
  geom_segment(aes(x=1, y=52000, xend=2, yend=52000),size=1,color="#c93900") +
  annotate("text", x = 2.5, y = 52000, label = "2018") +
  geom_rect(aes(xmin=1,xmax=2,ymin=48000,ymax=50000),color="grey90",fill="grey90") + 
  annotate("text", x = 4, y = 49000, label = "Climatological 5th-95th pctl")

monterey_plot + cordell_plot