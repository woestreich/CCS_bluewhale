#####
# cuti_comparision
# comparison between 37N and 38N (Monterey and Cordell Bank)
# between year range 2015 - 2018 
#####

## clear variables and load packages
rm(list = ls())
library(tidyverse)
library(patchwork)
library(RColorBrewer) 
library(zoo)
library(lubridate)
library(directlabels)
library(ggplot2)

## set running mean window size for smoothing of daily CUTI data
windowsize = 10

## load in daily cuti (from Jacox et al., 2018)
cuti_daily <- read.csv("data/oceanography/cuti_daily_nov2021.csv",header = TRUE) 
cuti_daily$date <- as.Date(with(cuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## 37N ## 

## Calculate cumulative sum of cuti for each year of time series (1988-2021) at 37N
cuti_daily$yday <- yday(cuti_daily$date)
cuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- cuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X37N) 
  i2 <- i1 + length(bcsum) - 1
  cuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of cuti for each day/year in the original cuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative cuti curves 
cuti_clim <- data.frame(matrix(ncol = 4, nrow = 366))
colnames(cuti_clim) <- c("yday","csummean","csum5pctl","csum95pctl")
for (i in 1:366) {
  b <- cuti_daily %>% filter(yday == i)
  cuti_clim$yday[i] <- i
  cuti_clim$csummean[i] <- mean(b$csum,na.rm = TRUE)
  cuti_clim$csum5pctl[i] <- quantile(b$csum,.05,na.rm = TRUE)
  cuti_clim$csum95pctl[i] <- quantile(b$csum,.95,na.rm = TRUE)
}

## running mean to smooth
cuti_clim$csummean <- rollapply(cuti_clim$csummean,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum5pctl <- rollapply(cuti_clim$csum5pctl,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum95pctl <- rollapply(cuti_clim$csum95pctl,windowsize,mean,fill=NA,na.rm = TRUE)

## 2015-2018 upwelling accumulation at a 37N 
#2015
yr15 = 2015
byr15 = cuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X37N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = cuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X37N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = cuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X37N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = cuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X37N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## Monterey 
Monterey <- ggplot(cuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="forestgreen", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="cornflowerblue", size=1) +
  ylab("CUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("Monterey Bay CUTI Trends (2015-2018)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=250, xend=40, yend=250),size=1,color="tomato") + 
  annotate("text", x = 60, y = 250, label = "2015") + 
  geom_segment(aes(x=0, y=240, xend=40, yend=240),size=1,color="darkgoldenrod1") + 
  annotate("text", x = 60, y = 240, label = "2016") +
  geom_segment(aes(x=0, y=230, xend=40, yend=230),size=1,color="forestgreen") + 
  annotate("text", x = 60, y = 230, label = "2017") +
  geom_segment(aes(x=0, y=220, xend=40, yend=220),size=1,color="cornflowerblue") +
  annotate("text", x = 60, y = 220, label = "2018") +
  geom_rect(aes(xmin=0,xmax=40,ymin=200,ymax=210),color="grey80",fill="grey80") + 
  annotate("text", x = 108, y = 205, label = "Climatological 5th-95th pctl")


## 38N ## 

## Calculate cumulative sum of cuti for each year of time series (1988-2021) at 38N
cuti_daily$yday <- yday(cuti_daily$date)
cuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- cuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X38N) 
  i2 <- i1 + length(bcsum) - 1
  cuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of cuti for each day/year in the original cuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative cuti curves 
cuti_clim <- data.frame(matrix(ncol = 4, nrow = 366))
colnames(cuti_clim) <- c("yday","csummean","csum5pctl","csum95pctl")
for (i in 1:366) {
  b <- cuti_daily %>% filter(yday == i)
  cuti_clim$yday[i] <- i
  cuti_clim$csummean[i] <- mean(b$csum,na.rm = TRUE)
  cuti_clim$csum5pctl[i] <- quantile(b$csum,.05,na.rm = TRUE)
  cuti_clim$csum95pctl[i] <- quantile(b$csum,.95,na.rm = TRUE)
}

## running mean to smooth
cuti_clim$csummean <- rollapply(cuti_clim$csummean,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum5pctl <- rollapply(cuti_clim$csum5pctl,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum95pctl <- rollapply(cuti_clim$csum95pctl,windowsize,mean,fill=NA,na.rm = TRUE)

## 2015-2018 upwelling accumulation at a 38N 
#2015
yr15 = 2015
byr15 = cuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X38N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = cuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X38N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = cuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X38N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = cuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X38N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## Cordell Bank 
Cordell <- ggplot(cuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="forestgreen", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="cornflowerblue", size=1) +
  ylab("CUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("Cordell Bank CUTI Trends (2015-2018)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=250, xend=40, yend=250),size=1,color="tomato") + 
  annotate("text", x = 60, y = 250, label = "2015") + 
  geom_segment(aes(x=0, y=240, xend=40, yend=240),size=1,color="darkgoldenrod1") + 
  annotate("text", x = 60, y = 240, label = "2016") +
  geom_segment(aes(x=0, y=230, xend=40, yend=230),size=1,color="forestgreen") + 
  annotate("text", x = 60, y = 230, label = "2017") +
  geom_segment(aes(x=0, y=220, xend=40, yend=220),size=1,color="cornflowerblue") +
  annotate("text", x = 60, y = 220, label = "2018") +
  geom_rect(aes(xmin=0,xmax=40,ymin=200,ymax=210),color="grey80",fill="grey80") + 
  annotate("text", x = 108, y = 205, label = "Climatological 5th-95th pctl")

## print plots 
Monterey + Cordell 