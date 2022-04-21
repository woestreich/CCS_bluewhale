#####
# beuti_comparision
# comparision between 37N and 38N (Monterey and Cordell Bank)
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
library(mosaic)

## set running mean window size for smoothing of daily BEUTI data
windowsize = 10

## load in daily beuti (from Jacox et al., 2018)
beuti_daily <- read.csv("data/oceanography/beuti_daily_nov2021.csv",header = TRUE) 
beuti_daily$date <- as.Date(with(beuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## 37N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2021) at 37N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X37N) 
  i2 <- i1 + length(bcsum) - 1
  beuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative beuti curves 
beuti_clim <- data.frame(matrix(ncol = 4, nrow = 366))
colnames(beuti_clim) <- c("yday", "csummean", "csum5pctl","csum95pctl")
for (i in 1:366) {
    b <- beuti_daily %>% filter(yday == i)
    beuti_clim$yday[i] <- i
    beuti_clim$csummean[i] <- mean(b$csum,na.rm = TRUE)
    beuti_clim$csum5pctl[i] <- quantile(b$csum,.05,na.rm = TRUE)
    beuti_clim$csum95pctl[i] <- quantile(b$csum,.95,na.rm = TRUE)
}

## running mean to smooth
beuti_clim$csummean <- rollapply(beuti_clim$csummean,windowsize,mean,fill=NA,na.rm = TRUE)
beuti_clim$csum5pctl <- rollapply(beuti_clim$csum5pctl,windowsize,mean,fill=NA,na.rm = TRUE)
beuti_clim$csum95pctl <- rollapply(beuti_clim$csum95pctl,windowsize,mean,fill=NA,na.rm = TRUE)


## 2015-2018 upwelling accumulation at a 37N 
#2015
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X37N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X37N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X37N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X37N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## Monterey 
Monterey <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey90") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="#b6d3df", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="#55abcd", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="#3e7bad", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="#204d8d", size=1) +
  geom_line(aes(yday, csummean), linetype = "dashed", size = 1, color = "grey40") + 
  ylab("BEUTI cumulative sum (mmol/m/s)") +
  xlab("Month") +
  scale_x_continuous(breaks = c(1,32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("Monterey Bay BEUTI Trends") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + # center/bold the title
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="grey40", linetype = "dashed") + 
  annotate("text", x = 92, y = 3200, label = "Climatological Mean") + 
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="#b6d3df") + 
  annotate("text", x = 60, y = 3000, label = "2015") +
  geom_segment(aes(x=0, y=2800, xend=40, yend=2800),size=1,color="#55abcd") + 
  annotate("text", x = 60, y = 2800, label = "2016") +
  geom_segment(aes(x=0, y=2600, xend=40, yend=2600),size=1,color="#3e7bad") +
  annotate("text", x = 60, y = 2600, label = "2017") +
  geom_segment(aes(x = 0, y = 2400, xend = 40, yend = 2400), size=1, color ='204d8d') + 
  annotate("text", x = 60, y = 2400, label = "2018") + 
  geom_rect(aes(xmin=0,xmax=40,ymin=3400,ymax=3600),color="grey90",fill="grey90") + 
  annotate("text", x = 108, y = 3500, label = "Climatological 5th-95th pctl") 
 


## 38N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2021) at 38N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X38N) 
  i2 <- i1 + length(bcsum) - 1
  beuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean, 5th percentile, and 95th percentile for cumulative beuti curves 
beuti_clim <- data.frame(matrix(ncol = 4, nrow = 366))
colnames(beuti_clim) <- c("yday","csummean","csum5pctl","csum95pctl")
for (i in 1:366) {
  b <- beuti_daily %>% filter(yday == i)
  beuti_clim$yday[i] <- i
  beuti_clim$csummean[i] <- mean(b$csum,na.rm = TRUE)
  beuti_clim$csum5pctl[i] <- quantile(b$csum,.05,na.rm = TRUE)
  beuti_clim$csum95pctl[i] <- quantile(b$csum,.95,na.rm = TRUE)
}

## running mean to smooth
beuti_clim$csummean <- rollapply(beuti_clim$csummean,windowsize,mean,fill=NA,na.rm = TRUE)
beuti_clim$csum5pctl <- rollapply(beuti_clim$csum5pctl,windowsize,mean,fill=NA,na.rm = TRUE)
beuti_clim$csum95pctl <- rollapply(beuti_clim$csum95pctl,windowsize,mean,fill=NA,na.rm = TRUE)

## 2015-2018 upwelling accumulation at a 38N 
#2015
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X38N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X38N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X38N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X38N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## Cordell Bank 
Cordell <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey90") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="#ffbd8c", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="#f89757", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="#d86024", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="#c93900", size=1) +
  geom_line(aes(yday, csummean), linetype = "dashed", size = 1, color = "grey40") + 
  ylab("BEUTI cumulative sum (mmol/m/s)") +
  xlab("Month") +
  scale_x_continuous(breaks = c(1,32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("Cordell Bank BEUTI Trends") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + # center/bold the title
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="grey40", linetype = "dashed") + 
  annotate("text", x = 92, y = 3200, label = "Climatological Mean") + 
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="#ffbd8c") + 
  annotate("text", x = 60, y = 3000, label = "2015") +
  geom_segment(aes(x=0, y=2800, xend=40, yend=2800),size=1,color="#f89757") + 
  annotate("text", x = 60, y = 2800, label = "2016") +
  geom_segment(aes(x=0, y=2600, xend=40, yend=2600),size=1,color="#d86024") +
  annotate("text", x = 60, y = 2600, label = "2017") +
  geom_segment(aes(x = 0, y = 2400, xend = 40, yend = 2400), size=1, color ='#c93900') + 
  annotate("text", x = 60, y = 2400, label = "2018") +
  geom_rect(aes(xmin=0,xmax=40,ymin=3400,ymax=3600),color="grey90",fill="grey90") + 
  annotate("text", x = 108, y = 3500, label = "Climatological 5th-95th pctl")

## print plots 
Monterey + Cordell 