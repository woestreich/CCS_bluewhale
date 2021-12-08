# beuti_37N_year_comparision_test

## clear variables and load packages
rm(list = ls())
library(tidyverse)
library(patchwork)
library(RColorBrewer) 
library(zoo)
library(lubridate)
library(directlabels)
library(ggplot2)

## set running mean window size for smoothing of daily BEUTI data
windowsize = 10

## load in daily beuti (from Jacox et al., 2018)
beuti_daily <- read.csv("data/oceanography/BEUTI_daily.csv",header = TRUE) 
beuti_daily$date <- as.Date(with(beuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## Calculate cumulative sum of beuti for each year of time series (1988-2020)
## Originally calculating for 37N (Monterey Bay), but can be altered
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X37N) #The latitude of analysis is chosen here
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

## Generate Plot B
## Single-year upwelling accumulation at a given latitude (37 N to start)
# Select year
yr = 2015
byr = beuti_daily %>% filter(year == yr)

# Calculate cumulative sum at a given latitude
byr$csum <- cumsum(b$X37N) #The latitude of analysis is chosen here

# Apply running mean to cumulative sum of BEUTI
byr$csummean <- rollapply(byr$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X37N) #The latitude of analysis is chosen here
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X37N) #The latitude of analysis is chosen here
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# plot
year_comparision <- ggplot(beuti_clim, aes(yday,csummean)) +
  #2015 line
  geom_line(data = byr, aes(yday,csummean), color="black", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="blue", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="red", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ylim(-150,3300) +
  ggtitle(paste("37 N,", yr, yr16, yr17)) +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3180, xend=40, yend=3180),size=1,color="black") + # part of the legend
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="blue") + # part of the legend
  geom_segment(aes(x=0, y=2790, xend=40, yend=2790),size=1,color="red") + # part of the legend
  annotate("text", label = paste("Current","year","(",yr,")"), x = 113, y = 3170) + # part of the legend
  annotate("text", label = paste("Current","year","(",yr16,")"), x = 113, y = 2990) + # part of the legend
  annotate("text", label = paste("Current","year","(",yr17,")"), x = 113, y = 2790) # part of the legend

## Display Plot B
year_comparision

