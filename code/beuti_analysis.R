######################################## 
# beuti_analysis.R
#
# "Sandbox" for exploring biologically-effective upwelling data (BEUTI, Jacox et al., 2018).
# Primary focus on calculating and plotting cumulative upwelling curves for latitudes across the CA Current System.
# 
# You can also calculate annual upwelling phenology metrics for each latitude
# (not currently included here but see https://github.com/woestreich/blue-whale-phenology/blob/main/scripts/cumulative_beuti.R)
#
# Will Oestreich and Natalie Cross
########################################

## clear variables and load packages
rm(list = ls())
library(tidyverse)
library(patchwork)
library(RColorBrewer) 
library(zoo)
library(lubridate)
library(directlabels)

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

## Generate Plot A 
## (Climatological mean upwelling accumulation at 37 N)
pa <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ylim(-150,3300) +
  ggtitle("37 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend

## Display Plot A
pa

## Generate Plot B
## Single-year upwelling accumulation at a given latitude (37 N to start)
# Select year
yr = 2015
byr = beuti_daily %>% filter(year == yr)

# Calculate cumulative sum at a given latitude
byr$csum <- cumsum(b$X37N) #The latitude of analysis is chosen here

# Apply running mean to cumulative sum of BEUTI
byr$csummean <- rollapply(byr$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# plot
pb <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  geom_line(data = byr, aes(yday,csummean), color="black", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ylim(-150,3300) +
  ggtitle(paste("37 N,", yr)) +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3180, xend=40, yend=3180),size=1,color="black") + # part of the legend
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Current","year","(",yr,")"), x = 113, y = 3170) + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend

## Display Plot B
pb
