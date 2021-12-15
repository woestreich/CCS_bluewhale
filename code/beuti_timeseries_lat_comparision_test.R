# beuti_timeseries_lat_comparision_test

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


## 31N latitude plot ##
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X31N) #The latitude of analysis is chosen here
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
## Generate 31N Plot
## (Climatological mean upwelling accumulation at 31 N)
thirtyone <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("31 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 32N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X32N) #The latitude of analysis is chosen here
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
## Generate 32N Plot
## (Climatological mean upwelling accumulation at 32 N)
thirtytwo <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("32 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 33N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X33N) #The latitude of analysis is chosen here
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
## Generate 33N Plot
## (Climatological mean upwelling accumulation at 33 N)
thirtythree <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("33 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 34N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X34N) #The latitude of analysis is chosen here
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
## Generate 34N Plot
## (Climatological mean upwelling accumulation at 34 N)
thirtyfour <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("34 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 35N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X35N) #The latitude of analysis is chosen here
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
## Generate 35N Plot
## (Climatological mean upwelling accumulation at 35 N)
thirtyfive <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("35 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 36N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X36N) #The latitude of analysis is chosen here
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
## Generate 36N Plot
## (Climatological mean upwelling accumulation at 36 N)
thirtysix <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("36 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 37N latitude plot (Monterey Bay) ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
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
## Generate 37N Plot
## (Climatological mean upwelling accumulation at 37 N)
thirtyseven <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("37 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 38N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X38N) #The latitude of analysis is chosen here
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
## Generate 38N Plot
## (Climatological mean upwelling accumulation at 38 N)
thirtyeight <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("38 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 39N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X39N) #The latitude of analysis is chosen here
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
## Generate 39N Plot
## (Climatological mean upwelling accumulation at 39 N)
thirtynine <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("39 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 40N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X40N) #The latitude of analysis is chosen here
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
## Generate 40N Plot
## (Climatological mean upwelling accumulation at 40 N)
fourty <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("40 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 41N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X41N) #The latitude of analysis is chosen here
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
## Generate 41N Plot
## (Climatological mean upwelling accumulation at 41 N)
fourtyone <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("41 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 42N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X42N) #The latitude of analysis is chosen here
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
## Generate 42N Plot
## (Climatological mean upwelling accumulation at 42 N)
fourtytwo <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("42 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 43N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X43N) #The latitude of analysis is chosen here
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
## Generate 43N Plot
## (Climatological mean upwelling accumulation at 43 N)
fourtythree <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("43 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 44N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X44N) #The latitude of analysis is chosen here
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
## Generate 44N Plot
## (Climatological mean upwelling accumulation at 44 N)
fourtyfour <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("44 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 45N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X45N) #The latitude of analysis is chosen here
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
## Generate 45N Plot
## (Climatological mean upwelling accumulation at 45 N)
fourtyfive <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("45 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 46N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X46N) #The latitude of analysis is chosen here
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
## Generate 46N Plot
## (Climatological mean upwelling accumulation at 46 N)
fourtysix <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("46 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend


## 47N Latitude Plot ## 
## Calculate cumulative sum of beuti for each year of time series (1988-2020)
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X47N) #The latitude of analysis is chosen here
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
## Generate 47N Plot
## (Climatological mean upwelling accumulation at 47 N)
fourtyseven <- ggplot(beuti_clim, aes(yday,csummean)) +
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey70") +
  geom_line(color="black",size=1,linetype="dashed") +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("47 N") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="black",linetype="dashed") + # part of the legend
  geom_rect(aes(xmin=0,xmax=40,ymin=2700,ymax=2900),color="grey70",fill="grey70") + # part of the legend
  annotate("text", label = paste("Climatological","mean"), x = 112, y = 2990) + # part of the legend
  annotate("text", label = paste("Climatological","5th-95th","pctl"), x = 135, y = 2790) # part of the legend

##arrays of the plots 
group1 <- thirtyone + thirtytwo + thirtythree
group2 <- thirtyfour + thirtyfive + thirtysix 
group3 <- thirtyseven + thirtyeight + thirtynine 
group4 <- fourty + fourtyone + fourtytwo
group5 <- fourtythree + fourtyfour + fourtyfive 
group6 <- fourtysix + fourtyseven 

##print plots, for viewing purposes 
group1 
group2 
group3 
group4
group5
group6

