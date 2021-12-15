###########
## beuti_summary_plot

## 2015-2020 year comparisions: 
#   2015 = red 
#   2016 = orange 
#   2017 = yellow 
#   2018 = green
#   2019 = blue 
#   2020 = purple 

## four different latitudes: 
#   34N = S. California Blight
#   37N = Monterey Bay
#   40N = Mendocino  
#   43N = S. Oregon 
###########

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

## 34N ##

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 34N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X34N) 
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
## 2015-2020 upwelling accumulation at a 34N 
#2015
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X34N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X34N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X34N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X34N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2019
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X34N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2020
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X34N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## plotA
tiff("beuti_summary_plot.tiff", units="in", width=9, height=4, res=400)
plotA <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkorange", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="forestgreen", size=1) +
  #2019 line
  geom_line(data = byr19, aes(yday,csummean), color="cornflowerblue", size=1) +
  #2020 line
  geom_line(data = byr20, aes(yday,csummean), color="mediumorchid4", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("34 N (2015-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="tomato") + 
  annotate("text", x = 60, y = 3200, label = "2015") + 
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="darkorange") + 
  annotate("text", x = 60, y = 3000, label = "2016") +
  geom_segment(aes(x=0, y=2800, xend=40, yend=2800),size=1,color="darkgoldenrod1") + 
  annotate("text", x = 60, y = 2800, label = "2017") +
  geom_segment(aes(x=0, y=2600, xend=40, yend=2600),size=1,color="forestgreen") +
  annotate("text", x = 60, y = 2600, label = "2018") +
  geom_segment(aes(x=0, y=2400, xend=40, yend=2400),size=1,color="cornflowerblue") + 
  annotate("text", x = 60, y = 2400, label = "2019") +
  geom_segment(aes(x=0, y=2200, xend=40, yend=2200),size=1,color="mediumorchid4") + 
  annotate("text", x = 60, y = 2200, label = "2020") +
  geom_rect(aes(xmin=0,xmax=40,ymin=3400,ymax=3600),color="grey80",fill="grey80") + 
  annotate("text", x = 108, y = 3500, label = "Climatological 5th-95th pctl")


## 37N ##

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 37N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X37N) 
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
## 2015-2020 upwelling accumulation at a 37N 
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

#2019
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X37N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2020
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X37N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## plotB
plotB <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkorange", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="forestgreen", size=1) +
  #2019 line
  geom_line(data = byr19, aes(yday,csummean), color="cornflowerblue", size=1) +
  #2020 line
  geom_line(data = byr20, aes(yday,csummean), color="mediumorchid4", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("37 N (2015-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="tomato") + 
    annotate("text", x = 60, y = 3200, label = "2015") + 
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="darkorange") + 
    annotate("text", x = 60, y = 3000, label = "2016") +
  geom_segment(aes(x=0, y=2800, xend=40, yend=2800),size=1,color="darkgoldenrod1") + 
    annotate("text", x = 60, y = 2800, label = "2017") +
  geom_segment(aes(x=0, y=2600, xend=40, yend=2600),size=1,color="forestgreen") +
    annotate("text", x = 60, y = 2600, label = "2018") +
  geom_segment(aes(x=0, y=2400, xend=40, yend=2400),size=1,color="cornflowerblue") + 
    annotate("text", x = 60, y = 2400, label = "2019") +
  geom_segment(aes(x=0, y=2200, xend=40, yend=2200),size=1,color="mediumorchid4") + 
    annotate("text", x = 60, y = 2200, label = "2020") +
  geom_rect(aes(xmin=0,xmax=40,ymin=3400,ymax=3600),color="grey80",fill="grey80") + 
    annotate("text", x = 108, y = 3500, label = "Climatological 5th-95th pctl")


## 40N ##

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 40N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X40N) 
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

## Generate Plot C
## 2015-2020 upwelling accumulation at a 40N 
#2015
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X40N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X40N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X40N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X40N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2019
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X40N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2020
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X40N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## plotC
plotC <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkorange", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="forestgreen", size=1) +
  #2019 line
  geom_line(data = byr19, aes(yday,csummean), color="cornflowerblue", size=1) +
  #2020 line
  geom_line(data = byr20, aes(yday,csummean), color="mediumorchid4", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("40 N (2015-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=5200, xend=40, yend=5200),size=1,color="tomato") + 
  annotate("text", x = 60, y = 5200, label = "2015") + 
  geom_segment(aes(x=0, y=4800, xend=40, yend=4800),size=1,color="darkorange") + 
  annotate("text", x = 60, y = 4800, label = "2016") +
  geom_segment(aes(x=0, y=4400, xend=40, yend=4400),size=1,color="darkgoldenrod1") + 
  annotate("text", x = 60, y = 4400, label = "2017") +
  geom_segment(aes(x=0, y=4000, xend=40, yend=4000),size=1,color="forestgreen") +
  annotate("text", x = 60, y = 4000, label = "2018") +
  geom_segment(aes(x=0, y=3600, xend=40, yend=3600),size=1,color="cornflowerblue") + 
  annotate("text", x = 60, y = 3600, label = "2019") +
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="mediumorchid4") + 
  annotate("text", x = 60, y = 3200, label = "2020") +
  geom_rect(aes(xmin=0,xmax=40,ymin=5600,ymax=5800),color="grey80",fill="grey80") + 
  annotate("text", x = 108, y = 5700, label = "Climatological 5th-95th pctl")


## 43N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 43N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2020) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X43N) 
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

## Generate Plot D
## 2015-2020 upwelling accumulation at a 43N 
#2015
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X43N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2016
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X43N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2017
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X43N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2018
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X43N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2019
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X43N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)

#2020
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X43N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)

## plotD
plotD <- ggplot(beuti_clim, aes(yday,csummean)) +   
  geom_ribbon(aes(ymin=csum5pctl,ymax=csum95pctl),fill = "grey80") +
  #2015 line
  geom_line(data = byr15, aes(yday,csummean), color="tomato", size=1) +
  #2016 line
  geom_line(data = byr16, aes(yday,csummean), color="darkorange", size=1) +
  #2017 line
  geom_line(data = byr17, aes(yday,csummean), color="darkgoldenrod1", size=1) +
  #2018 line
  geom_line(data = byr18, aes(yday,csummean), color="forestgreen", size=1) +
  #2019 line
  geom_line(data = byr19, aes(yday,csummean), color="cornflowerblue", size=1) +
  #2020 line
  geom_line(data = byr20, aes(yday,csummean), color="mediumorchid4", size=1) +
  ylab("BEUTI cumulative sum\n(mmol/m/s)") +
  xlab("Yearday") +
  theme_classic() +
  theme(legend.position = "none")  +
  ggtitle("43 N (2015-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  geom_segment(aes(x=0, y=3200, xend=40, yend=3200),size=1,color="tomato") + 
  annotate("text", x = 60, y = 3200, label = "2015") + 
  geom_segment(aes(x=0, y=3000, xend=40, yend=3000),size=1,color="darkorange") + 
  annotate("text", x = 60, y = 3000, label = "2016") +
  geom_segment(aes(x=0, y=2800, xend=40, yend=2800),size=1,color="darkgoldenrod1") + 
  annotate("text", x = 60, y = 2800, label = "2017") +
  geom_segment(aes(x=0, y=2600, xend=40, yend=2600),size=1,color="forestgreen") +
  annotate("text", x = 60, y = 2600, label = "2018") +
  geom_segment(aes(x=0, y=2400, xend=40, yend=2400),size=1,color="cornflowerblue") + 
  annotate("text", x = 60, y = 2400, label = "2019") +
  geom_segment(aes(x=0, y=2200, xend=40, yend=2200),size=1,color="mediumorchid4") + 
  annotate("text", x = 60, y = 2200, label = "2020") +
  geom_rect(aes(xmin=0,xmax=40,ymin=3400,ymax=3600),color="grey80",fill="grey80") + 
  annotate("text", x = 108, y = 3500, label = "Climatological 5th-95th pctl")

##print plots
(plotA + plotB) / (plotC + plotD)
dev.off()


