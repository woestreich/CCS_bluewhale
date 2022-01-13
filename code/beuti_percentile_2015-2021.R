###
## beuti_percentile_comparision
## including 2021 (total range: 2015-2021)
# comparing between 4 latitude: 
# 34N - S. CA Blight 
# 37N - Monterey Bay 
# 40N - Mendocino 
# 43N - S. Oregon 

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

## set running mean window size for smoothing of daily BEUTI data
windowsize = 10

## load in daily beuti (from Jacox et al., 2018)
beuti_daily <- read.csv("data/oceanography/beuti_daily_nov2021.csv",header = TRUE) 
beuti_daily$date <- as.Date(with(beuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")


## 34N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 34N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X34N) 
  i2 <- i1 + length(bcsum) - 1
  beuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean for culmulative beuti curves at 34N
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


## 2015 ##
#upwelling accumulation 
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X34N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean15 <- mean(byr15$csummean, na.rm = TRUE)

# percentile
percentile_15_34 <- ecdf(beuti_clim$csummean)(mean15)


## 2016 ##
#upwelling accumulation 
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X34N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean16 <- mean(byr16$csummean, na.rm = TRUE)

# percentile
percentile_16_34 <- ecdf(beuti_clim$csummean)(mean16)


## 2017 ##
#upwelling accumulation 
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X34N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean17 <- mean(byr17$csummean, na.rm = TRUE)

# percentile
percentile_17_34 <- ecdf(beuti_clim$csummean)(mean17)


## 2018 ##
#upwelling accumulation 
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X34N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean18 <- mean(byr18$csummean, na.rm = TRUE)

# percentile
percentile_18_34 <- ecdf(beuti_clim$csummean)(mean18)


## 2019 ##
#upwelling accumulation 
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X34N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean19 <- mean(byr19$csummean, na.rm = TRUE)

# percentile
percentile_19_34 <- ecdf(beuti_clim$csummean)(mean19)


## 2020 ##
#upwelling accumulation 
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X34N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)

# average upwelling 
mean20 <- mean(byr20$csummean, na.rm = TRUE)

# percentile
percentile_20_34 <- ecdf(beuti_clim$csummean)(mean20)


## 2021 ##
#upwelling accumulation 
yr21 = 2021
byr21 = beuti_daily %>% filter(year == yr21)
byr21$csum <- cumsum(b$X34N) 
byr21$csummean <- rollapply(byr21$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean21 <- mean(byr21$csummean, na.rm = TRUE)
# percentile
percentile_21_34 <- ecdf(beuti_clim$csummean)(mean21)


## 37N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 37N
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

## calculate long-term climatological mean for culmulative beuti curves at 37N
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


## 2015 ##
#upwelling accumulation 
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X37N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean15 <- mean(byr15$csummean, na.rm = TRUE)
# percentile
percentile_15_37 <- ecdf(beuti_clim$csummean)(mean15)


## 2016 ##
#upwelling accumulation 
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X37N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean16 <- mean(byr16$csummean, na.rm = TRUE)
# percentile
percentile_16_37 <- ecdf(beuti_clim$csummean)(mean16)


## 2017 ##
#upwelling accumulation 
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X37N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean17 <- mean(byr17$csummean, na.rm = TRUE)
# percentile
percentile_17_37 <- ecdf(beuti_clim$csummean)(mean17)


## 2018 ##
#upwelling accumulation 
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X37N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean18 <- mean(byr18$csummean, na.rm = TRUE)
# percentile
percentile_18_37 <- ecdf(beuti_clim$csummean)(mean18)


## 2019 ##
#upwelling accumulation 
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X37N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean19 <- mean(byr19$csummean, na.rm = TRUE)
# percentile
percentile_19_37 <- ecdf(beuti_clim$csummean)(mean19)


## 2020 ##
#upwelling accumulation 
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X37N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean20 <- mean(byr20$csummean, na.rm = TRUE)
# percentile
percentile_20_37 <- ecdf(beuti_clim$csummean)(mean20)

## 2021 ##
#upwelling accumulation 
yr21 = 2021
byr21 = beuti_daily %>% filter(year == yr21)
byr21$csum <- cumsum(b$X37N) 
byr21$csummean <- rollapply(byr21$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean21 <- mean(byr21$csummean, na.rm = TRUE)
# percentile
percentile_21_37 <- ecdf(beuti_clim$csummean)(mean21)


## 40N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 40N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X40N) 
  i2 <- i1 + length(bcsum) - 1
  beuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean for culmulative beuti curves at 40N
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


## 2015 ##
#upwelling accumulation 
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X40N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean15 <- mean(byr15$csummean, na.rm = TRUE)
# percentile
percentile_15_40 <- ecdf(beuti_clim$csummean)(mean15)


## 2016 ##
#upwelling accumulation 
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X40N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean16 <- mean(byr16$csummean, na.rm = TRUE)
# percentile
percentile_16_40 <- ecdf(beuti_clim$csummean)(mean16)


## 2017 ##
#upwelling accumulation 
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X40N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean17 <- mean(byr17$csummean, na.rm = TRUE)
# percentile
percentile_17_40 <- ecdf(beuti_clim$csummean)(mean17)


## 2018 ##
#upwelling accumulation 
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X40N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean18 <- mean(byr18$csummean, na.rm = TRUE)
# percentile
percentile_18_40 <- ecdf(beuti_clim$csummean)(mean18)


## 2019 ##
#upwelling accumulation 
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X40N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean19 <- mean(byr19$csummean, na.rm = TRUE)
# percentile
percentile_19_40 <- ecdf(beuti_clim$csummean)(mean19)


## 2020 ##
#upwelling accumulation 
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X40N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean20 <- mean(byr20$csummean, na.rm = TRUE)
# percentile
percentile_20_40 <- ecdf(beuti_clim$csummean)(mean20)

## 2021 ##
#upwelling accumulation 
yr21 = 2021
byr21 = beuti_daily %>% filter(year == yr21)
byr21$csum <- cumsum(b$X40N) 
byr21$csummean <- rollapply(byr21$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean21 <- mean(byr21$csummean, na.rm = TRUE)
# percentile
percentile_21_40 <- ecdf(beuti_clim$csummean)(mean21)


## 43N ## 

## Calculate cumulative sum of beuti for each year of time series (1988-2020) at 43N
beuti_daily$yday <- yday(beuti_daily$date)
beuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2021) {
  b <- beuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X43N) 
  i2 <- i1 + length(bcsum) - 1
  beuti_daily$csum[i1:i2] <- bcsum #this line stores the cumulative sume of beuti for each day/year in the original beuti_daily data frame
  i1 <- i2 + 1
}

## calculate long-term climatological mean for culmulative beuti curves at 43N
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


## 2015 ##
#upwelling accumulation 
yr15 = 2015
byr15 = beuti_daily %>% filter(year == yr15)
byr15$csum <- cumsum(b$X43N) 
byr15$csummean <- rollapply(byr15$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean15 <- mean(byr15$csummean, na.rm = TRUE)
# percentile
percentile_15_43 <- ecdf(beuti_clim$csummean)(mean15)


## 2016 ##
#upwelling accumulation 
yr16 = 2016
byr16 = beuti_daily %>% filter(year == yr16)
byr16$csum <- cumsum(b$X43N) 
byr16$csummean <- rollapply(byr16$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean16 <- mean(byr16$csummean, na.rm = TRUE)
# percentile
percentile_16_43 <- ecdf(beuti_clim$csummean)(mean16)


## 2017 ##
#upwelling accumulation 
yr17 = 2017
byr17 = beuti_daily %>% filter(year == yr17)
byr17$csum <- cumsum(b$X43N) 
byr17$csummean <- rollapply(byr17$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean17 <- mean(byr17$csummean, na.rm = TRUE)
# percentile
percentile_17_43 <- ecdf(beuti_clim$csummean)(mean17)


## 2018 ##
#upwelling accumulation 
yr18 = 2018
byr18 = beuti_daily %>% filter(year == yr18)
byr18$csum <- cumsum(b$X43N) 
byr18$csummean <- rollapply(byr18$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean18 <- mean(byr18$csummean, na.rm = TRUE)
# percentile
percentile_18_43 <- ecdf(beuti_clim$csummean)(mean18)


## 2019 ##
#upwelling accumulation 
yr19 = 2019
byr19 = beuti_daily %>% filter(year == yr19)
byr19$csum <- cumsum(b$X43N) 
byr19$csummean <- rollapply(byr19$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean19 <- mean(byr19$csummean, na.rm = TRUE)
# percentile
percentile_19_43 <- ecdf(beuti_clim$csummean)(mean19)


## 2020 ##
#upwelling accumulation 
yr20 = 2020
byr20 = beuti_daily %>% filter(year == yr20)
byr20$csum <- cumsum(b$X43N) 
byr20$csummean <- rollapply(byr20$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean20 <- mean(byr20$csummean, na.rm = TRUE)
# percentile
percentile_20_43 <- ecdf(beuti_clim$csummean)(mean20)

## 2021 ##
#upwelling accumulation 
yr21 = 2021
byr21 = beuti_daily %>% filter(year == yr21)
byr21$csum <- cumsum(b$X43N) 
byr21$csummean <- rollapply(byr21$csum,windowsize,mean,fill=NA,na.rm = TRUE)
# average upwelling 
mean21 <- mean(byr21$csummean, na.rm = TRUE)
# percentile
percentile_21_43 <- ecdf(beuti_clim$csummean)(mean21)



## dataframe 
percentiles <-data.frame("year" = c(2015, 2016, 2017, 2018, 2019, 2020, 2021), 
                         "perc_34" = c(percentile_15_34, percentile_16_34, percentile_17_34, percentile_18_34, percentile_19_34, percentile_20_34, percentile_21_34),
                         "perc_37" = c(percentile_15_37, percentile_16_37, percentile_17_37, percentile_18_37, percentile_19_37, percentile_20_37, percentile_21_37), 
                         "perc_40" = c(percentile_15_40, percentile_16_40, percentile_17_40, percentile_18_40, percentile_19_40, percentile_20_40, percentile_21_40), 
                         "perc_43" = c(percentile_15_43, percentile_16_43, percentile_17_43, percentile_18_43, percentile_19_43, percentile_20_43, percentile_21_43))

##graph 

Percentiles <- ggplot(percentiles, aes(year, (perc_34:perc_40))) + 
  #34N line
  geom_line(data = percentiles, aes(year, perc_34), color = "tomato", size=1) + 
  #37N line
  geom_line(data = percentiles, aes(year, perc_37), color = "forestgreen", size=1) + 
  #40N line
  geom_line(data = percentiles, aes(year, perc_40), color = "cornflowerblue", size=1) + 
  #43N line
  geom_line(data = percentiles, aes(year, perc_43), color = "mediumorchid4", size=1) + 
  ylab("Percentile") + 
  ggtitle("Comparision of BEUTI Percentiles at Latitudes Along the CA Current") + 
  geom_segment(aes(x=2015, y=.99, xend=2015.5, yend=.99),size=1,color="tomato") + 
  annotate("text", x = 2015.8, y = .99, label = "34N") + 
  geom_segment(aes(x=2015, y=.96, xend=2015.5, yend=.96),size=1,color="forestgreen") + 
  annotate("text", x = 2015.8, y = .96, label = "37N") +
  geom_segment(aes(x=2015, y=.93, xend=2015.5, yend=.93),size=1,color="cornflowerblue") + 
  annotate("text", x = 2015.8, y = .93, label = "40N") +
  geom_segment(aes(x=2015, y=.90, xend=2015.5, yend=.90),size=1,color="mediumorchid4") +
  annotate("text", x = 2015.8, y = .90, label = "43N")

Percentiles

