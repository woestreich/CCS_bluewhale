# ssha_timeseries_attempt

## clear variables and load packages
rm(list = ls())
library(tidyverse)
library(patchwork)
library(RColorBrewer) 
library(zoo)
library(lubridate)
library(directlabels)
library(ggplot2)

## set running mean window size for smoothing of ssha data
windowsize = 10

## load in ssha data 
load("/Users/nataliecross/Desktop/THESIS/data/CCS_bluewhale/data/oceanography/CCS-SSHA-nonseasonal.RData")
ssha <- H

## Generate time series plot
tiff("ssha_timeseries.tiff",units="in", width=10, height=4, res=400)
ssha_timeseries <- ggplot(data = ssha, aes(x=date, y=SSHA)) + 
  geom_col() + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") + 
  ggtitle("Time Series (1992-2020) of Average Sea Surface Height Anomaly")

##Display time series plot
ssha_timeseries
dev.off()

##recent timeseries for 2015 - 2020 ssha data 
recent <- ssha[c(90:113), c(1:2)]
tiff("ssha_recent.tiff",units="in", width=10, height=4, res=400)
ssha_recent <- ggplot(data = recent, aes(x=date, y=SSHA)) + 
  geom_col() + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") + 
  ggtitle("Time Series (2015-2020) of Average Sea Surface Height Anomaly")
ssha_recent
dev.off()

