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
ssha_timeseries <- ggplot(data = ssha, aes(x=date, y=SSHA)) + 
  geom_col() + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") 
##need to adjust axis and formatting

##Display time series plot
ssha_timeseries
