###
# CUTI THRESHOLD 
# calculate percentage of days CUTI is above a significant threshold 
# threshold = 0.5 m/s 
# time range = July 1st --> December 31st (months when CUTI is most influential)
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
library(mosaic)

## set running mean window size for smoothing of daily cuti data
windowsize = 10

## load in daily cuti (from Jacox et al., 2018)
cuti_daily <- read.csv("data/oceanography/cuti_daily_nov2021.csv",header = TRUE) 


