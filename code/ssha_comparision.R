### 
# ssha_comparision 
# overall CCS ssha mean compared to ssha mean at specific locations: 
# Monterey Bay and Cordell Bank 
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

## set running mean window size for smoothing of ssha data
windowsize = 10

## load in ssha data 
load("/Users/nataliecross/Desktop/THESIS/data/CCS_bluewhale/data/oceanography/CCS-SSHA-nonseasonal.RData")
ssha <- H
load("/Users/nataliecross/Desktop/THESIS/data/CCS_bluewhale/data/oceanography/SSHAquarterly.RData")
ssha_m_c <- S

## generate time series plot for 2015 - 2017 for overall CCS 
ccs = ssha[c(90:101), c(1:2)]
ssha_ccs <- ggplot(data = ccs, aes(x=date, y=SSHA)) + 
  geom_col() + 
  ylim(-2.5,10.5) + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") + 
  ggtitle("Overall CCS SSHA (2015-2017)")
ssha_ccs

## time series plot for 2015-2017 in Monterey Bay 
monterey = ssha_m_c[c(1:12), c(2:3)]
ssha_monterey <- ggplot(data = monterey, aes(x=time, y=SSHA)) + 
  geom_col() + 
  ylim(-2.5,10.5) + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") + 
  ggtitle("Monterey Bay SSHA (2015-2017)")
ssha_monterey 

## time series plot for 2015-2017 in Cordell Bank 
cordell = ssha_m_c[c(13:24), c(2:3)]
ssha_cordell <- ggplot(data = cordell, aes(x=time, y=SSHA)) + 
  geom_col() + 
  ylim(-2.5,10.5) + 
  xlab("year") + 
  ylab("Sea Surface Height Anomaly") + 
  ggtitle("Cordell Bank SSHA (2015-2017)")
ssha_cordell 

ssha_ccs + ssha_monterey + ssha_cordell
