##########################################
# Project: CRT Group Project - Waves  
# Purpose: TSA wave period - hourly
# Author: Caroline Brasileiro Pena
# Date: 10/2021
##########################################

rm(list = ls()) # tidy work space
gc()

require(tidyverse)
require(kableExtra)
require(tseries)
require(TSA)
require(imputeTS)

data_h <- read.csv("bouy_data_hourly_mean.csv") # hourly data

### Stations data frames #############################################################################################################

M1_h <- data_h %>% filter(station_id == "M1")
M2_h <- data_h %>% filter(station_id == "M2")
M3_h <- data_h %>% filter(station_id == "M3")
M4_h <- data_h %>% filter(station_id == "M4" | station_id == "M4-Archive")
M5_h <- data_h %>% filter(station_id == "M5")
M6_h <- data_h %>% filter(station_id == "M6")
FS1_h <- data_h %>% filter(station_id == "FS1")
Belmullet_h <- data_h %>% filter(station_id == "Belmullet-AMETS")
M1_interp <- as.vector(read.csv("M1_interpolated_data.csv")) %>% select(waveperiod)

### TSA wave period - M1 ########################################################################################################

Date_full <- tibble(date = seq(as.POSIXct("2001-02-06 00:00:00"), 
                               as.POSIXct("2007-07-09 23:00:00"), by="hour")) #Create a vector with all hours
M1_t <- tibble(time = as.POSIXct(M1_h$time, format="%Y-%m-%dT%H:%M:%S"), waveperiod = M1_interp$waveperiod)
M1_t <- left_join(Date_full, M1_t, by=c("date" = "time")) # Bind dates with existent period values
M1_t <- zoo::na.locf(M1_t$waveperiod) # replace NA values with the last value observed

firstHour <- (as.Date("2001-02-06 23:00:00")-as.Date("2001-01-01 00:00:00"))
M1_period  <- ts(M1_t, start=c(0),frequency=24) # Hourly time series

plot(M1_period, main = "M1 wave period")


M1_period_training <- ts(M1_t[1:47518],start=c(0), frequency=24) # training set
M1_period_test <- ts(M1_t[47519:56279],start=c(1980), frequency=24) # test set


boxcox <- BoxCox.ar(M1_period_training) 
boxcox$mle # 0.4
boxcox$ci # 0.4 to 0.4 
M1_period_training <- M1_period_training^0.4 # TRANSFORMATION = 0.4

adf.test(M1_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M1_period_training)
plot(decomp)
diff(range(M1_period_training)) # 1.022199
diff(range(decomp$trend,na.rm=T)) # 0.6261555
diff(range(decomp$seasonal,na.rm=T)) # 0.004018053
diff(range(decomp$random,na.rm=T)) # 1.098205
# the most expressive trend is the random, but overall trend also plays an important role. No seasonality.


ACF <- acf(M1_period_training, plot = F)
ACF$lag=ACF$lag*24
plot(ACF) #not a white noise;
ACF_diff <- acf(diff(M1_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*24
plot(ACF_diff) #RANDOM WALK

