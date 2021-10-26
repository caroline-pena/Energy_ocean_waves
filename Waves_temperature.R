##########################################
# Project: CRT Group Project - Waves  
# Objective: TSA sea temperature
# Author: Caroline Brasileiro Pena
# Date: 09/2021
##########################################

rm(list = ls()) # tidy work space
gc()

require(tidyverse)
require(kableExtra)
require(tseries)
require(TSA)
require(imputeTS)

data <- read.csv("bouy_data_daily.csv") # daily data


### Stations data frames #############################################################################################################

unique(data$station_id) # M1, M2, M3, FS1, M4-Archive, M5, M6, M4, Belmullet-AMETS

M1 <- data %>% filter(station_id == "M1")
M2 <- data %>% filter(station_id == "M2")
M3 <- data %>% filter(station_id == "M3")
M4 <- data %>% filter(station_id == "M4" | station_id == "M4-Archive")
M5 <- data %>% filter(station_id == "M5")
M6 <- data %>% filter(station_id == "M6")
FS1 <- data %>% filter(station_id == "FS1")
Belmullet <- data %>% filter(station_id == "Belmullet-AMETS")

### Initial exploration ##############################################################################################################

par(mfrow=c(2,3))
hist(M1$seatemperature)
hist(M2$seatemperature)
hist(M3$seatemperature)
hist(M4$seatemperature)
hist(M5$seatemperature)
hist(M6$seatemperature)
par(mfrow=c(1,1))

### TS Analysis - Sea temperature ########################################################################################################

#---------- M1:
M1 <- M1 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M1_t <- M1 %>% group_by(Month_Yr) %>% summarise(seatemperature = mean(seatemperature)) %>% ungroup() # mean temperature per month-year
M1_temp <- ts(M1_t$seatemperature,start=c(2001, 2), end=c(2007, 7), frequency=12) # TS object

plot(M1_temp, main = "M1 sea temperature")
points(x=as.vector(time(M1_temp)), y=as.vector(M1_temp),
       pch=as.vector(season(M1_temp)), cex=0.6, col=4)

M1_temp_training <- ts(M1_t$seatemperature[1:66],start=c(2001, 2), end=c(2006, 7), frequency=12) # training set
M1_temp_test <- ts(M1_t$seatemperature[67:78],start=c(2006, 8), end=c(2007, 7), frequency=12) # test set




boxcox <- BoxCox.ar(M1_temp_training) # 0 is in the CI -> ln transformation
M1_temp_training <- log(M1_temp_training)
adf.test(M1_temp_training) # p-value = 0.01 -> stationary


decomp <- decompose(M1_temp_training)
plot(decomp)
diff(range(M1_temp_training)) # 7.341547
diff(range(decomp$trend,na.rm=T)) # 0.8155539
diff(range(decomp$seasonal,na.rm=T)) # 5.517725
diff(range(decomp$random,na.rm=T)) # 3.581548
# the most expressive trend is the seasonal


ACF <- acf(M1_temp_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise; suggestion of AR component
ACF_diff <- acf(diff(M1_temp_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M1_temp_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(2)

eacf(M1_temp_training) # MA(2), ARMA(2,1) ...
subs <- armasubsets(M1_temp_training,nar=12,nma=12) 
plot(subs) # AR(12), ARMA(10,8)...


# Seasonal analysis:

sdiff <- diff(M1_temp_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SIMA(2)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SARI(2)12

# Trying seasonal modelling:

model1 <- arima(M1_temp_training, order = c(2,1,1), seasonal = list(order=c(2,1,2),period=12)) # ARIMA (2,1,1) x (2,1,2)12
model1 #AIC = -178.88
tsdiag(model1) # This looks ok  -> The timeplot of the residuals looks relatively good and between -2 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model1)) # residuals approx. normally distributed
qqnorm(rstandard(model1)); qqline(rstandard(model1)) # normal
shapiro.test(rstandard(model1)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(model1, n.ahead=24, transform=function(x){exp(x)}, col = "blue", 
     main = "Sea temperature prediction - M1", ylab = "Temperature")
lines(M1_temp_test, col='red', type = 'o') # the test set


# Error:
model1.p <- predict(model1, n.ahead=11) # the predicted values
model1.p$pred <- exp(model1.p$pred) # transform the predicted values
model1.e <- mean((model1.p$pred - M1_temp_test)^2) # mean squared error = 0.6831798




#---------- M2:
M2 <- M2 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M2_t <- M2 %>% group_by(Month_Yr) %>% summarise(seatemperature = mean(seatemperature)) %>% ungroup() # mean temperature per month-year
M2_temp <- ts(M2_t$seatemperature,start=c(2001, 5), end=c(2021, 9), frequency=12) # TS object




M2_temp <- na_seadec(M2_temp) # replace NA values with values from interpolation




plot(M2_temp, main = "M2 sea temperature")
points(x=as.vector(time(M2_temp)), y=as.vector(M2_temp),
       pch=as.vector(season(M2_temp)), cex=0.6, col=4)

decomp <- decompose(M2_temp)
plot(decomp)

ACF <- acf(M2_temp, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise
ACF_diff <- acf(diff(M2_temp), plot = F)
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

