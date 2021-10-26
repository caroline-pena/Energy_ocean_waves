##########################################
# Project: CRT Group Project - Waves  
# Objective: TSA wave period - monthly
# Author: Caroline Brasileiro Pena
# Date: 10/2021
##########################################

rm(list = ls()) # tidy work space
gc()

require(tidyverse)
require(kableExtra)
require(imputeTS)
require(tseries)
library(TSA)


data_d <- read.csv("bouy_data_daily.csv") # daily data


### Stations data frames #############################################################################################################

M1 <- data_d %>% filter(station_id == "M1")
M2 <- data_d %>% filter(station_id == "M2")
M3 <- data_d %>% filter(station_id == "M3")
M4 <- data_d %>% filter(station_id == "M4" | station_id == "M4-Archive")
M5 <- data_d %>% filter(station_id == "M5")
M6 <- data_d %>% filter(station_id == "M6")
FS1 <- data_d %>% filter(station_id == "FS1")
Belmullet <- data_d %>% filter(station_id == "Belmullet-AMETS")


### TSA wave period - M1 ########################################################################################################

M1 <- M1 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M1_t <- M1 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M1_t <- na_interpolation(M1_t$waveperiod, option = "spline") # replace NA values with values from spline interpolation
M1_period <- ts(M1_t, start=c(2001, 2), end=c(2007, 7), frequency=12) # TS object



plot(M1_period, main = "M1 wave period")
points(x=as.vector(time(M1_period)), y=as.vector(M1_period),
       pch=as.vector(season(M1_period)), cex=0.6, col=4)

M1_period_training <- ts(M1_t[1:65],start=c(2001, 2), end=c(2006, 7), frequency=12) # training set
M1_period_test <- ts(M1_t[66:78],start=c(2006, 8), end=c(2007, 7), frequency=12) # test set


boxcox <- BoxCox.ar(M1_period_training) 
boxcox$mle # 0
boxcox$ci # -1 to 1.3 -> no transformation, since 1 is inside the CI

adf.test(M1_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M1_period_training)
plot(decomp)
diff(range(M1_period_training)) # 3.673759
diff(range(decomp$trend,na.rm=T)) # 0.8860234
diff(range(decomp$seasonal,na.rm=T)) # 1.953137
diff(range(decomp$random,na.rm=T)) # 1.925885
# the most expressive trend is the seasonal, but randomic values affect the series greatly


ACF <- acf(M1_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise; suggestion of AR component
ACF_diff <- acf(diff(M1_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M1_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(4)

eacf(M1_period_training) # AR(1), ARMA(2,1)
subs <- armasubsets(M1_period_training,nar=12,nma=12) 
plot(subs) # ARMA(9,12)...


# Seasonal analysis:

sdiff <- diff(M1_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SIMA(12)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SARI(12)12

# Trying seasonal modelling:

model1 <- arima(M1_period_training, order = c(2,0,1), seasonal = list(order=c(1,1,1), period=12)) # ARIMA (2,0,1) x (1,1,1)12
model1 #AIC = 98.25
tsdiag(model1) # This looks ok  -> The timeplot of the residuals looks relatively good and between -2 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model1)) # residuals approx. normally distributed
qqnorm(rstandard(model1)); qqline(rstandard(model1)) # does look approx. normal
shapiro.test(rstandard(model1)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(model1, n.ahead = 24, col = "blue", 
     main = "Wave period prediction - M1", ylab = "Period (s)")
lines(M1_period_test, col='red', type = 'o') # the test set
legend(2001,5.5, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (2,0,1) x (1,1,1)12", side=3)


# Error:
model1.p <- predict(model1, n.ahead=12) # the predicted values
model1.e <- sqrt(mean((model1.p$pred - M1_period_test)^2)) # RMSE = 2.298454


# M1: SOMETHING UNUSUAL HAPPENED IN THE LAST OBSERVED MONTHS


### TSA wave period - M2 ########################################################################################################

M2 <- M2 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M2_t <- M2 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M2_t <- tibble(Month_Yr = unique(M2$Month_Yr), waveperiod = M2_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2001-05-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M2_t <- left_join(Date_full, M2_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M2_t <- na_interpolation(M2_t$waveperiod, option = "spline") # replace NA values with values from spline interpolation
M2_period <- ts(M2_t, start=c(2001, 5), end=c(2021, 9), frequency=12) # TS object



plot(M2_period, main = "M2 wave period")
points(x=as.vector(time(M2_period)), y=as.vector(M2_period),
       pch=as.vector(season(M2_period)), cex=0.6, col=4)

M2_period_training <- ts(M2_t[1:232], start=c(2001, 5), end=c(2020, 9), frequency=12) # training set
M2_period_test <- ts(M2_t[233:245], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M2_period_training) 
boxcox$mle # -1.3
boxcox$ci # -2 to -0.3 
M2_period_training <- M2_period_training^-1.3 # TRANSFORMATION: -1.3

adf.test(M2_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M2_period_training)
plot(decomp)
diff(range(M2_period_training)) # 0.1057905
diff(range(decomp$trend,na.rm=T)) # 0.06444298
diff(range(decomp$seasonal,na.rm=T)) # 0.03296043
diff(range(decomp$random,na.rm=T)) # 0.06012751
# the most expressive trend is the seasonal, but randomic values affect the series greatly


ACF <- acf(M2_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, all positive
ACF_diff <- acf(diff(M2_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M2_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(14)


eacf(M2_period_training) # AR(1), ARMA(2,1)
subs <- armasubsets(M2_period_training,nar=12,nma=12) 
plot(subs) # AR(5)...


# Seasonal analysis:

sdiff <- diff(M2_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # No model
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# No model

# Trying seasonal modelling:

model2 <- arima(M2_period_training, order = c(1,0,0), seasonal = list(order=c(2,1,1),period=12)) # ARIMA (1,0,0) x (2,1,1)12
model2 #AIC = -1315.64
tsdiag(model2) # This looks ok  -> The timeplot of the residuals looks relatively good and between -3 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model2)) # residuals approx. normally distributed
qqnorm(rstandard(model2)); qqline(rstandard(model2)) # does look approx. normal
shapiro.test(rstandard(model2)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(model2, n.ahead = 24, transform=function(x){x^(-1/1.3)}, col = "blue", 
     main = "Wave period prediction - M2", ylab = "Period (s)")
lines(M2_period_test, col='red', type = 'o') # the test set
legend(2001,6.3, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,0) x (2,1,1)12", side=3)

# Error:
model2.p <- predict(model2, n.ahead=12) # the predicted values
model2.p$pred <- (model2.p$pred)^(-1/1.3) # transform the predicted values
model2.e <- sqrt(mean((model2.p$pred - M2_period_test)^2)) # mean squared error = 0.393754


# M2: Looks ok, but not so good

### TSA wave period - M3 ########################################################################################################

M3 <- M3 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M3_t <- M3 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M3_t <- tibble(Month_Yr = unique(M3$Month_Yr), waveperiod = M3_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2002-07-22"), 
                                                   as.Date("2021-09-10"), by = "months")), "%Y-%m")) # Create a vector with all months
M3_t <- left_join(Date_full, M3_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M3_t <- na_interpolation(M3_t$waveperiod, option = "spline") # replace NA values with values from spline interpolation
M3_period <- ts(M3_t, start=c(2002, 7), end=c(2021, 9), frequency=12) # TS object



plot(M3_period, main = "M3 wave period")
points(x=as.vector(time(M3_period)), y=as.vector(M3_period),
       pch=as.vector(season(M3_period)), cex=0.6, col=4)

M3_period_training <- ts(M3_t[1:218], start=c(2002, 7), end=c(2020, 9), frequency=12) # training set
M3_period_test <- ts(M3_t[219:231], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M3_period_training) 
boxcox$mle # 0.3
boxcox$ci # -0.4 to 1.1 
 # TRANSFORMATION: NO (1 inside the CI)

adf.test(M3_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M3_period_training)
plot(decomp)
diff(range(M3_period_training)) # 4.058741
diff(range(decomp$trend,na.rm=T)) # 2.14545
diff(range(decomp$seasonal,na.rm=T)) # 1.904117
diff(range(decomp$random,na.rm=T)) # 4.450497
# the most expressive trend is the randomic, but seasonal trend affects the series greatly


ACF <- acf(M3_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M3_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M3_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(3)


eacf(M3_period_training) # ARMA(2,1), MA(2)
subs <- armasubsets(M3_period_training,nar=12,nma=12) 
plot(subs) # AR(10)...


# Seasonal analysis:

sdiff <- diff(M3_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # No model
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# AR(13)12

# Trying seasonal modelling:

model3 <- arima(M3_period_training, order = c(3,0,1), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (3,0,1) x (1,1,1)12
model3 #AIC = 398.02
tsdiag(model3) # This looks reasonable -> The timeplot of the residuals looks relatively good and between -3 and 3;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model3)) # residuals approx. normally distributed
qqnorm(rstandard(model3)); qqline(rstandard(model3)) # does look approx. normal
shapiro.test(rstandard(model3)) #p-value < 0.05 -> non-normally distributed


# Prediction:
plot(model3, n.ahead = 24, col = "blue", 
     main = "Wave period prediction - M3", ylab = "Period (s)")
lines(M3_period_test, col='red', type = 'o') # the test set
legend(2002,5.5, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (3,0,1) x (1,1,1)12", side=3)

# Error:
model3.p <- predict(model3, n.ahead=12) # the predicted values
model3.e <- sqrt(mean((model3.p$pred - M3_period_test)^2)) # RMSE = 0.8104999


# M3: Looks ok, but not so good. RESIDUALS ARE NON-NORMALLY DISTRIBUTED\



### TSA wave period - M4 ########################################################################################################

M4 <- M4 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M4_t <- M4 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M4_t <- tibble(Month_Yr = unique(M4$Month_Yr), waveperiod = M4_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2003-04-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M4_t <- left_join(Date_full, M4_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M4_t <- na_interpolation(M4_t$waveperiod, option = "spline") # replace NA values with values from spline interpolation
M4_period <- ts(M4_t, start=c(2003, 4), end=c(2021, 9), frequency=12) # TS object



plot(M4_period, main = "M4 wave period")
points(x=as.vector(time(M4_period)), y=as.vector(M4_period),
       pch=as.vector(season(M4_period)), cex=0.6, col=4)

M4_period_training <- ts(M4_t[1:209], start=c(2003, 4), end=c(2020, 9), frequency=12) # training set
M4_period_test <- ts(M4_t[210:222], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M4_period_training) 
boxcox$mle # 0.3
boxcox$ci # -0.4 to 1.0 
# TRANSFORMATION: NO (1 inside the CI)

adf.test(M4_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M4_period_training)
plot(decomp)
diff(range(M4_period_training)) # 4.798606
diff(range(decomp$trend,na.rm=T)) # 1.925132
diff(range(decomp$seasonal,na.rm=T)) # 1.844686
diff(range(decomp$random,na.rm=T)) # 2.524041
# the most expressive trend is the randomic, but seasonal and overall trends affect the series greatly


ACF <- acf(M4_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M4_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M4_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(4)


eacf(M4_period_training) # ARMA(2,1), AR(4), MA(3), ...
subs <- armasubsets(M4_period_training,nar=12,nma=12) 
plot(subs) # AR(10)...


# Seasonal analysis:

sdiff <- diff(M4_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SMA(14)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12, SAR(13)12,...

# Trying seasonal modelling:

model4 <- arima(M4_period_training, order = c(2,0,1), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (2,0,1) x (1,1,1)12
model4 #AIC = 257.31
tsdiag(model4) # Really nice plots -> The timeplot of the residuals looks relatively good and between -2 and 2;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(model4)) # residuals approx. normally distributed
qqnorm(rstandard(model4)); qqline(rstandard(model4)) # does look approx. normal
shapiro.test(rstandard(model4)) #p-value >> 0.05 -> normally distributed


# Prediction:
plot(model4, n.ahead = 24, col = "blue", 
     main = "Wave period prediction - M4", ylab = "Period (s)")
lines(M4_period_test, col='red', type = 'o') # the test set
legend(2003,9.9, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (2,0,1) x (1,1,1)12", side=3)

# Error:
model4.p <- predict(model4, n.ahead=12) # the predicted values
model4.e <- sqrt(mean((model4.p$pred - M4_period_test)^2)) # RMSE = 0.5025982


# M4: Fits well!

### TSA wave period - M5 ########################################################################################################

M5 <- M5 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M5_t <- M5 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M5_t <- tibble(Month_Yr = unique(M5$Month_Yr), waveperiod = M5_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2004-10-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M5_t <- left_join(Date_full, M5_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M5_t <- na_interpolation(M5_t$waveperiod, option = "stine") # replace NA values with values from spline interpolation
M5_period <- ts(M5_t, start=c(2004, 10), end=c(2021, 9), frequency=12) # TS object



plot(M5_period, main = "M5 wave period")
points(x=as.vector(time(M5_period)), y=as.vector(M5_period),
       pch=as.vector(season(M5_period)), cex=0.6, col=4)

M5_period_training <- ts(M5_t[1:182], start=c(2004, 10), end=c(2020, 9), frequency=12) # training set
M5_period_test <- ts(M5_t[183:195], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M5_period_training) 
boxcox$mle # -0.6
boxcox$ci # -1.4 to 0.3 
M5_period_training <- log(M5_period_training)
# TRANSFORMATION: LN (0 inside the CI)

adf.test(M5_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M5_period_training)
plot(decomp)
diff(range(M5_period_training)) # 0.5877473
diff(range(decomp$trend,na.rm=T)) # 0.2834926
diff(range(decomp$seasonal,na.rm=T)) # 0.1264384
diff(range(decomp$random,na.rm=T)) # 0.5410111
# the most expressive trend is the randomic, but seasonal and overall trends affect the series greatly


ACF <- acf(M5_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M5_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk, but very close to be

PACF <- pacf(M5_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1)


eacf(M5_period_training) # ARMA(1,1), ARMA(2,1), ...
subs <- armasubsets(M5_period_training,nar=12,nma=12) 
plot(subs) # ARMA(9,9)...


# Seasonal analysis:

sdiff <- diff(M5_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # no suggestions
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12

# Trying seasonal modelling:

model5 <- arima(M5_period_training, order = c(1,0,1), seasonal = list(order=c(1,1,0),period=12)) # ARIMA (1,0,1) x (1,1,0)12
model5 #AIC = -258.97
tsdiag(model5) # Nice plots -> The timeplot of the residuals looks relatively good and between -4 and 3;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(model5)) # residuals approx. normally distributed
qqnorm(rstandard(model5)); qqline(rstandard(model5)) # does look approx. normal
shapiro.test(rstandard(model5)) #p-value < 0.05 -> non-normally distributed, but probably because of one outlier


# Prediction:
plot(model5, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave period prediction - M5", ylab = "Period (s)")
lines(M5_period_test, col='red', type = 'o') # the test set
legend(2005,8.5, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,1) x (1,1,0)12", side=3)


# Error:
model5.p <- predict(model5, n.ahead=12) # the predicted values
model5.p$pred <- exp(model5.p$pred) # transform predictions
model5.e <- sqrt(mean((model5.p$pred - M5_period_test)^2)) # RMSE = 0.849516


# M5: Looks ok, with some deviations


### TSA wave period - M6 ########################################################################################################

M6 <- M6 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M6_t <- M6 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
M6_t <- tibble(Month_Yr = unique(M6$Month_Yr), waveperiod = M6_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2006-09-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M6_t <- left_join(Date_full, M6_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M6_t <- na_interpolation(M6_t$waveperiod, option = "stine") # replace NA values with values from spline interpolation
M6_period <- ts(M6_t, start=c(2006, 9), end=c(2021, 9), frequency=12) # TS object



plot(M6_period, main = "M6 wave period")
points(x=as.vector(time(M6_period)), y=as.vector(M6_period),
       pch=as.vector(season(M6_period)), cex=0.6, col=4)

M6_period_training <- ts(M6_t[1:168], start=c(2006, 9), end=c(2020, 9), frequency=12) # training set
M6_period_test <- ts(M6_t[169:181], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M6_period_training) 
boxcox$mle # -0.5
boxcox$ci # -1.4 to 0.3 
M6_period_training <- log(M6_period_training)
# TRANSFORMATION: LN (0 inside the CI)

adf.test(M6_period_training) # p-value = 0.01 -> stationary


decomp <- decompose(M6_period_training)
plot(decomp)
diff(range(M6_period_training)) # 0.5287177
diff(range(decomp$trend,na.rm=T)) # 0.1086169
diff(range(decomp$seasonal,na.rm=T)) # 0.2781315
diff(range(decomp$random,na.rm=T)) # 0.2677506
# the most expressive trend is the seasonal, but randomic and overall trends affect the series greatly


ACF <- acf(M6_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M6_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M6_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(5)


eacf(M6_period_training) # AR(6), ARMA(2,1), MA(2),...
subs <- armasubsets(M6_period_training,nar=12,nma=12) 
plot(subs) # AR(11),...


# Seasonal analysis:

sdiff <- diff(M6_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SMA(1)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12

# Trying seasonal modelling:

model6 <- arima(M6_period_training, order = c(2,0,1), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (2,0,1) x (1,1,1)12
model6 #AIC = -436.9
tsdiag(model6) # Nice plots -> The timeplot of the residuals looks relatively good and between -3 and 2;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(model6)) # residuals approx. normally distributed
qqnorm(rstandard(model6)); qqline(rstandard(model6)) # does look approx. normal
shapiro.test(rstandard(model6)) #p-value >> 0.05 -> normally distributed


# Prediction:
plot(model6, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave period prediction - M6", ylab = "Period (s)")
lines(M6_period_test, col='red', type = 'o') # the test set
legend(2007,9.7, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (2,0,1) x (1,1,1)12", side=3)

# Error:
model6.p <- predict(model6, n.ahead=12) # the predicted values
model6.p$pred <- exp(model6.p$pred) # transform predictions
model6.e <- sqrt(mean((model6.p$pred - M6_period_test)^2)) # mean squared error = 0.2915622


# M6: Looks ok, with some deviations


### TSA wave period - FS1 ########################################################################################################

FS1 <- FS1 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
FS1_t <- FS1 %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
FS1_t <- tibble(Month_Yr = unique(FS1$Month_Yr), waveperiod = FS1_t$waveperiod)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2003-01-01"), 
                                                   as.Date("2008-02-01"), by = "months")), "%Y-%m")) # Create a vector with all months
FS1_t <- left_join(Date_full, FS1_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
FS1_t <- na_interpolation(FS1_t$waveperiod, option = "stine") # replace NA values with values from spline interpolation
FS1_period <- ts(FS1_t, start=c(2003, 1), end=c(2008, 2), frequency=12) # TS object



plot(FS1_period, main = "FS1 wave period") # bad values at the end of the TS -> cut off last 2y
points(x=as.vector(time(FS1_period)), y=as.vector(FS1_period),
       pch=as.vector(season(FS1_period)), cex=0.6, col=4) 

FS1_period_training <- ts(FS1_t[1:36], start=c(2003, 1), end=c(2006, 12), frequency=12) # training set
FS1_period_test <- ts(FS1_t[37:44], start=c(2007, 1), end=c(2007, 8), frequency=12) # test set


boxcox <- BoxCox.ar(FS1_period_training) 
boxcox$mle # -0.1
boxcox$ci # -1.3 to 1.9
# TRANSFORMATION: NONE (1 inside the CI)

adf.test(FS1_period_training) # p-value = 0.0236 (<0.05) -> stationary


decomp <- decompose(FS1_period_training)
plot(decomp)
diff(range(FS1_period_training)) # 2.211611
diff(range(decomp$trend,na.rm=T)) # 0.2482885
diff(range(decomp$seasonal,na.rm=T)) # 1.548128
diff(range(decomp$random,na.rm=T)) # 1.407887
# the most expressive trend is the seasonal, but randomic trend affects the series greatly


ACF <- acf(FS1_period_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(FS1_period_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) # Random walk! (lag 6 just significant)

PACF <- pacf(FS1_period_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(6)


eacf(FS1_period_training) # AR(1), MA(1), ...
subs <- armasubsets(FS1_period_training,nar=12,nma=12) 
plot(subs) # AR(7)


# Seasonal analysis:

sdiff <- diff(FS1_period_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # no seasonality
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# no seasonality

# Trying seasonal modelling:

modelFS1 <- arima(FS1_period_training, order = c(1,0,0), seasonal = list(order=c(1,1,0),period=12)) # ARIMA (2,0,1) x (1,1,1)12
modelFS1 #AIC = 51.63
tsdiag(modelFS1) # Nice plots -> The timeplot of the residuals looks relatively good and between -3 and 1;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(modelFS1)) # residuals approx. normally distributed
qqnorm(rstandard(modelFS1)); qqline(rstandard(modelFS1)) # does not look normal
shapiro.test(rstandard(modelFS1)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(modelFS1, n.ahead = 24, col = "blue", 
     main = "Wave period prediction - FS1", ylab = "Period (s)")
lines(FS1_period_test, col='red', type = 'o') # the test set
legend(2003,7.3, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,0) x (1,1,0)12", side=3)

# Error:
modelFS1.p <- predict(modelFS1, n.ahead=8) # the predicted values
modelFS1.e <- sqrt(mean((modelFS1.p$pred - FS1_period_test)^2)) # mean squared error = 0.1364608


# FS1: Doesn't look very good, but real values are inside CI


### TSA wave period - Balmullet ########################################################################################################

Belmullet <- Belmullet %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
Belmullet_t <- Belmullet %>% group_by(Month_Yr) %>% summarise(waveperiod = mean(waveperiod)) %>% ungroup() # mean period per month-year
Belmullet_t <- tibble(Month_Yr = unique(Belmullet$Month_Yr), waveperiod = Belmullet_t$waveperiod)
# Too few observations! Only 5... 

#######
Errors_period <- tibble(Station = c("M1", "M2", "M3", "M4", "M5", "M6", "FS1", "Belmullet"),
                        RMSE_period = c(model1.e, model2.e, model3.e, model4.e, model5.e, model6.e, modelFS1.e, "NA"))

