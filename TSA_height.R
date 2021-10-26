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


### TSA wave length - M1 ########################################################################################################

M1 <- M1 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M1_t <- M1 %>% group_by(Month_Yr) %>% summarise(waveheight= mean(waveheight)) %>% ungroup() # mean period per month-year
M1_t <- na_interpolation(M1_t$waveheight, option = "spline") # replace NA values with values from spline interpolation
M1_height <- ts(M1_t, start=c(2001, 2), end=c(2007, 7), frequency=12) # TS object



plot(M1_height, main = "M1 wave height")
points(x=as.vector(time(M1_height)), y=as.vector(M1_height),
       pch=as.vector(season(M1_height)), cex=0.6, col=4)

M1_height_training <- ts(M1_t[1:65],start=c(2001, 2), end=c(2006, 7), frequency=12) # training set
M1_height_test <- ts(M1_t[66:78],start=c(2006, 8), end=c(2007, 7), frequency=12) # test set


boxcox <- BoxCox.ar(M1_height_training) 
boxcox$mle # -0.1
boxcox$ci # -0.6 to 0.4 -> ln transformation, since 0 is inside the CI
M1_height_training <- log(M1_height_training)
adf.test(M1_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M1_height_training)
plot(decomp)
diff(range(M1_height_training)) # 1.224528
diff(range(decomp$trend,na.rm=T)) # 0.3286766
diff(range(decomp$seasonal,na.rm=T)) # 0.6867089
diff(range(decomp$random,na.rm=T)) # 0.6905352
# the most expressive trend is the randomic, but seasonal trend affects the series greatly


ACF <- acf(M1_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise; suggestion of AR component
ACF_diff <- acf(diff(M1_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M1_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(5)

eacf(M1_height_training) # AR(1), AR(5), AR(6), AR(7), ARMA(2,1), ...
subs <- armasubsets(M1_height_training,nar=12,nma=12) 
plot(subs) # ARMA(12,7)...


# Seasonal analysis:

sdiff <- diff(M1_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SIMA(1)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SARI(1)12

# Trying seasonal modelling:

model1 <- arima(M1_height_training, order = c(1,0,0), seasonal = list(order=c(1,1,1), period=12)) # AR(1) x (1,1,1)12
model1 #AIC = -4.07
tsdiag(model1) # This looks ok  -> The timeplot of the residuals looks relatively good and between -2 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model1)) # residuals approx. normally distributed
qqnorm(rstandard(model1)); qqline(rstandard(model1)) # does look approx. normal
shapiro.test(rstandard(model1)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(model1, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave height prediction - M1", ylab = "Height (m)")
lines(M1_height_test, col='red', type = 'o') # the test set
legend(2001,7, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,0) x (1,1,1)12", side=3)


# Error:
model1.p <- predict(model1, n.ahead=12) # the predicted values
model1.p$pred <- exp(model1.p$pred) # transformation of predicted values
model1.e <- sqrt(mean((model1.p$pred - M1_height_test)^2)) # mean squared error = 1.016454


# M1: Good-looking model


### TSA wave height - M2 ########################################################################################################

M2 <- M2 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M2_t <- M2 %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
M2_t <- tibble(Month_Yr = unique(M2$Month_Yr), waveheight = M2_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2001-05-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M2_t <- left_join(Date_full, M2_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M2_t <- na_interpolation(M2_t$waveheight, option = "stine") # replace NA values with values from spline interpolation
M2_height <- ts(M2_t, start=c(2001, 5), end=c(2021, 9), frequency=12) # TS object



plot(M2_height, main = "M2 wave height")
points(x=as.vector(time(M2_height)), y=as.vector(M2_height),
       pch=as.vector(season(M2_height)), cex=0.6, col=4)

M2_height_training <- ts(M2_t[1:232], start=c(2001, 5), end=c(2020, 9), frequency=12) # training set
M2_height_test <- ts(M2_t[233:245], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M2_height_training) 
boxcox$mle # 0.1
boxcox$ci # -0.2 to 0.3 
M2_height_training <- log(M2_height_training) # TRANSFORMATION: ln

adf.test(M2_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M2_height_training)
plot(decomp)
diff(range(M2_height_training)) # 1.504974
diff(range(decomp$trend,na.rm=T)) # 0.9428491
diff(range(decomp$seasonal,na.rm=T)) # 0.622393
diff(range(decomp$random,na.rm=T)) # 0.9448978
# the most expressive trend is the randomic, followed by overall trend and then seasonality


ACF <- acf(M2_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. AR suggestion
ACF_diff <- acf(diff(M2_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk. Again, AR suggestion

PACF <- pacf(M2_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(5), AR(9) 


eacf(M2_height_training) # AR(5), AR(6), ARMA(2,2),...
subs <- armasubsets(M2_height_training,nar=12,nma=12) 
plot(subs) # ARMA(4,4)...


# Seasonal analysis:

sdiff <- diff(M2_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # No model. SAR suggestion
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(13)12, SAR(1)

# Trying seasonal modelling:

model2 <- arima(M2_height_training, order = c(2,0,2), seasonal = list(order=c(1,1,0),period=12)) # ARIMA (1,0,0) x (2,1,1)12
model2 #AIC = -1.4
tsdiag(model2) # This looks ok  -> The timeplot of the residuals looks relatively good and between -2 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model2)) # residuals approx. normally distributed
qqnorm(rstandard(model2)); qqline(rstandard(model2)) # does look approx. normal (very good looking)
shapiro.test(rstandard(model2)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(model2, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave period prediction - M2", ylab = "Height (m)")
lines(M2_height_test, col='red', type = 'o') # the test set
legend(2001,3, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (2,0,2) x (1,1,0)12", side=3)


# Error:
model2.p <- predict(model2, n.ahead=12) # the predicted values
model2.p$pred <- exp(model2.p$pred) # transform the predicted values
model2.e <- sqrt(mean((model2.p$pred - M2_height_test)^2)) # RMSE = 0.4557903


# M2: Looks ok, but not so good

### TSA wave height - M3 ########################################################################################################

M3 <- M3 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M3_t <- M3 %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
M3_t <- tibble(Month_Yr = unique(M3$Month_Yr), waveheight = M3_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2002-07-22"), 
                                                   as.Date("2021-09-10"), by = "months")), "%Y-%m")) # Create a vector with all months
M3_t <- left_join(Date_full, M3_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M3_t <- na_interpolation(M3_t$waveheight, option = "spline") # replace NA values with values from spline interpolation
M3_height <- ts(M3_t, start=c(2002, 7), end=c(2021, 9), frequency=12) # TS object



plot(M3_height, main = "M3 wave height")
points(x=as.vector(time(M3_height)), y=as.vector(M3_height),
       pch=as.vector(season(M3_height)), cex=0.6, col=4)

M3_height_training <- ts(M3_t[1:218], start=c(2002, 7), end=c(2020, 9), frequency=12) # training set
M3_height_test <- ts(M3_t[219:231], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M3_height_training) 
boxcox$mle # 0.2
boxcox$ci # -0.1 to 0.4 
M3_height_training <- log(M3_height_training) # TRANSFORMATION: ln

adf.test(M3_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M3_height_training)
plot(decomp)
diff(range(M3_height_training)) # 1.659503
diff(range(decomp$trend,na.rm=T)) # 0.8443547
diff(range(decomp$seasonal,na.rm=T)) # 0.7258103
diff(range(decomp$random,na.rm=T)) # 1.897923
# the most expressive trend is the randomic, but overall and seasonal trends affect the series greatly


ACF <- acf(M3_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M3_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M3_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(3), AR(10),...


eacf(M3_height_training) # ARMA(2,1), AR(5)
subs <- armasubsets(M3_height_training,nar=12,nma=12) 
plot(subs) # AR(10)...


# Seasonal analysis:

sdiff <- diff(M3_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # No model
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12

# Trying seasonal modelling:

model3 <- arima(M3_height_training, order = c(1,0,1), seasonal = list(order=c(1,1,0),period=12)) # ARIMA (1,0,1) x (1,1,0)12
model3 #AIC = 81.45
tsdiag(model3) # This looks reasonable -> The timeplot of the residuals looks relatively good and between -4 (outlier) and 3;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model3)) # residuals approx. normally distributed
qqnorm(rstandard(model3)); qqline(rstandard(model3)) # does look approx. normal except for an outlier
shapiro.test(rstandard(model3)) #p-value < 0.05 -> non-normally distributed, but probably because of an outlier


# Prediction:
plot(model3, n.ahead = 24, transform = function(x){exp(x)}, col = "blue", 
     main = "Wave height prediction - M3", ylab = "Height (m)")
lines(M3_height_test, col='red', type = 'o') # the test set
legend(2002,10, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,1) x (1,1,0)12", side=3)


# Error:
model3.p <- predict(model3, n.ahead=12) # the predicted values
model3.p$pred <- exp(model3.p$pred)
model3.e <- sqrt(mean((model3.p$pred - M3_height_test)^2)) # RMSE = 1.096689


# M3: Looks ok, but not so good. RESIDUALS ARE NON-NORMALLY DISTRIBUTED (outlier).



### TSA wave height - M4 ########################################################################################################

M4 <- M4 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M4_t <- M4 %>% group_by(Month_Yr) %>% summarise(waveheight  = mean(waveheight)) %>% ungroup() # mean period per month-year
M4_t <- tibble(Month_Yr = unique(M4$Month_Yr), waveheight  = M4_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2003-04-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M4_t <- left_join(Date_full, M4_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M4_t <- na_interpolation(M4_t$waveheight, option = "stine") # replace NA values with values from spline interpolation
M4_height <- ts(M4_t, start=c(2003, 4), end=c(2021, 9), frequency=12) # TS object



plot(M4_height, main = "M4 wave height")
points(x=as.vector(time(M4_height)), y=as.vector(M4_height),
       pch=as.vector(season(M4_height)), cex=0.6, col=4)

M4_height_training <- ts(M4_t[1:209], start=c(2003, 4), end=c(2020, 9), frequency=12) # training set
M4_height_test <- ts(M4_t[210:222], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M4_height_training) 
boxcox$mle # 0.2
boxcox$ci # -0.1 to 0.4
M4_height_training <- log(M4_height_training) # TRANSFORMATION: ln (0 inside the CI)

adf.test(M4_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M4_height_training)
plot(decomp)
diff(range(M4_height_training)) # 1.661198
diff(range(decomp$trend,na.rm=T)) # 0.9406221
diff(range(decomp$seasonal,na.rm=T)) # 0.5934214
diff(range(decomp$random,na.rm=T)) # 0.9589386
# the most expressive trend is the randomic, but seasonal and overall trends affect the series greatly


ACF <- acf(M4_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M4_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M4_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(3), AR(7)


eacf(M4_height_training) # ARMA(2,1), MA(4), ...
subs <- armasubsets(M4_height_training,nar=12,nma=12) 
plot(subs) # AR(9)...


# Seasonal analysis:

sdiff <- diff(M4_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # No model, SAR suggestion
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12, SAR(13)12,...

# Trying seasonal modelling:

model4 <- arima(M4_height_training, order = c(2,0,1), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (2,0,1) x (1,1,1)12
model4 #AIC = -68.57
tsdiag(model4) # Nice plots -> The timeplot of the residuals looks relatively good and between -3 and 2;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(model4)) # residuals not normally distributed (may have outliers)
qqnorm(rstandard(model4)); qqline(rstandard(model4)) # does not look normal
shapiro.test(rstandard(model4)) #p-value < 0.05 -> non-normally distributed


# Prediction:
plot(model4, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave height prediction - M4", ylab = "Height (m)")
lines(M4_height_test, col='red', type = 'o') # the test set
legend(2003,6, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (2,0,1) x (1,1,1)12", side=3)


# Error:
model4.p <- predict(model4, n.ahead=12) # the predicted values
model4.p$pred <- exp(model4.p$pred)
model4.e <- sqrt(mean((model4.p$pred - M4_height_test)^2)) # RMSE = 1.290442


# M4: Doesn't look good, and residuals are non-normally distributed

### TSA wave height - M5 ########################################################################################################

M5 <- M5 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M5_t <- M5 %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
M5_t <- tibble(Month_Yr = unique(M5$Month_Yr), waveheight = M5_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2004-10-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M5_t <- left_join(Date_full, M5_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M5_t <- na_interpolation(M5_t$waveheight, option = "stine") # replace NA values with values from spline interpolation
M5_height <- ts(M5_t, start=c(2004, 10), end=c(2021, 9), frequency=12) # TS object



plot(M5_height, main = "M5 wave height")
points(x=as.vector(time(M5_height)), y=as.vector(M5_height),
       pch=as.vector(season(M5_height)), cex=0.6, col=4)

M5_height_training <- ts(M5_t[1:182], start=c(2004, 10), end=c(2020, 9), frequency=12) # training set
M5_height_test <- ts(M5_t[183:195], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M5_height_training) 
boxcox$mle # 0.3
boxcox$ci # 0.0 to 0.5 
M5_height_training <- log(M5_height_training)
# TRANSFORMATION: LN (0 inside the CI)

adf.test(M5_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M5_height_training)
plot(decomp)
diff(range(M5_height_training)) # 1.739976
diff(range(decomp$trend,na.rm=T)) # 0.4693716
diff(range(decomp$seasonal,na.rm=T)) # 0.5823329
diff(range(decomp$random,na.rm=T)) # 1.472622
# the most expressive trend is the randomic, but seasonal and overall trends affect the series greatly


ACF <- acf(M5_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M5_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk, but close to be. IMA(12,1) suggestion

PACF <- pacf(M5_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(5), AR(10), AR(16)


eacf(M5_height_training) # AR(1), AR(5), ARMA(1,1), ARMA(2,1),...
subs <- armasubsets(M5_height_training,nar=12,nma=12) 
plot(subs) # AR(5)


# Seasonal analysis:

sdiff <- diff(M5_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SMA(1)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12

# Trying seasonal modelling:

model5 <- arima(M5_height_training, order = c(1,0,0), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (1,0,0) x (1,1,1)12
model5 #AIC = 35.89
tsdiag(model5) # Looks ok -> The timeplot of the residuals looks relatively good and between -4 and 2;
# ACF of residuals with only lag 0 significant, all p-values above 0.05 in Ljung-Box.
hist(rstandard(model5)) # residuals approx. normally distributed
qqnorm(rstandard(model5)); qqline(rstandard(model5)) # does look approx. normal, except for an outlier
shapiro.test(rstandard(model5)) #p-value < 0.05 -> non-normally distributed, but probably because of one outlier


# Prediction:
plot(model5, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave height prediction - M5", ylab = "Height (m)")
lines(M5_height_test, col='red', type = 'o') # the test set
legend(2005,4, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,0) x (1,1,1)12", side=3)


# Error:
model5.p <- predict(model5, n.ahead=12) # the predicted values
model5.p$pred <- exp(model5.p$pred) # transform predictions
model5.e <- sqrt(mean((model5.p$pred - M5_height_test)^2)) # RMSE = 0.7159866



# M5: Looks ok, with some deviations


### TSA wave height - M6 ########################################################################################################

M6 <- M6 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
M6_t <- M6 %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
M6_t <- tibble(Month_Yr = unique(M6$Month_Yr), waveheight = M6_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2006-09-01"), 
                                                   as.Date("2021-09-01"), by = "months")), "%Y-%m")) # Create a vector with all months
M6_t <- left_join(Date_full, M6_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
M6_t <- na_interpolation(M6_t$waveheight, option = "stine") # replace NA values with values from spline interpolation
M6_height <- ts(M6_t, start=c(2006, 9), end=c(2021, 9), frequency=12) # TS object



plot(M6_height, main = "M6 wave height")
points(x=as.vector(time(M6_height)), y=as.vector(M6_height),
       pch=as.vector(season(M6_height)), cex=0.6, col=4)

M6_height_training <- ts(M6_t[1:168], start=c(2006, 9), end=c(2020, 9), frequency=12) # training set
M6_height_test <- ts(M6_t[169:181], start=c(2020, 10), end=c(2021, 9), frequency=12) # test set


boxcox <- BoxCox.ar(M6_height_training) 
boxcox$mle # 0
boxcox$ci # -0.2 to 0.3 
M6_height_training <- log(M6_height_training)
# TRANSFORMATION: LN (0 inside the CI)

adf.test(M6_height_training) # p-value = 0.01 -> stationary


decomp <- decompose(M6_height_training)
plot(decomp)
diff(range(M6_height_training)) # 1.404023
diff(range(decomp$trend,na.rm=T)) # 0.3364596
diff(range(decomp$seasonal,na.rm=T)) # 0.7600349
diff(range(decomp$random,na.rm=T)) # 0.7660207
# the most expressive trends are the seasonal and randomic, but overall trend also affects the series greatly


ACF <- acf(M6_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(M6_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) #not a random walk

PACF <- pacf(M6_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(5)


eacf(M6_height_training) # ARMA(2,1), MA(2),...
subs <- armasubsets(M6_height_training,nar=12,nma=12) 
plot(subs) # AR(5),...


# Seasonal analysis:

sdiff <- diff(M6_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # SMA(1)12
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# SAR(1)12

# Trying seasonal modelling:

model6 <- arima(M6_height_training, order = c(5,0,0), seasonal = list(order=c(1,1,1),period=12)) # ARIMA (5,0,0) x (1,1,1)12
model6 #AIC = -83.13
tsdiag(model6) # Ok plots -> The timeplot of the residuals looks relatively good and between -3 and 2;
# ACF of residuals with only lag 0 significant, all p-values above or on 0.05 in Ljung-Box.
hist(rstandard(model6)) # residuals approx. normally distributed
qqnorm(rstandard(model6)); qqline(rstandard(model6)) # does look approx. normal
shapiro.test(rstandard(model6)) #p-value >> 0.05 -> normally distributed


# Prediction:
plot(model6, n.ahead = 24, transform=function(x){exp(x)}, col = "blue", 
     main = "Wave height prediction - M6", ylab = "Height (m)")
lines(M6_height_test, col='red', type = 'o') # the test set
legend(2007,7, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (5,0,0) x (1,1,1)12", side=3)


# Error:
model6.p <- predict(model6, n.ahead=12) # the predicted values
model6.p$pred <- exp(model6.p$pred) # transform predictions
model6.e <- sqrt(mean((model6.p$pred - M6_height_test)^2)) # RMSE= 0.8281802


# M6: Looks ok, with some deviations


### TSA wave height - FS1 ########################################################################################################

FS1 <- FS1 %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
FS1_t <- FS1 %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
FS1_t <- tibble(Month_Yr = unique(FS1$Month_Yr), waveheight = FS1_t$waveheight)
Date_full <- tibble(date = format(as.Date(seq.Date(as.Date("2003-01-01"), 
                                                   as.Date("2008-02-01"), by = "months")), "%Y-%m")) # Create a vector with all months
FS1_t <- left_join(Date_full, FS1_t, by=c("date" = "Month_Yr")) # Bind dates with existent period values
FS1_t <- na_interpolation(FS1_t$waveheight, option = "stine") # replace NA values with values from spline interpolation
FS1_height <- ts(FS1_t, start=c(2003, 1), end=c(2008, 2), frequency=12) # TS object



plot(FS1_height, main = "FS1 wave height") # bad values at the end of the TS -> cut off last 2y
points(x=as.vector(time(FS1_height)), y=as.vector(FS1_height),
       pch=as.vector(season(FS1_height)), cex=0.6, col=4) 

FS1_height_training <- ts(FS1_t[1:36], start=c(2003, 1), end=c(2006, 12), frequency=12) # training set
FS1_height_test <- ts(FS1_t[37:44], start=c(2007, 1), end=c(2007, 8), frequency=12) # test set


boxcox <- BoxCox.ar(FS1_height_training) 
boxcox$mle # 0.9
boxcox$ci # -0.2 to 1.7
# TRANSFORMATION: NONE (1 inside the CI)

adf.test(FS1_height_training) # p-value = 0.03821 (<0.05) -> stationary


decomp <- decompose(FS1_height_training)
plot(decomp)
diff(range(FS1_height_training)) # 2.033858
diff(range(decomp$trend,na.rm=T)) # 0.1667434
diff(range(decomp$seasonal,na.rm=T)) # 1.529952
diff(range(decomp$random,na.rm=T)) # 1.073466
# the most expressive trend is the seasonal, but randomic trend affects the series greatly


ACF <- acf(FS1_height_training, plot = F)
ACF$lag=ACF$lag*12
plot(ACF) #not a white noise. Many significant lags, AR suggestion
ACF_diff <- acf(diff(FS1_height_training), plot = F) # difference once and check if the TS is a random walk
ACF_diff$lag=ACF_diff$lag*12
plot(ACF_diff) # Random walk! 

PACF <- pacf(FS1_height_training, plot = F) 
PACF$lag=PACF$lag*12
plot(PACF) #AR(1), AR(6)


eacf(FS1_height_training) # AR(1), MA(1), ARMA(1,1), ...
subs <- armasubsets(FS1_height_training,nar=12,nma=12) 
plot(subs) # AR(8)


# Seasonal analysis:

sdiff <- diff(FS1_height_training,lag=12)
ACF_seasonal <- acf(sdiff,lag.max = 36, plot = F)
ACF_seasonal$lag <- ACF_seasonal$lag *12
plot(ACF_seasonal) # no seasonality
PACF_seasonal <- pacf(sdiff, plot = F)
PACF_seasonal$lag <- PACF_seasonal$lag *12
plot(PACF_seasonal)# no seasonality

# Trying seasonal modelling:

modelFS1 <- arima(FS1_height_training, order = c(1,0,1), seasonal = list(order=c(1,1,0),period=12)) # ARIMA (1,0,1) x (1,1,0)12
modelFS1 #AIC = 48.35
tsdiag(modelFS1) # Nice plots -> The timeplot of the residuals looks relatively good and between -1 and 2;
# ACF of residuals with only lag 0 significant, all p-values much above 0.05 in Ljung-Box.
hist(rstandard(modelFS1)) # residuals do not look normally distributed
qqnorm(rstandard(modelFS1)); qqline(rstandard(modelFS1)) # does not look normal
shapiro.test(rstandard(modelFS1)) #p-value > 0.05 -> normally distributed


# Prediction:
plot(modelFS1, n.ahead = 24, col = "blue", 
     main = "Wave height prediction - FS1", ylab = "Height (m)")
lines(FS1_height_test, col='red', type = 'o') # the test set
legend(2003,3.5, legend=c("Observed", "Prediction", "Observed", "95% CI"),
       col=c("black","black", "red", "blue"), lty=c(1,2,1,2), cex=0.8)
mtext("ARIMA (1,0,1) x (1,1,0)12", side=3)


# Error:
modelFS1.p <- predict(modelFS1, n.ahead=8) # the predicted values
modelFS1.e <- sqrt(mean((modelFS1.p$pred - FS1_height_test)^2)) # RSME = 0.2636708


# FS1: Doesn't look so bad, and real values are inside CI


### TSA wave height- Belmullet ########################################################################################################

Belmullet <- Belmullet %>% mutate(Month_Yr = format(as.Date(time), "%Y-%m")) # create a column with the month and year of each observation
Belmullet_t <- Belmullet %>% group_by(Month_Yr) %>% summarise(waveheight = mean(waveheight)) %>% ungroup() # mean period per month-year
Belmullet_t <- tibble(Month_Yr = unique(Belmullet$Month_Yr), waveheight = Belmullet_t$waveheight)
# Too few observations! Only 5... 


### Table errors of predictions - period and height ###################################################################################

Errors_height <- tibble(Station = c("M1", "M2", "M3", "M4", "M5", "M6", "FS1", "Belmullet"),
                        RMSE_height = c(model1.e, model2.e, model3.e, model4.e, model5.e, model6.e, modelFS1.e, "NA"))


Errors_df <- tibble(Station = Errors_height$Station, RMSE_height = Errors_height$RMSE_height, RMSE_period = Errors_period$RMSE_period)

Errors_df %>% knitr::kable(caption = "Prediction errors - ARIMA") %>% 
        kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, font_size = 15) 
