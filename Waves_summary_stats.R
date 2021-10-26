##########################################
# Project: CRT Group Project - Waves  
# Purpose: Summary stats - hourly data
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

data_h <- read.csv("bouy_data_hourly_mean.csv") # hourly data



### Initial exploration ##############################################################################################################

# Missing data:

data_h %>% count(station_id) # what data do we have for each station
nr <- nrow(data_h) # note the number of rows

missing_ness <- # create a df of the number of missing value per col by:
  data_h %>% # take the data frame
  summarise_all(~ sum(is.na(.))) %>% # counting up the number of NA values in each col
  t() %>% # transpose the matrix
  as.data.frame() %>% # and do some house keeping
  rownames_to_column() %>% 
  as_tibble()

missing_ness <- missing_ness %>%
  rename(number_missing = V1, Variable = rowname) %>% # rename col to something useful and
  mutate(prop_missing = round(number_missing/nr, 2)) # add the prop of missing rows to data set 


missing_ness %>% knitr::kable(caption = "Missing data (altogether)") %>% kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, font_size = 15) 


# Create summary statistics for wave height, wave period, and hmax:
df <- tibble(Waveheight = data_h$waveheight, Waveperiod = data_h$waveperiod, Wavehmax = data_h$hmax)

df.sum <- df %>%
  summarise_all(list(min = ~min(., na.rm = TRUE), 
                     q25 = ~quantile(., 0.25, na.rm = TRUE), 
                     median = ~median(., na.rm = TRUE), 
                     q75 = ~quantile(., 0.75, na.rm = TRUE), 
                     max = ~max(., na.rm = TRUE),
                     mean = ~mean(., na.rm = TRUE), 
                     sd = ~sd(., na.rm = TRUE)))

# Reshape the output for presentation in a table
df.sum %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd) %>%
  knitr::kable(caption = "Summary stats") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, font_size = 15) 


# pairs plot wave height x wave period x wind speed x wave hmax
df_simple <- tibble(Waveheight = data_h$waveheight, Waveperiod = data_h$waveperiod, Wavehmax = data_h$hmax,
                    WindSpeed = data_h$windspeed)

GGally::ggpairs(df_simple, na.rm = TRUE)

# histograms wave height:
par(mfrow=c(2,4))
hist(M1_h$waveheight)
hist(M2_h$waveheight)
hist(M3_h$waveheight)
hist(M4_h$waveheight)
hist(M5_h$waveheight)
hist(M6_h$waveheight)
hist(FS1_h$waveheight)
hist(Belmullet_h$waveheight)
par(mfrow=c(1,1))


### Stations data frames #############################################################################################################

unique(data_h$station_id) # M1, M2, M3, FS1, M4-Archive, M5, M6, M4, Belmullet-AMETS

M1_h <- data_h %>% filter(station_id == "M1")
M2_h <- data_h %>% filter(station_id == "M2")
M3_h <- data_h %>% filter(station_id == "M3")
M4_h <- data_h %>% filter(station_id == "M4" | station_id == "M4-Archive")
M5_h <- data_h %>% filter(station_id == "M5")
M6_h <- data_h %>% filter(station_id == "M6")
FS1_h <- data_h %>% filter(station_id == "FS1")
Belmullet_h <- data_h %>% filter(station_id == "Belmullet-AMETS")



####### Missing data - Wave height: ###########################################################################

# M1:
nr_M1 <- nrow(M1_h)
M1_height_missing <- tibble(missing = sum(is.na(M1_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M1, accuracy=0.001))

# M2:
nr_M2 <- nrow(M2_h)
M2_height_missing <- tibble(missing = sum(is.na(M2_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M2, accuracy=0.001))

# M3:
nr_M3 <- nrow(M3_h)
M3_height_missing <- tibble(missing = sum(is.na(M3_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M3, accuracy=0.001))

# M4:
nr_M4 <- nrow(M4_h)
M4_height_missing <- tibble(missing = sum(is.na(M4_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M4, accuracy=0.001))

# M5:
nr_M5 <- nrow(M5_h)
M5_height_missing <- tibble(missing = sum(is.na(M5_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M5, accuracy=0.001))

# M6:
nr_M6 <- nrow(M6_h)
M6_height_missing <- tibble(missing = sum(is.na(M6_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M6, accuracy=0.001))

# FS1:
nr_FS1 <- nrow(FS1_h)
FS1_height_missing <- tibble(missing = sum(is.na(FS1_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_FS1, accuracy=0.001))

# Belmullet:
nr_Belmullet <- nrow(Belmullet_h)
Belmullet_height_missing <- tibble(missing = sum(is.na(Belmullet_h$waveheight))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_Belmullet, accuracy=0.001))

# Proportion to the whole data:
M1_prop <- scales::percent(nr_M1/nr, accuracy=0.001)
M2_prop <- scales::percent(nr_M2/nr, accuracy=0.001)
M3_prop <- scales::percent(nr_M3/nr, accuracy=0.001)
M4_prop <- scales::percent(nr_M4/nr, accuracy=0.001)
M5_prop <- scales::percent(nr_M5/nr, accuracy=0.001)
M6_prop <- scales::percent(nr_M6/nr, accuracy=0.001)
FS1_prop <- scales::percent(nr_FS1/nr, accuracy=0.001)
Belmullet_prop <- scales::percent(nr_Belmullet/nr, accuracy=0.001)


####### Missing data - Wave period: ###########################################################################

# M1:
M1_period_missing <- tibble(missing = sum(is.na(M1_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M1, accuracy=0.001))

# M2:
M2_period_missing <- tibble(missing = sum(is.na(M2_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M2, accuracy=0.001))

# M3:
M3_period_missing <- tibble(missing = sum(is.na(M3_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M3, accuracy=0.001))

# M4:
M4_period_missing <- tibble(missing = sum(is.na(M4_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M4, accuracy=0.001))

# M5:
M5_period_missing <- tibble(missing = sum(is.na(M5_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M5, accuracy=0.001))

# M6:
M6_period_missing <- tibble(missing = sum(is.na(M6_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_M6, accuracy=0.001))

# FS1:
FS1_period_missing <- tibble(missing = sum(is.na(FS1_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_FS1, accuracy=0.001))

# Belmullet:
Belmullet_period_missing <- tibble(missing = sum(is.na(Belmullet_h$waveperiod))) %>% 
  mutate(prop_missing = scales::percent(missing/nr_Belmullet, accuracy=0.001))

##### Visualisation - missing data ######################################################################################################################

# Table:

missing_df <- tibble(Station = c("M1", "M2", "M3", "M4", "M5", "M6", "FS1", "Belmullet-AMETS"),
                                Proportion_data = c(M1_prop, M2_prop, M3_prop, M4_prop, M5_prop, M6_prop, FS1_prop, Belmullet_prop),
                                Missing_height = c(M1_height_missing$missing, M2_height_missing$missing, M3_height_missing$missing, 
                                            M4_height_missing$missing, M5_height_missing$missing, M6_height_missing$missing, 
                                            FS1_height_missing$missing, Belmullet_height_missing$missing),
                                Prop_missing_height = c(M1_height_missing$prop_missing, M2_height_missing$prop_missing, M3_height_missing$prop_missing, 
                                                       M4_height_missing$prop_missing, M5_height_missing$prop_missing, M6_height_missing$prop_missing, 
                                                       FS1_height_missing$prop_missing, Belmullet_height_missing$prop_missing),
                                Missing_period = c(M1_period_missing$missing, M2_period_missing$missing, M3_period_missing$missing, 
                                            M4_period_missing$missing, M5_period_missing$missing, M6_period_missing$missing, 
                                            FS1_period_missing$missing, Belmullet_period_missing$missing),
                                Prop_missing_period = c(M1_period_missing$prop_missing, M2_period_missing$prop_missing, M3_period_missing$prop_missing, 
                                                       M4_period_missing$prop_missing, M5_period_missing$prop_missing, M6_period_missing$prop_missing, 
                                                       FS1_period_missing$prop_missing, Belmullet_period_missing$prop_missing))


missing_df %>% knitr::kable(caption = "Missing data - wave height and wave period") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, font_size = 15)


# Time distribution of missing data:
# Wave height:

M1_h <- M1_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M1_t <- aggregate(waveheight ~ Month_Yr, data=M1_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M1_missing_height_t <- ts(M1_t$waveheight,start=c(2001, 2), end=c(2007, 7), frequency=12) # TS object

M2_h <- M2_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M2_t <- aggregate(waveheight ~ Month_Yr, data=M2_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M2_missing_height_t <- ts(M2_t$waveheight,start=c(2001, 5), end=c(2021, 9), frequency=12) # TS object

M3_h <- M3_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M3_t <- aggregate(waveheight ~ Month_Yr, data=M3_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M3_missing_height_t <- ts(M3_t$waveheight,start=c(2002, 7), end=c(2021, 9), frequency=12) # TS object

M4_h <- M4_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M4_t <- aggregate(waveheight ~ Month_Yr, data=M4_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M4_missing_height_t <- ts(M4_t$waveheight,start=c(2003, 4), end=c(2021, 9), frequency=12) # TS object

M5_h <- M5_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M5_t <- aggregate(waveheight ~ Month_Yr, data=M5_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M5_missing_height_t <- ts(M5_t$waveheight,start=c(2004, 10), end=c(2021, 9), frequency=12) # TS object

M6_h <- M6_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
M6_t <- aggregate(waveheight ~ Month_Yr, data=M6_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M6_missing_height_t <- ts(M6_t$waveheight,start=c(2006, 9), end=c(2021, 9), frequency=12) # TS object

FS1_h <- FS1_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
FS1_t <- aggregate(waveheight ~ Month_Yr, data=FS1_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
FS1_missing_height_t <- ts(FS1_t$waveheight,start=c(2003, 1), end=c(2008, 2), frequency=12) # TS object

Belmullet_h <- Belmullet_h %>% mutate(Month_Yr = format(as.Date(date), "%Y-%m")) # create a column with the month and year of each observation
Belmullet_t <- aggregate(waveheight ~ Month_Yr, data=Belmullet_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
Belmullet_missing_height_t <- ts(Belmullet_t$waveheight,start=c(2011, 4), end=c(2011, 8), frequency=12) # TS object





par(mfrow=c(2,4))

plot(M1_missing_height_t, main = "M1")
points(x=as.vector(time(M1_missing_height_t)), y=as.vector(M1_missing_height_t),
       pch=as.vector(season(M1_missing_height_t)), cex=0.6, col=4)

plot(M2_missing_height_t, main = "M2")
points(x=as.vector(time(M2_missing_height_t)), y=as.vector(M2_missing_height_t),
       pch=as.vector(season(M2_missing_height_t)), cex=0.6, col=4)

plot(M3_missing_height_t, main = "M3")
points(x=as.vector(time(M3_missing_height_t)), y=as.vector(M3_missing_height_t),
       pch=as.vector(season(M3_missing_height_t)), cex=0.6, col=4)

plot(M4_missing_height_t, main = "M4")
points(x=as.vector(time(M4_missing_height_t)), y=as.vector(M4_missing_height_t),
       pch=as.vector(season(M4_missing_height_t)), cex=0.6, col=4)

plot(M5_missing_height_t, main = "M5")
points(x=as.vector(time(M5_missing_height_t)), y=as.vector(M5_missing_height_t),
       pch=as.vector(season(M5_missing_height_t)), cex=0.6, col=4)

plot(M6_missing_height_t, main = "M6")
points(x=as.vector(time(M6_missing_height_t)), y=as.vector(M6_missing_height_t),
       pch=as.vector(season(M6_missing_height_t)), cex=0.6, col=4)

plot(FS1_missing_height_t, main = "FS1")
points(x=as.vector(time(FS1_missing_height_t)), y=as.vector(FS1_missing_height_t),
       pch=as.vector(season(FS1_missing_height_t)), cex=0.6, col=4)

plot(Belmullet_missing_height_t, main = "Belmullet-AMETS")
points(x=as.vector(time(Belmullet_missing_height_t)), y=as.vector(Belmullet_missing_height_t),
       pch=as.vector(season(Belmullet_missing_height_t)), cex=0.6, col=4)

mtext("Missing wave height - hourly data", side = 3, line = -1.5, outer = TRUE)



# Wave period:

M1_t <- aggregate(waveperiod ~ Month_Yr, data=M1_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M1_missing_period_t <- ts(M1_t$waveperiod,start=c(2001, 2), end=c(2007, 7), frequency=12) # TS object

M2_t <- aggregate(waveperiod ~ Month_Yr, data=M2_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M2_missing_period_t <- ts(M2_t$waveperiod,start=c(2001, 5), end=c(2021, 9), frequency=12) # TS object

M3_t <- aggregate(waveperiod ~ Month_Yr, data=M3_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M3_missing_period_t <- ts(M3_t$waveperiod,start=c(2002, 7), end=c(2021, 9), frequency=12) # TS object

M4_t <- aggregate(waveperiod ~ Month_Yr, data=M4_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M4_missing_period_t <- ts(M4_t$waveperiod,start=c(2003, 4), end=c(2021, 9), frequency=12) # TS object

M5_t <- aggregate(waveperiod ~ Month_Yr, data=M5_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M5_missing_period_t <- ts(M5_t$waveperiod,start=c(2004, 10), end=c(2021, 9), frequency=12) # TS object

M6_t <- aggregate(waveperiod ~ Month_Yr, data=M6_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
M6_missing_period_t <- ts(M6_t$waveperiod,start=c(2006, 9), end=c(2021, 9), frequency=12) # TS object

FS1_t <- aggregate(waveperiod ~ Month_Yr, data=FS1_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
FS1_missing_period_t <- ts(FS1_t$waveperiod,start=c(2003, 1), end=c(2008, 2), frequency=12) # TS object

Belmullet_t <- aggregate(waveperiod ~ Month_Yr, data=Belmullet_h, function(x) {sum(is.na(x))}, na.action = NULL) # sum NA per month-year
Belmullet_missing_period_t <- ts(Belmullet_t$waveperiod,start=c(2011, 4), end=c(2011, 8), frequency=12) # TS object



par(mfrow=c(2,4))

plot(M1_missing_period_t, main = "M1")

plot(M2_missing_period_t, main = "M2")

plot(M3_missing_period_t, main = "M3")

plot(M4_missing_period, main = "M4")

plot(M5_missing_period_t, main = "M5")

plot(M6_missing_period_t, main = "M6")

plot(FS1_missing_period_t, main = "FS1")

plot(Belmullet_missing_period_t, main = "Belmullet-AMETS")

mtext("Missing wave period - hourly data", side = 3, line = -1.5, outer = TRUE)
par(mfrow=c(1,1))

######## Calculating wave length ###############################################################################

# 1st step: use deep wave equation

M1_h <- M1_h %>% mutate(wavelength_deep = 9.81 * M1_h$waveperiod^2 / (2*pi)) # create column wave length deep water
M2_h <- M2_h %>% mutate(wavelength_deep = 9.81 * M2_h$waveperiod^2 / (2*pi))
M3_h <- M3_h %>% mutate(wavelength_deep = 9.81 * M3_h$waveperiod^2 / (2*pi))
M4_h <- M4_h %>% mutate(wavelength_deep = 9.81 * M4_h$waveperiod^2 / (2*pi))
M5_h <- M5_h %>% mutate(wavelength_deep = 9.81 * M5_h$waveperiod^2 / (2*pi))
M6_h <- M6_h %>% mutate(wavelength_deep = 9.81 * M6_h$waveperiod^2 / (2*pi))
Belmullet_h <- Belmullet_h %>% mutate(wavelength_deep = 9.81 * Belmullet_h$waveperiod^2 / (2*pi))
FS1_h <- FS1_h %>% mutate(wavelength_deep = 9.81 * FS1_h$waveperiod^2 / (2*pi))

# 2nd step: d/lambda
M1_d_lambda <- 128/mean(M1_h$wavelength_deep, na.rm = TRUE) # the water depth of the site divided by mean(wavelength deep)
M2_d_lambda <- 107/mean(M2_h$wavelength_deep, na.rm = TRUE)
M3_d_lambda <- 168/mean(M3_h$wavelength_deep, na.rm = TRUE)
M4_d_lambda <- 88/mean(M4_h$wavelength_deep, na.rm = TRUE)
M5_d_lambda <- 30/mean(M5_h$wavelength_deep, na.rm = TRUE)
M6_d_lambda <- 3220/mean(M6_h$wavelength_deep, na.rm = TRUE)
Belmullet_d_lambda <- 57/mean(Belmullet_h$wavelength_deep, na.rm = TRUE)
FS1_d_lambda <- 68/mean(FS1_h$wavelength_deep, na.rm = TRUE)

# Table of results:
deep_water <- tibble(Station = c("M1", "M2", "M3", "M4", "M5", "M6", "FS1", "Belmullet-AMETS"),
                     Wavelength_deep = c(round(mean(M1_h$wavelength_deep, na.rm = TRUE),3), round(mean(M2_h$wavelength_deep, na.rm = TRUE),3),
                                      round(mean(M3_h$wavelength_deep, na.rm = TRUE),3), round(mean(M4_h$wavelength_deep, na.rm = TRUE),3),
                                      round(mean(M5_h$wavelength_deep, na.rm = TRUE),3), round(mean(M6_h$wavelength_deep, na.rm = TRUE),3),
                                      round(mean(Belmullet_h$wavelength_deep, na.rm = TRUE),3), round(mean(FS1_h$wavelength_deep, na.rm = TRUE),3)),
                     Rational = c(round(M1_d_lambda,3), round(M2_d_lambda,3), round(M3_d_lambda,3), round(M4_d_lambda,3), round(M5_d_lambda,3), round(M6_d_lambda,3),
                                  round(FS1_d_lambda,3), round(Belmullet_d_lambda,3)),
                     Water_depth = ifelse(Rational >= 0.5, "Deep water", ifelse(Rational <= 0.05, "Shallow water", "Intermediate depth")))


deep_water %>% knitr::kable(caption = "Water depth - hourly data computation") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, font_size = 15)






