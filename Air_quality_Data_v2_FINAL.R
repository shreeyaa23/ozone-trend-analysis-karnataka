## Project Air quality Data - GROUP 8

# This dataset contains air quality measurements recorded in different cities of Karnataka, 
# including pollutant levels such as Ozone, CO, SO2, NO2, PM10, and PM2.5 over different time periods. 
# It also provides station information and dates for each recorded observation
# The dataset covers the air quality data from August 1, 2021, to July 31, 2023

# Load required libraries
library(dplyr)
library(tsibble)
library(ggplot2)
library(feasts)
library(lubridate)
library(fpp3)
library(broom)
library(tidyverse)
library(fable)

# Load the dataset
air_quality_data <- read.csv("G:\\VCU\\FM Midterm\\PROJECT\\Cleaned_Karnataka_Air_Quality_Dataset.csv")

# Convert 'From Date' to date format and convert to yearmonth
air_quality_data <- air_quality_data %>% 
  mutate(Date = as.Date(From.Date, format = "%d-%m-%Y %H:%M")) %>%
  mutate(Month = yearmonth(Date)) %>%
  select(Date, Month, Ozone)

# Remove duplicate rows to ensure distinct rows for tsibble conversion
air_quality_data <- air_quality_data %>% distinct()

# Add a unique key for each row to ensure tsibble compatibility
air_quality_data <- air_quality_data %>% mutate(record_id = row_number())

# Convert the dataset to a tsibble
air_quality_tsibble <- air_quality_data %>%
  as_tsibble(index = Date, key = record_id)

# Partition the data into training and testing sets
# Keeping the years up to 2022 as the training set, the rest as testing
train_data <- air_quality_tsibble %>%
  filter(year(Month) <= 2022)

# Visualize the training data

# Create a time series plot, grouping by Month to avoid single observation per group issue
train_data %>%
  group_by(Month) %>%
  summarise(Ozone = mean(Ozone, na.rm = TRUE)) %>%
  autoplot(Ozone) + 
  ggtitle("Time Series Plot of Ozone Levels in Karnataka") +
  xlab("Month") +
  ylab("Ozone")

# Create a seasonal plot
train_data %>%
  group_by(Month) %>%
  summarise(Ozone = mean(Ozone, na.rm = TRUE)) %>%
  gg_season(Ozone, period = "month") +
  ggtitle("Seasonal Plot of Ozone Levels in Karnataka") +
  xlab("Month") +
  ylab("Ozone")

# Note: Sub-series plots require data with sufficient periodicity to show trends across individual periods. 
# Given the limitations of this dataset, generating a meaningful sub-series plot may not be feasible.
# Removing sub-series plot to avoid misleading or empty visualizations.

# Fit a regression model to the training data 
fit_ozone <- train_data %>%
  index_by(Month) %>%
  summarise(Ozone = mean(Ozone, na.rm = TRUE)) %>%
  model(TSLM(Ozone ~ trend() + season()))

# Report on the fitted model
fit_ozone %>% report()
# Create a plot to compare the fit to the training data
fit_ozone %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Ozone, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  ggtitle("Comparison of Fitted vs Actual Ozone Levels in Karnataka") +
  xlab("Month") +
  ylab("Ozone") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Fitted" = "red"))

# Create a plot to show the forecast and prediction interval
forecast_ozone <- fit_ozone %>% forecast(h = "12 months")

# Generate a plot to show the forecast along with prediction intervals
autoplot(forecast_ozone, train_data_monthly) +
  ggtitle("Forecast of Ozone Levels in Karnataka with Prediction Interval") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

# Fit an ARIMA model by following the iterative procedure to find the best ARIMA model for this time series
# Determine the level of differencing needed to make the data stationary
train_data_monthly %>% features(Ozone, unitroot_nsdiffs) #D=0

# Determine the order of differencing required for ARIMA model
d_value <- train_data_monthly %>% features(Ozone, unitroot_ndiffs)
d_value #d = 0

# Fit an ARIMA model with d=0 and D=0
arima_fit_0 <- train_data_monthly %>%
  model(ARIMA(Ozone ~ pdq(0, 0, 0) + PDQ(0, 0, 0)))

# Report on the fitted ARIMA model
report(arima_fit_0) #AIC=106.71

# Plot residuals of the ARIMA model to check for stationarity and patterns
arima_fit_0 %>%
  augment() %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("Residual Diagnostics for ARIMA Model")

#Significant spikes are observed at lag 1 in ACF.
#There are smaller but notable spikes at lags 4, 5, and 6.The spike at lag 1 is the most significant.
#To keep the model parsimonious and avoid overfitting, I suggest starting with a q value of 1

#Significant spikes at lag 1 in the PACF.
#This indicates that a single autoregressive term should be used to model these dependencies.Therefore, p = 1.

#-----------------
# Fit an ARIMA model with p=1, d=0, q=0, D=0
arima_fit_A <- train_data_monthly %>%
  model(ARIMA(Ozone ~ pdq(1, 0, 0) + PDQ(0, 0, 0)))
# Report on the fitted ARIMA model
report(arima_fit_A) #AIC= 98.71

arima_fit_B <- train_data_monthly %>%
  model(ARIMA(Ozone ~ pdq(0, 0, 1) + PDQ(0, 0, 0)))
# Report on the fitted ARIMA model
report(arima_fit_B) #AIC= 102.19   

arima_fit_C <- train_data_monthly %>%
  model(ARIMA(Ozone ~ pdq(1, 0, 1) + PDQ(0, 0, 0)))
# Report on the fitted ARIMA model
report(arima_fit_C) #AIC= 100.46   

# Plot residuals of the ARIMA model to check for stationarity and patterns
arima_fit_A %>%
  augment() %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("Residual Diagnostics for ARIMAA Model")
#-------------------

# Create a plot to compare the fit to the training data without showing the validation data or forecast
arima_fit_A %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Ozone, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval
forecast_arima_A <- arima_fit_A %>% forecast(h = 12)

autoplot(forecast_arima_A, train_data_monthly) +
  ggtitle("Forecast of Ozone Levels in Karnataka with Prediction Interval (ARIMA Model)") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

# Use auto.arima() to fit an ARIMA (p, d, q) (P, D, Q) model to the training data
arima_auto_fit <- train_data_monthly %>%
  model(auto = ARIMA(Ozone))

# Report on the fitted auto ARIMA model
report(arima_auto_fit) #(1,0,0) #AIC=98.71
# This is an ARIMA(1,0,0). #the seasonal components (P, D, Q) were not needed or not significant, resulting in a model without a seasonal component.

# Create a plot to compare the fit to the training data without showing the validation data or forecast
arima_auto_fit %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Ozone, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval
forecast_auto_arima <- arima_auto_fit %>% forecast(h = 12)

autoplot(forecast_auto_arima, train_data_monthly) +
  ggtitle("Forecast of Ozone Levels in Karnataka with Prediction Interval (Auto ARIMA Model)") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

## Fit an exponential smoothing model - ETS
# Fit your recommended ETS model(s)
ets_manual1 <- train_data_monthly %>% model(ETS(Ozone ~ error("A") + trend("N") + season("N")))
# Report on the fitted ETS model
report(ets_manual1) #AIC: 99.72092

ets_manual2 <- train_data_monthly %>% model(ETS(Ozone ~ error("A") + trend("N") + season("A")))
# Report on the fitted ETS model
report(ets_manual2) #AIC: 85.66484

ets_manual3 <- train_data_monthly %>% model(ETS(Ozone ~ error("A") + trend("N") + season("M")))
# Report on the fitted ETS model
report(ets_manual3) #AIC: 86.35142

# ETS model 2 has the lowest AIC.
# Create a plot to compare the fit to the training data without showing the validation data or the forecast
ets_manual2 %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Ozone, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval
forecast_ets_manual2 <- ets_manual2 %>% forecast(h = 12)

autoplot(forecast_ets_manual2, train_data_monthly) +
  ggtitle("Forecast of Ozone Levels in Karnataka with Prediction Interval (ETS Model)") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

# Fit an ETS model allowing the algorithm to choose the structure for error, trend, and seasonality from the training data
ets_auto <- train_data_monthly %>% model(ETS(Ozone))

# Report on the fitted auto ETS model
report(ets_auto) #AIC 99.72092 (A,N,N) 

# Create a plot to compare the fit to the training data without showing the validation data or forecast
ets_auto %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Ozone, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval
forecast_ets_auto <- ets_auto %>% forecast(h = 12)

autoplot(forecast_ets_auto, train_data_monthly) +
  ggtitle("Forecast of Ozone Levels in Karnataka with Prediction Interval (Auto ETS Model)") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()


## Assess the predictive accuracy of all the five models in cross-validation
all.models.fit <- train_data_monthly %>%
  model(
    arima_manual = ARIMA(Ozone ~ pdq(1,0,0) + PDQ(0,0,0)),
    arima_auto = ARIMA(Ozone),
    ts_reg = TSLM(Ozone ~ trend() + season()),
    ets_manual = ETS(Ozone ~ error("A") + trend("N") + season("A")),
    ets_auto = ETS(Ozone),
    naive = NAIVE(Ozone),
    snaive = SNAIVE(Ozone)
  )

# Forecast for 12 months
all.models.pred <- all.models.fit %>% forecast(h = 12)

all.models.pred %>% accuracy(air_quality_tsibble) %>% arrange(MAPE, decreasing = TRUE)

# Based on the lowest MAPE, the ARIMA manual model has the best performance, which is same as the ARIMA auto model because of same pdq values.

#---------------------
# Combine the best models into an ensemble

# Define the ensemble model with manual ARIMA and ETS
ensemble_model <- train_data_monthly %>%
  model(
    arima_manual = ARIMA(Ozone ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
    ets_manual = ETS(Ozone ~ error("A") + trend("N") + season("A")),
    #Regression = TSLM(Ozone ~ trend() + season())
  ) %>%
  mutate(Ensemble = (arima_manual + ets_manual) / 2)


#-----
#Ensemble model - include different weights for the ARIMA and ETS models using the inverse variance weighting approach (commonly known as "inv_var")
ensemble_model2 <- train_data_monthly %>%
  model(
    arima_manual = ARIMA(Ozone ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
    ets_manual = ETS(Ozone ~ error("A") + trend("N") + season("A")),
    # Regression = TSLM(Ozone ~ trend() + season())
    combination = combination_model(
      ARIMA(Ozone ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
      ETS(Ozone ~ error("A") + trend("N") + season("A")),
      cmbn_args = list(weights = "inv_var")
    )
  )

#-----

# Forecast for 12 months with the ensemble
all.models.pred <- ensemble_model %>% forecast(h = 12)

all.models.pred %>% accuracy(air_quality_tsibble) %>% arrange(MAPE, decreasing = TRUE)

# Forecast for 12 months with the ensemble - inv_var
all.models.pred <- ensemble_model2 %>% forecast(h = 12)

all.models.pred %>% accuracy(air_quality_tsibble) %>% arrange(MAPE, decreasing = TRUE)

#ensemble model with equal weights performs better than inv_Var.
# Forecast with the ensemble
ensemble_forecast <- ensemble_model %>% forecast(h = "12 months")

# Plot ensemble forecast
autoplot(ensemble_forecast, train_data_monthly) +
  ggtitle("Ensemble Forecast of Ozone Levels in Karnataka") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

#-------------------

# Log Transformation of Ozone Data
train_data_monthly <- train_data_monthly %>%
  mutate(Log_Ozone = log(Ozone + 1))  # Add 1 to handle zero values

# Visualize the log-transformed data
train_data_monthly %>%
  autoplot(Log_Ozone) +
  ggtitle("Log-Transformed Time Series of Ozone Levels in Karnataka") +
  xlab("Month") +
  ylab("Log(Ozone)")

# Fit a model on log-transformed data (ARIMA)
arima_log_fit <- train_data_monthly %>%
  model(ARIMA(Log_Ozone ~ pdq(1, 0, 0)))

# Forecast log-transformed data
forecast_log_arima <- arima_log_fit %>% forecast(h = "12 months")

# Plot log-transformed forecast
autoplot(forecast_log_arima, train_data_monthly) +
  ggtitle("Forecast of Log-Transformed Ozone Levels") +
  xlab("Month") +
  ylab("Log(Ozone)") +
  theme_minimal()

#Operational Forecast
# Remove duplicate rows to ensure distinct rows for tsibble conversion
air_quality_data <- air_quality_data %>% distinct()

# Add a unique key for each row to ensure tsibble compatibility
air_quality_data <- air_quality_data %>% mutate(record_id = row_number())

# Convert the dataset to a tsibble
air_quality_tsibble <- air_quality_data %>%
  as_tsibble(index = Date, key = record_id)

# Filter the data to include only up to August 2023
filtered_data <- air_quality_tsibble %>%
  filter(Date <= as.Date("2023-08-31"))

# Summarize the data to create monthly data
monthly_data <- filtered_data %>%
  index_by(Month) %>%
  summarise(Ozone = mean(Ozone, na.rm = TRUE))

# Ensure the tsibble has a proper regular structure
monthly_data <- monthly_data %>%
  fill_gaps(Ozone = NA) %>%  # Fill missing months
  mutate(Ozone = ifelse(is.na(Ozone), 0, Ozone)) # Replace NAs with 0 for consistent plotting

# Fit an ARIMA auto model to the filtered dataset
arima_auto_fit <- monthly_data %>%
  model(ARIMA(Ozone))

# Forecast for January 2024 (5 months ahead from August 2023)
arima_auto_forecast_january_2024 <- arima_auto_fit %>% forecast(h = "5 months")

# Plot the original data and the forecast
autoplot(arima_auto_forecast_january_2024, monthly_data) +
  ggtitle("Operational Forecast of Ozone Levels in Karnataka for January 2024 (ARIMA Auto Model)") +
  xlab("Month") +
  ylab("Ozone") +
  theme_minimal()

# Print the forecasted values for January 2024
print(arima_auto_forecast_january_2024)
#THANK YOU!

