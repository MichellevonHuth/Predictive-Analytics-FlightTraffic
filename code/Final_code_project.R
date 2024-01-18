---
title: "Analysis of Monthly Flights in Denmark"
output: pdf_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Loading the  packages
packages <- c("fpp3", "grid", "gridExtra", "ncdf4", "ncdf4.helpers", "urca", "seasonal")

for(pkg in packages){
  library(pkg, character.only = TRUE)
}
```

## R Markdown

```{r libraries, message=FALSE, warning=FALSE, paged.print=FALSE}
rm(list=ls())
library(dplyr)
library(ggplot2)
library(fpp3)
library(tsibble)
library(seasonal)
library(urca)
library(strucchange)
library(bsts)
```


# Read in the dataset

```{r}
flight_traffic <- read.csv("/Users/michellevonhuth/Documents/Copenhagen/CBS/Semester2/Predictive Analytics/Final Project/FinalProject/flights-denmark.csv", sep=";")
```

# Perform data transformations - convert to a tsibble object

```{r}
flights_reality <- flight_traffic %>%
  mutate(month = yearmonth(as.character(month))) %>%
  as_tsibble(index = month)

# Only use data up intil 2022 Januray 
flights <- flights_reality %>% 
  filter(yearmonth(month) < yearmonth("2020 January"))
```

############################
#### VISUAL INVESTIGATION ##
############################

# Plot data and the perform autocorrelation

```{r}

flights %>% autoplot(flights) + labs(title = "Monthly Traveling Passengers in Denmark")
flights %>% gg_season(flights) + labs(title = "Seasonal plot")
flights %>% gg_subseries(flights) + labs(title = "Seasonal subseries plot")
flights %>% ACF(flights, lag_max = 24) %>% autoplot() + labs(title = "ACF")
```

# Perform SEATS decomposition
```{r}
seats_dcmp <- flights%>%
model(seats = X_13ARIMA_SEATS(flights ~ seats())) %>%
components()

plot_seats <- autoplot(seats_dcmp) +
labs(title = "Monthly Flights in Denmark: Decomposition with SEATS")
print(plot_seats)
```
The SEATS decomposition shows an increasing trend and seasonality throughout the time period analyzed. There is no abrupt dip in the time series. The irregularity in the time series describes variations that are not explained by trend or seasonality.


######################
### TRANSFORM DATA ###
######################


# Box-Cox Transformation
```{r}
# Box-Cox Transformation
lambda <- flights %>%
  features(flights, features = guerrero) %>%
  pull(lambda_guerrero)

# Print the value of lambda
print(lambda)

# Create new column with transformed data using optimal lambda value
flights <- flights %>%
  mutate(box_cox_flights = box_cox(flights, lambda))

# Visualize transformed data
flights %>%
  autoplot(box_cox_flights) +
  labs(title = "Box-Cox Transformation of Monthly Flights in Denmark")
```

```{r}
flights$flights_log = log(flights$flights)
```

```{r}
flights %>%
  autoplot(flights_log)
```
# SEATS decomposition and ACF on Box-Cox transformed data
```{r}
seats_dcmp <- flights%>%
model(seats = X_13ARIMA_SEATS(box_cox_flights ~ seats())) %>%
components()

plot_seats <- autoplot(seats_dcmp) +
labs(title = "Monthly Flights in Denmark: Decomposition with SEATS")
print(plot_seats)

flights %>% ACF(box_cox_flights, lag_max = 60) %>% autoplot() + labs(title = "ACF")
```

#####################################################################

## Check for stationarity

#### First, check for stationarity in plots 
```{r}
flights %>% autoplot(box_cox_flights %>% difference(lag = 12))
flights %>% autoplot(box_cox_flights %>% difference())
flights %>% autoplot(box_cox_flights %>% difference(lag = 12) %>% difference())
```
After the application of first differences, the data still shows non-stationarity due to the presence of pronounced seasonality. However, after the application of both first differences and seasonal differences, the data appears to be stationary and has the characteristics of white noise. To validate these assumptions, ADF and KPSS tests are performed.


# ADF - no difference 

#### ADF with trend to test for a deterministic trend, drift and unit root: 
```{r}
summary(ur.df(flights$box_cox_flights, type = "trend", selectlags = c("AIC"), lags = 12))
```
  
#### Data may exhibit a deterministic trend or drift, but not a deterministic trend with drift. 
\

tau3: -2.144 > -3.13 (t > cv), do not reject the null hypothesis of a unit root (non-stationarity). 

phi2: 5.031  > 4.07  (t > cv), reject the null hypothesis of no deterministic trend and no drift, indicating the presence of a deterministic trend or drift in the data. 

phi3: 2.3485 < 5.47 (t < cv), do not reject the null hypothesis of a unit root and no trend. 

In summary -> we cannot reject the null hypothesis of non-stationarity, suggesting that our data might be non-stationary. The tau3 test does not reject the null hypothesis of a unit root, while the phi3 test does not reject the null hypothesis of no deterministic trend with drift. However, the phi2 test rejects the null hypothesis of no deterministic trend and no drift, indicating evidence of a deterministic trend or drift in the data. For this reason, we can conclude that the data may exhibit a deterministic trend or drift, but not a deterministic trend with drift. For this reason, we move on to test for only drift and unit root. 


#### ADF with drift to test for drift and unit root: 
```{r}
summary(ur.df(flights$box_cox_flights, type = "drift",selectlags = c("AIC"), lags = 12))
```
#### There may be evidence of a drift term.  
\

tau2: -0.1375 > -2.57 (t > cv), do not reject the null hypothesis of a unit root (non-stationarity). 

phi1: 5.114  > 3.81 (t > cv), reject the null hypothesis of no drift, indicating the presence of drift in the data. 

In summary ->  in this case, we are unable to reject the null hypothesis of non-stationarity for tau2, which supports the previous test that suggested our data is non-stationary. We can also reject the null hypothesis of no drift for phi1, indicating that there is evidence of a drift term in the data. Despite the presence of drift, there might still be a unit root in the data. For this reason, we should consider testing for only the unit root. 



#### ADF test to test for unit root:
```{r}
summary(ur.df(flights$box_cox_flights, type = "none",selectlags = c("AIC"), lags = 12))
```
#### The series most likely has a unit root and is non-stationary (stochastic trend). 
\

tau1: 3.2017 > -1.62 (t > cv), we do not reject the null hypothesis of a unit root (non-stationarity). 


#### A summary of the findings from ADF tests: 
\

Unit root: The series has a unit root, which implies that the data is non-stationary. 

Non-stationary: The results indicate that the data is non-stationary as we cannot reject the null hypothesis of non-stationarity in any of the tests. 

Deterministic trend: The phi2 test result rejects the null hypothesis of no deterministic trend and no drift, which indicates that there might be a deterministic trend or drift in the data. 

Stochastic trend: Since the data is non-stationary and has a unit root, it is likely to have a stochastic trend.

Drift: The phi1 test result rejects the null hypothesis of no drift, suggesting that there may be evidence of a drift term in the data.

In summary -> the ADF tests indicate that the data is non-stationary and has a unit root, which implies the presence of a stochastic trend. There may be a deterministic trend or drift in the data, but not a deterministic trend with drift. The presence of drift is also suggested by the test results.



# KPSS - no difference 

From here, considering the results obtained from the ADF tests, I will perform the KPSS tests to further investigate the stationary, deterministic trend, and drift in the data. Since my data suggests the presence of a deterministic trend or drift, it makes sense to perform both KPSS tests with trend and drift, and one with only drift. 

#### KPSS test with trend and drift: 
```{r}
summary(ur.kpss(flights$box_cox_flights, type = "tau"))  # Trend + drift
```
#### This implies that the series does not have a unit root and is stationary. 
\

t: 0.0376 < 0.119 (t < cv), we do not reject the null hypothesis of stationarity. 

In summary -> this means that we cannot reject the null hypothesis of stationarity, and we cannot conclude that the time series has a unit root or a deterministic trend. This implies that the series does not have a unit root and is stationary. 


#### KPSS test with drift: 
```{r}
summary(ur.kpss(flights$box_cox_flights, type = "mu")) # Drift
```
#### This implies that the series have a unit root and is non-stationary. 
\

t: 3.2009 > 0.347 (t > cv), we reject the null hypothesis of no drift, indicating the presence of drift in the data. 

In summary -> this means that we cannot accept the null hypothesis of stationarity, and we can conclude that the time series has a unit root or a deterministic trend. This implies that the series is non-stationary. 

Based on the test results, they indicate that the time series is non-stationary. As a result, differentiation should be used, and the ADF and KPSS tests should be performed again.


### Taking 12 month difference and start from the top again:

#### First, check for stationarity in plots, take the first difference to try to remove the trending pattern
```{r}
flights %>% autoplot(box_cox_flights %>% difference())
```
There isn't a visible trend at the moment. Let's proceed. We compute the ACF and PACF plots for the series to better understand the seasonal pattern; they could indicate whether we should take a seasonal difference in addition to a regular second difference.


#### ADF with first difference (trend):
```{r}
summary(ur.df(diff(log(flights$flights), lag=12), type = "trend", selectlags = "AIC", lags=12))
```
##### The series may have a unit root (non-stationarity) and there is evidence of drift. 
\

tau3: -4.5955 > -3.13 (t > cv) -> do not reject the null hypothesis of a unit root (non-stationarity). 

phi2: 7.0605 > 4.07 (t > cv) -> reject the null hypothesis of no drift, indicating the presence of drift in the data. 

phi3: 10.5907 > 5.47 (t > cv) -> do not reject the null hypothesis of no linear trend. 

In summary -> suggests that the series may have a unit root (non-stationarity) and there is evidence of drift. However, there is no evidence of a linear trend in the data. 


#### ADF with first difference (drift):
```{r}
summary(ur.df(diff(log(flights$flights), lag=12), type = "drift", selectlags = "AIC", lags = 12))
```
##### The series may have a unit root (non-stationarity) and there is evidence of drift. 
\

tau2: -4.5778 > -2.57 (t > cv) ->  not reject the null hypothesis of a unit root (non-stationarity). 

phi1: 10.4782 > 3.81 (t > cv) -> reject the null hypothesis of no drift, indicating the presence of drift in the data. 

In summary -> suggests that the series may have a unit root (non-stationarity) and there is evidence of drift.


#### ADF with first difference (unit root):
```{r}
summary(ur.df(diff(log(flights$flights), lag=12), type = "none", selectlags = "AIC", lags = 12))
```
#### The series may have a unit root (non-stationarity) and there is evidence of drift. 
\

tau1: -3.4207 > -1.62 (t > cv) -> do not reject the null hypothesis of a unit root (non-stationarity). 

In summary -> suggests that the series may have a unit root (non-stationarity) and there is evidence of drift. 


#### KPSS with first difference (trend + drift): 
```{r}
summary(ur.kpss(diff(log(flights$flights), lag=12), type="tau")) # trend + drift
```
### Data is stationary!  
\
t: 0.078 < 0.119 (t < cv) -> cannot reject the null hypothesis. 

In summary -> result suggests that there is no unit root and the time series is stationary. 


#### KPSS with first difference (drift): 
```{r}
summary(ur.kpss(diff(log(flights$flights), lag=12), type = "mu")) # drift
```
### Data is stationary!   
\
t: 0.1464 < 0.347 (t < cv) -> cannot reject the null hypothesis. 

In summary -> suggests that there is no evidence of a unit root with drift, and the time series may be stationary without a drift component. 



### Summary of Results from first difference:

The results of the ADF test indicated that the series may have a unit root, suggesting non-stationarity. Additionally, there was evidence of drift in the data. The ADF test with a regression type of "trend" revealed a rejection of the null hypothesis of no drift, indicating the presence of a drift component. Similarly, the ADF test with a regression type of "drift" also indicated the presence of drift in the series. However, no evidence of a linear trend was found.

In contrast, the KPSS tests yielded different outcomes. The KPSS test with a type of "tau" suggested that the time series is stationary, as the test statistic was lower than the critical value at a 10% significance level. Likewise, the KPSS test with a type of "mu" indicated the absence of a unit root with drift, further suggesting stationary behavior.

Let's attempt to combine seasonal and first differences for a smoother outcome.



# Seasonal and first differences combined. 

#### ACF plot of the differenced data to see if we can spot any seasonal pattern:
```{r}
flights %>% gg_tsdisplay(flights %>% difference(), plot_type = "partial")
```
The very high and regular spikes around lag 6, 12, 18 suggest that taking the seasonal difference with lag = 12 would make sense.


#### Plotting seasonal and first differences combined:
```{r}
flights %>% autoplot(log(flights) %>% difference(lag = 12) %>% difference())

```
The resulting plot exhibits a more consistent variation, let's run the ADF and KPPS tests to see if the series is now stationary. 


#### ADF with seasonal and first difference (trend):
```{r}
summary(ur.df(diff(diff(log(flights$flights),lag=12)), type = "trend", lags = 12))
```

### Data is stationary!  
\
tau3: -6.1494 < -3.13 (t < cv), reject the null hypothesis of a unit root, the transformed data is stationary. 

phi2: 12.6142 > 4.07 (t > cv), do not reject the null hypothesis of no trend or drift, evidence of a deterministic trend or drift. 

phi3: 18.9109 > 5.47 (t > cv), do not reject the null hypothesis of no trend or drift, is evidence of a deterministic trend or drift. 

In summary -> indicates that the data is stationary, but may exhibit a deterministic trend or drift. 


#### ADF with seasonal and first difference (drift):
```{r}
summary(ur.df(diff(diff(log(flights$flights), lag=12)), type = "drift", selectlags = "AIC", lags = 12))
```
### Data is stationary!  
\
tau2: -6.1378 < -2.57 (t < cv), reject the null hypothesis, indicating stationarity. 

phi1: 18.8467 > 3.81 (t > cv), not reject null, may exhibit a deterministic trend or drift. 

In summary -> suggests that the series is stationary as the test statistic for tau2 is less than the critical value. Therefore, the series can be considered stationary!  


#### ADF with seasonal and first difference (unit root):
```{r}
summary(ur.df(diff(diff(log(flights$flights), lag=12)), type="none", selectlags = "AIC", lags = 12))
```
### Data is stationary!  
\
tau1 -> t < cv -> reject null (data is stationary!). 

tau1: -6.1526 < -1.62 (t < cv), we reject the null hypothesis of a unit root (non-stationarity). 

In summary -> suggests that the transformed data is stationary (rejecting the null hypothesis of a unit root) without a deterministic trend or drift component. 



#### KPSS with seasonal and first difference (trend + drift): 
```{r}
summary(ur.kpss(diff(diff(log(flights$flights), lag=12)), type = "tau"))
```
### Data is stationary!  
\
t: 0.0202 < 0.119 (t < cv) -> cannot reject the null hypothesis. 

In summary -> suggests that there is no evidence of a unit root and the time series is stationary. 


#### KPSS with seasonal and first difference (drift): 
```{r}
summary(ur.kpss(diff(diff(log(flights$flights), lag=12)), type = "mu"))
```
### Data is stationary!  
\
t: 0.0323 < 0.347 (t < cv) -> cannot reject the null hypothesis. 

In summary -> suggests that there is no evidence of a unit root, indicating stationarity in the time series even after seasonal and first differences with drift are applied. 

Finally, the difference + seasonal difference with lag = 12 was able to make the data stationary and ready for ARIMA modeling. 

As our data is considered stationary, lets have a look at the ACF and PACF plots for the differenced and seasonally differenced data: 
```{r}
flights %>%
  gg_tsdisplay(difference(flights %>% difference(lag = 12)), plot_type = "partial")
```

#### Based on the provided ACF and PACF plots, here are the interpretations:
\

Non-seasonal ARIMA component: From ACF, an MA(2) component seems to be required. From PACF, the plot suggests an AR(2) component. PACF typically cuts off after the AR term. Therefore, it might be more appropriate to consider ARIMA(2, 0, 2) as the non-seasonal component. 

Seasonal ARIMA component: The ACF and PACF both have significant spikes at lags 11, 12, and 13. This suggests seasonality of order 12. From ACF, there is a significant negative spike at lag 12, suggesting an MA(1) component. From PACF, there are negative spikes at lags 12 and 13, which suggests an AR(2) component.


############################
#### STRUCTURAL BREAKS ##
############################

# QLR Test 
```{r}
# First we need to prepare our data for the structural break test
flights <- flights %>%
  mutate(l1.flights = lag(box_cox_flights),
         l12.flights = lag(box_cox_flights, 12))

# QLR test
qlr_flights <- Fstats(box_cox_flights ~ l1.flights + l12.flights, data = as.ts(flights), from = 0.15)
test_flights <- sctest(qlr_flights, type = "supF")
test_flights

# Plot it
breakpoints_flights <- breakpoints(qlr_flights, alpha = 0.05)
plot(qlr_flights, alpha = 0.05, main = "F Statistics of Monthly Flights in Denmark")
lines(breakpoints_flights)

```
The QLR test plot shows that the curve goes above the red line, indicating the presence of a structural break in the time series data. The associated p-value of 0.002672 suggests that the null hypothesis of no structural break can be rejected. This implies that there is likely at least one structural break in the data, meaning that the pattern before and after the break is different. Therefore, the Chow test is performed to further detect the presence of a structural break in the data. 

# Chow test 
```{r}
# Identify the breakpoints
breakpoints_flights <- breakpoints(qlr_flights, alpha = 0.05)

# Perform the Chow test
k <- breakpoints_flights$breakpoints[1]
data1 <- data.frame(y = flights$box_cox_flights[1:k], x = flights$l1.flights[1:k], z = flights$l12.flights[1:k])
data2 <- data.frame(y = flights$box_cox_flights[(k + 1):nrow(flights)],
                    x = flights$l1.flights[(k + 1):nrow(flights)],
                    z = flights$l12.flights[(k + 1):nrow(flights)])

model1 <- lm(y ~ x + z, data = data1)
model2 <- lm(y ~ x + z, data = data2)

residuals_full <- residuals(lm(box_cox_flights ~ l1.flights + l12.flights, data = flights))
residuals_seg1 <- residuals(model1)
residuals_seg2 <- residuals(model2)

ssr_full <- sum(residuals_full^2)
ssr_seg1 <- sum(residuals_seg1^2)
ssr_seg2 <- sum(residuals_seg2^2)

chow_test_statistic <- ((ssr_full - (ssr_seg1 + ssr_seg2)) / 2) / ((ssr_seg1 + ssr_seg2) / (nrow(flights) - 4))
p_value <- 1 - pf(chow_test_statistic, df1 = 2, df2 = nrow(flights) - 4)

# Print the p-value
print(p_value)

```
The obtained p-value of 0.543486, it indicates that there is no strong statistical evidence to reject the null hypothesis of no structural break. The p-value is greater than the commonly used significance level of 0.05, suggesting that there is no significant difference in the relationship between the two segments of the data. 

However, the QRL test is more informative and reliable in detecting structural breaks, providing insights into variable relationships and underlying dynamics. The Chow test assesses overall model fit but may miss specific parameter changes. To confirm structural breaks, a cumulative sum test was conducted.


# Cumulative sum test
```{r}
# Fit the linear regression model
model <- lm(box_cox_flights ~ l1.flights + l12.flights, data = flights)

# Calculate the residuals
residuals <- residuals(model)

# Calculate the CUSUM statistic
cusum <- cumsum(residuals)

# Calculate the absolute CUSUM statistic
abs_cusum <- abs(cusum)

# Calculate the critical values for the CUSUM test
h <- nrow(flights)
delta <- 1.96 / sqrt(h)  # Adjust the critical value based on the desired significance level

# Find the index of the potential break point(s)
breakpoints <- which(abs_cusum > delta)

# Check if any breakpoints were found
if (length(breakpoints) > 0) {
  # Print the detected break point(s)
  cat("Potential break point(s) detected at index:", breakpoints, "\n")
} else {
  cat("No structural break detected.\n")
}

# Create the index vector for plotting
index <- 1:length(abs_cusum)

# Plot the CUSUM statistic and critical values
plot(index, abs_cusum, type = "l", main = "CUSUM Test for Structural Break",
     xlab = "Observation Index", ylab = "CUSUM Statistic")
abline(h = delta, col = "red", lty = 2)  # Upper critical value
abline(h = -delta, col = "red", lty = 2)  # Lower critical value

```
The cumulative sum (cusum) analysis detected potential structural breaks at indices 92, 93, 94, 95, 96, 97, 98, 100, and 101. These break points indicate significant shifts or changes in the underlying data patterns. Upon visual inspection of the cusum graph, it is evident that a spike in the cumulative sum crosses the threshold represented by the red line. This suggests a departure from the expected behavior and indicates the presence of structural breaks in the data. Therefore,I will proceed by splitting the data into two subsets, and only use the data after the structural break. 

# Split the data 
```{r}
# Identify the point of the structural break
breakpoint <- breakpoints_flights$breakpoints[1]

# Split the data at the point of the structural break
flights_before_break <- flights[1:breakpoint,]
flights_after_break <- flights[(breakpoint+1):nrow(flights),]
```
#### (!) Going further, I will only use the dataset taking place after the structural break. (!)  


# ACF and PACF plots for flights after structural break: 
```{r}
# ACF plot for Flights After Break
acf_after <- acf(flights_after_break$box_cox_flights, main = "ACF - Flights After Break")
acf_values_after <- acf_after$acf

# PACF plot for Flights After Break
pacf_after <- pacf(flights_after_break$box_cox_flights, main = "PACF - Flights After Break")
pacf_values_after <- pacf_after$acf
```
ACF: The significant autocorrelation at lag 1 suggests the possibility of an AR(1) model. Additionally, the significant autocorrelation at lag 12 indicates potential seasonal behavior. This suggests the inclusion of a seasonal MA(1) term in the model. Therefore, an ARMA(1,1) model or an AR(1) model with a seasonal MA(1) term could be appropriate for capturing the autocorrelation in the data. 


PACF: The significant partial autocorrelation at lag 1, which decreases and becomes non-significant beyond lag 1, suggests the inclusion of an autoregressive term in the model, such as an AR(1) model. However, there are some non-significant spikes at lags 2, 3, 4, 6, and 7, indicating possible residual autocorrelation after removing the effect of the previous lags. This suggests that an AR(1) model or an ARMA(1,0) model may be suitable for capturing the partial autocorrelation in the data. 


Considering these observations, it would be reasonable to consider fitting an ARMA(1,1) model or an AR(1) model with a seasonal MA(1) term. 


##################
#### ARIMA  ####
##################

```{r}
train_after_break <- flights_after_break %>% filter(month <= max(month)-12*2)
test_after_break <- flights_after_break %>% filter(month > max(month) - 12 * 2)
train_after_break
test_after_break
```

# Build ARIMA models
```{r}
# After break 
arima <- train_after_break %>%
  model(
    auto_arima = ARIMA(box_cox_flights, stepwise = FALSE, approx = FALSE),
    model1_arima = ARIMA(box_cox_flights ~ pdq(1,1,1) + PDQ(0,1,0)),
    model2_arima = ARIMA(box_cox_flights ~ pdq(1,1,0) + PDQ(0,1,1))
  )

report(arima %>% dplyr::select(auto_arima))
report(arima %>% dplyr::select(model1_arima))
report(arima %>% dplyr::select(model2_arima))
```


#### Pivoting model to long format:
```{r}
arima %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")
```

#### Check the residuals for all three models: 

Auto ARIMA:
```{r}
arima %>%dplyr::select(auto_arima) %>% gg_tsresiduals(lag=36)
```
Model1: 
```{r}
arima %>%dplyr::select(model1_arima) %>% gg_tsresiduals(lag=36)
```
Model2: 
```{r}
arima %>%dplyr::select(model2_arima) %>% gg_tsresiduals(lag=36)
```
The residuals demonstrate some oscillation around the zero line, indicating that our model is, on average, unbiased. 
Nevertheless, notable anomalies are observed in February from year 2010 until 2014. 

The ACF has no significant spikes surpass the boundary, suggesting that the residuals do not display any substantial autocorrelation. 

The normal distribution histogram of the residuals exhibits a multimodal pattern with multiple spikes at different counts. The presence of a large spike to the right of zero residuals suggests a cluster of positive residuals, while the spike to the left indicates a cluster of negative residuals. The spike at the center, directly at zero residuals, indicates a cluster of residuals close to zero. The remaining residuals display a symmetric distribution, slightly skewed to the left towards the end. The isolated spikes on the sides could indicate the presence of outliers or anomalies in the data. Overall, the distribution of residuals shows some deviations from a perfect normal distribution, suggesting potential non-normality in the data.


# Ljung-box test (ARIMA) 
```{r}
arima %>%
residuals() %>%
features(.resid, features = ljung_box, lag = 20)

```
Since the p-value obtained from the Ljung-Box test for the "auto" model is considerably higher than the conventional significance level of 0.05, we lack substantial evidence to reject the null hypothesis. As a result, we can reasonably infer that there is no noteworthy autocorrelation observed in the residuals. This suggests that the residuals of our "auto" model exhibit properties similar to white noise, reinforcing the credibility and appropriateness of our chosen model. 


############################ 
#      Forecast ARIMA     # 
############################

### Train data 

#### Forecast the auto_arima model and model1: 
```{r}
forecast_1 <- forecast(arima, h=36) %>%
  filter(.model=='auto_arima')

forecast_2 <- forecast(arima, h=36) %>%
  filter(.model=='model1_arima')
```

Plotting the 'auto' ARIMA Model with train data:
```{r}
autoplot(forecast_1, train_after_break, color = "yellow")  
```

Plotting the 'model1' ARIMA Model with train data:
```{r}
autoplot(forecast_2, train_after_break, color = "yellow") 
```


### Test data 

#### Plotting the 'auto' ARIMA Model with test data:
```{r}
plot.arima <- arima %>%
  dplyr::select(auto_arima) %>%
  forecast(test_after_break) %>%
  autoplot(flights, color = "yellow") 


print(plot.arima)
```
The prediction interval for the auto model, shows that the prediction interval ("yellow color") is tightly close to the forecasted values ("black line"). This suggests that the model predicts a very small range of uncertainty, but a tiny bit more uncertainty in the positive direction (values more than the forecast). The presence of seasonality indicates that the model has detected and incorporated a repeating pattern in the data.

# Plotting the 'model1' with test data: 
```{r}
plot.arima <- arima %>%
  dplyr::select(model1_arima) %>%
  forecast(test_after_break) %>%
  autoplot(flights, color = "yellow") 


print(plot.arima)
```
The prediction interval for the model1 is wider both above and below the forecast line, compared to the auto model. This implies that it is predicting a larger range of uncertainty for future values, both in the positive and negative direction. This might indicate that 'model1' is less certain about future values compared to the 'auto' ARIMA model.

```{r}
plot.arima <- arima %>%
  dplyr::select(model2_arima) %>%
  forecast(test_after_break) %>%
  autoplot(flights, color = "yellow") 


print(plot.arima)
```

### Calculate the accuracy of these forecasts
```{r}
arima%>%
  forecast(test_after_break) %>%
  accuracy(flights)
```

The auto model has a slightly negative mean error and performs reasonably well in terms of RMSE, MAE, and MAPE. The model1 and model2 models show positive mean errors and have comparable performance in terms of the other metrics. The ACF1 values for all models suggest no significant residual autocorrelation. Overall, the auto model seems to be the best choice based on its performance and lack of autocorrelation in the residuals.



############################ 
#           ETS           # 
############################

# Auto, guess1 and guess2 models 
```{r}
ets <- train_after_break %>%
  model(
    auto_ets = ETS(box_cox_flights),
    guess1_ets = ETS(box_cox_flights ~ error("A") + trend("Ad") + season("A")),
    guess2_ets = ETS(box_cox_flights ~ error("M") + trend("Ad") + season("A"))
  )

report(ets %>%
dplyr::select(auto_ets))

report(ets %>%
dplyr::select(guess1_ets))

report(ets %>%
dplyr::select(guess2_ets))

ets %>%
dplyr::select(auto_ets) %>%
gg_tsresiduals(type = "innovation")

ets %>%
dplyr::select(guess1_ets) %>%
gg_tsresiduals(type = "innovation")

ets %>%
dplyr::select(guess2_ets) %>%
gg_tsresiduals(type = "innovation")
```
The residuals demonstrate a some oscillation around the zero line, indicating that our model is, on average, unbiased. 
Nevertheless, notable anomalies are observed in February 2012, and February 2014. The ACF has no significant spikes surpass the boundary, suggesting that the residuals do not display any substantial autocorrelation. The histogram plot of the residuals demonstrates some asymmetric distribution. The frequency of residuals approx. gradually decreases on both sides, suggesting a normal distribution.


#### Ljung-box test (ETS)
```{r}
ets %>%
residuals() %>%
features(.resid, features = ljung_box, lag = 20)
```
The Ljung-Box p-values for all three models are high, which suggests that there is no significant autocorrelation in the residuals of either model. This indicates that the ETS models adequately capture the autocorrelation structure in the data.


############################ 
#      Forecast ETS     # 
############################

#### Forecasts from Guessed ETS model
```{r}
plot_guess <- ets %>%
  dplyr::select(guess2_ets) %>%
  forecast(test_after_break) %>%
  autoplot(flights, color = "orange") +
  labs(title = "Forecasts from Guessed2 ETS model")

print(plot_guess)
```


#### Forecasts from Auto ETS model
```{r}
plot_auto <- ets %>%
  dplyr::select(auto_ets) %>%
  forecast(test_after_break) %>%
  autoplot(flights, color = "orange") +
  labs(title = "Forecasts from Auto ETS model")
  
print(plot_auto)
```

```{r}
ets_forecast <- ets %>%
forecast(test_after_break)

accuracy(ets_forecast, flights %>%
dplyr::select(box_cox_flights))
```
The "auto_ets" model exhibited the best performance among the three models, as it achieved lower values for all matrices, compared to the "guess1_ets" and "guess2_ets" models, indicating higher forecasting accuracy and a closer fit to the test data. 


############################ 
#          SNAIVE          # 
############################

#### Forecasting with SNAIVE Model:
```{r}
model(train_after_break, snaive = SNAIVE(flights)) %>% report()
```
The SNAIVE model estimated the variance (sigma^2) of the "flights" series to be 4562.6236. This indicates that the model's predictions had a relatively large spread or deviation from the actual data points. A higher variance suggests that the model may not have captured the underlying patterns and dynamics of the data effectively, resulting in less accurate predictions. 


#### SNAIVE Model with lagged year variable (train data):
```{r}
train_after_break %>% model(SNAIVE(box_cox_flights ~ lag("year")))
```

#### Fit multiple models on train data (Naive, Random Walk, Mean, and SNAIVE):
```{r}
flights_fit <- train_after_break %>%
  model(
    naive = NAIVE(box_cox_flights),
    drift = RW(box_cox_flights ~ drift()),
    mean = MEAN(box_cox_flights), 
    snaive = SNAIVE(box_cox_flights)
  )
```


#### Forecasts the models with a horizon of "2 years": 
```{r}
forecast_flights <- flights_fit %>% 
  forecast(h= "2 years")
```


```{r}
flights_fit %>%
  dplyr::select(snaive) %>%
  gg_tsresiduals()

```
There is one significant spike at lag 12 that goes outside the threshold boundaries in the ACF plot, it suggests the presence of a significant seasonal autocorrelation at that lag. It may imply that the model is not adequately capturing the seasonal patterns in the data. The presence of two high spikes in the histogram suggests a bimodal distribution, indicating the existence of two distinct groups or patterns within the observed values.


#### Forecasting with SNAIVE Model: 
```{r}
flights_fit%>%
  dplyr::select(snaive) %>%
  forecast(h = "2 years") %>%
  autoplot(flights,color = "red")
```
The red curves in the forecast occasionally exceed the black line, indicating higher forecast errors, the overall performance captures the underlying trend and seasonality, suggesting a reasonably good forecast. However,the red curves extend beyond the black line more frequently than the ETS and ARMA models indicating that the snaive model may have more variability and less accuracy in predicting the future values.

#### Accuracy Assessment of Forecast
```{r}
accuracy(forecast_flights, flights %>%
dplyr::select(box_cox_flights))
```
The Snaive model outperformed the other models, exhibiting the lowest values for RMSE, MAE, MASE, and RMSSE, suggesting it provides the most accurate forecast for the given data.


############################ 
#          Reality        # 
############################

#### Finally, what the actual air traffic looked like both during and after the pandemic: 
```{r}
flights_reality %>% autoplot(flights) + labs(title = "Flight traffic")

```

