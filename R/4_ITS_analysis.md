# Interrupted time-series analysis

The objective of this code is to use processed counts to generate ITS analysis

R version 3.6.2

## Importing packages
```R
library(tidyverse)
library(data.table)
library(tableone)
library(lubridate)
library(dplyr)
library(forecast)
```

## Input files
1) Processed weekly attendance count file, after manual addition of coefficients
2) Counterfactual weekly attendance count file

## Interrupted time-series modelling, Gaussian generalised linear model

```R
# GLM model 
model1 = glm(count ~ lockdown+time+time_2+time_3+christmas+feb_break+tax_season+easter+bank+may_break+summer+autumn_break+min_restrictions+lockdown_2+lockdown_3+leaving_lockdown, 
    family=gaussian(link="identity"),
    att_count)

summary(model1)
round(confint(model1),3)
pred1 = predict(model1, type="response",att_count)

# GLM model counterfactual
att_count_cf = data.frame(lockdown=0,time=att_count$time,time_2=att_count$time_2,time_3=att_count$time_3,christmas=att_count$christmas,feb_break=att_count$feb_break,tax_season=att_count$tax_season,easter=att_count$easter,bank=att_count$bank,may_break=att_count$may_break,summer=att_count$summer,autumn_break=att_count$autumn_break,min_restrictions=0,lockdown_2=0,lockdown_3=0,leaving_lockdown=0)

pred1b = predict(model1,att_count_cf,type="response")

# Checking diagnostics
res1 = residuals(model1, type="deviance")
checkresiduals(res1)
acf(res1)
pacf(res1)
```

## Interrupted time-series modelling, ARIMA model

```R
# ARIMA model 
model3 = auto.arima(att_count$count, 
    seasonal=TRUE, 
    xreg=cbind(att_count$lockdown,att_count$time, att_count$time_2,att_count$time_3,att_count$christmas,att_count$feb_break,att_count$tax_season,att_count$easter,att_count$bank,att_count$may_break,att_count$summer,att_count$autumn_break,att_count$min_restrictions,att_count$lockdown_2,att_count$lockdown_3,att_count$leaving_lockdown), 
    max.d=1,
    D=1,
    stepwise=F,
    trace=T)

summary(model3)
round(confint(model3),3)
arima = as.data.frame(fitted(model3))
arima$upper_ci = arima$x + 1.96*sqrt(model3$sigma2)
arima$lower_ci = arima$x - 1.96*sqrt(model3$sigma2)

# Recheck diagnostics
checkresiduals(model3)
Box.test(model3$residuals, lag=1, type="Ljung-Box")
acf(model3$residuals)
pacf(model3$residuals)

# Load counterfactual attendance dataset
att_count_cf = fread("attendance_count_noae_weekly_model_cf.csv")

# ARIMA model counterfactual
model4 = Arima(att_count_cf$count,order=c(0,0,2), 
    xreg=cbind(att_count_cf$time, att_count_cf$time_2,att_count_cf$time_3,att_count_cf$christmas,att_count_cf$feb_break,att_count_cf$tax_season,att_count_cf$easter,att_count_cf$bank,att_count_cf$may_break,att_count_cf$summer,att_count_cf$autumn_break))

fc = forecast(model4, 
    xreg=cbind(att_count_cf$time, att_count_cf$time_2,att_count_cf$time_3,att_count_cf$christmas,att_count_cf$feb_break,att_count_cf$tax_season,att_count_cf$easter,att_count_cf$bank,att_count_cf$may_break,att_count_cf$summer,att_count_cf$autumn_break))

fc.ts = ts(as.numeric(fc$mean))
att_count_ts.2 = as.data.frame(fc.ts)
forecast_sigma = fc$model
att_count_ts.2$upper_ci = att_count_ts.2$x + 1.96*sqrt(forecast_sigma$sigma2)
att_count_ts.2$lower_ci = att_count_ts.2$x - 1.96*sqrt(forecast_sigma$sigma2)
```