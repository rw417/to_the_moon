library(tseries)
library(forecast)
library(MLmetrics)
library(astsa)
library(ggplot2)
library(progress)


btc <- read.csv("~/GitHub/ids702_modeling/Final Project/20_intermediate_files/btc_prices_social_cleaned.csv")

# Modeling for fun ####
# close
btc_ts
tsClose <- ts(btc$close)
auto.arima(tsClose)

model1 <- arima(tsClose ~ volume_btc + reddit_posts_per_hour, data=btc, c(2,1,2))
summary(model1)

predict(model1, n.ahead=10)

# return
return <- (btc$close - btc$open) / btc$open
tsReturn <- ts(return)
auto.arima(tsReturn)

model2 <- arima(tsReturn, c(2,0,0))
summary(model2)

predict(model2, n.ahead=1)

# return
auto.arima(btc$close, xreg=cbind(btc$volume_btc, btc$reddit_comments_per_hour))

model3 <- arima(btc$close, c(2,1,2), xreg=cbind(btc$volume_btc, btc$reddit_comments_per_hour))
summary(model3)

predict(model3,n.ahead=10)

# EDA ####
tsClose <- ts(btc$close)
ts.plot(tsClose)
# is closing price stationary?
acf(tsClose, lag.max=400)
pacf(tsClose) # seems like only lag1 matters

adf_test <- adf.test(tsClose, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose)
print(kpss_test)

# looks like data is not stationary
# however, if we only look at the last 200 rows (approx 8 days), would that be stationary?
btc_200 <- btc[(nrow(btc)-199):nrow(btc),]

tsClose <- ts(btc_200$close)
ts.plot(tsClose)

acf(tsClose)
pacf(tsClose)

adf_test <- adf.test(tsClose, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose)
print(kpss_test)

# still not stationary
# what about 100 rows?
btc_100 <- btc[(nrow(btc)-99):nrow(btc),]

tsClose <- ts(btc_100$close)
ts.plot(tsClose)

acf(tsClose)
pacf(tsClose)

adf_test <- adf.test(tsClose, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose)
print(kpss_test)


#####
# what about returns?
#####
btc_200$return <- (btc_200$close - btc_200$open) / btc_200$open
tsReturn <- ts(btc_200$return)
ts.plot(tsReturn)

acf(tsReturn)
pacf(tsReturn)

# return doesn't seem to be a time series for 200 rows!!!

# what about all rows???
btc$return <- (btc$close - btc$open) / btc$open
tsReturn <- ts(btc$return)
ts.plot(tsReturn) # it's certainly messy

acf(tsReturn)
pacf(tsReturn, lag.max = 1000)

adf_test <- adf.test(tsReturn, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn)
print(kpss_test)


# what about log returns?
btc$logreturn <- log(btc$close/btc$open)
tsLogReturn <- ts(btc$logreturn)
ts.plot(tsLogReturn) # it's certainly messy

acf(tsLogReturn, ylim=c(0,0.1), lag.max=1000)
pacf(tsLogReturn, lag.max = 1000)
# no change - will use returns since it's more straightforward

# second order differrencing
tsReturn2 <- diff(tsClose, lag=10)
ts.plot(tsReturn2) # it's certainly messy

acf(tsReturn2)
pacf(tsReturn2, lag.max = 1000, ylim=c(-0.2,0.2))

adf_test <- adf.test(tsReturn2, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn2)
print(kpss_test)


# What about last 8000 rows (last year)?
btc_8k <- btc[(nrow(btc)-8000):nrow(btc),]
tsClose_8k <- ts(btc_8k$close)
ts.plot(tsClose_8k)

tsClose_8k_diff <- diff(tsClose_8k, lag=1, difference=1)
ts.plot(tsClose_8k_diff, ylim=c(-2000,2000))
btc_8k$return <- (btc_8k$close - btc_8k$open) / btc_8k$open
tsReturn_8k <- ts(btc_8k$return)
ts.plot(tsReturn_8k)

acf(tsClose_8k_diff)
pacf(tsClose_8k_diff, lag.max = 1000)

acf(tsReturn_8k)
pacf(tsReturn_8k, lag.max = 1000)






# Modeling ####
# seems like partial autocorrelation decreases at around 500 days - try this
model1 <- arima(tsReturn, c(5,0,0))
summary(model1)

predict(model1, n.ahead=1)

far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
e <- tsCV(tsReturn, far2, h=1)


# Dynamic Regression
xreg = cbind(reddit_active_users = stats::lag(ts(btc[,"reddit_active_users"]),-1),
             reddit_posts_per_hour = stats::lag(ts(btc[,"reddit_posts_per_hour"]),-1),
             reddit_comments_per_hour=stats::lag(ts(btc[,"reddit_comments_per_hour"]),-1)
             )

xreg = cbind(reddit_posts_per_hour = stats::lag(ts(btc[,"reddit_posts_per_hour"]),-1),
             reddit_posts_per_hour_l2 = stats::lag(ts(btc[,"reddit_posts_per_hour"]),-2))

fit1 <- auto.arima(btc$return[3:(nrow(btc)-20)], xreg=xreg[3:(nrow(btc)-20),1], d=1)
fit1.1 <- auto.arima(ts(btc$return[3:(nrow(btc)-20)]), xreg=xreg[3:(nrow(btc)-20),1], d=1)
fit2 <- auto.arima(btc$return[3:(nrow(btc)-20)], xreg=xreg[3:(nrow(btc)-20),2], d=1)
fit3 <- auto.arima(btc$return[3:(nrow(btc)-20)], xreg=xreg[3:(nrow(btc)-20),1:3])
fit3 <- auto.arima(btc$return, xreg = ts(btc$open))

fit1[['aicc']]; fit1.1[['aicc']]; fit2[['aicc']]; fit3[['aicc']]
checkresiduals(fit)
fc <- forecast(fit, h=1,xreg=xreg[1:20,])

fc


# After 11/19 Office Hour ####
## one-step-ahead forecast ####
pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = 100, clear = TRUE, width= 80)
one_step_ahead_arima <- function(data, ahead, st, ed){
  len <- ahead
  results = matrix(ncol = 2, nrow = len)
  for (i in 1:len){
    pb$tick()
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    training_observed = window(data, start = start_cal, end = end_cal, frequency = 24)
    
    fit <- Arima(training_observed, order=c(2,0,2), seasonal=c(0,0,2))
    
    demandforecast = forecast(fit,1)$mean[1]
    observed = window(data, start=(end_cal + c(0,1)), end=(end_cal + c(0,1)))
    
    results[i,1]= observed
    results[i,2]= demandforecast
  }
  return(results)
}


one_step_ahead_sarima <- function(data, validation) {
  len <- length(validation)
  results = matrix(ncol = 2, nrow = len)
  for (i in 1:len){
    training_observed = window(data, start = c(1,1), end = c(832,(23+i)), frequency = 24)
    
    forecasted.sarima = sarima.for(training_observed, n.ahead = 1,
                                   p=2,d=0,q=2,P=0,D=0,Q=2,S=24)
    
    demandforecast = forecasted.sarima$pred
    observed = validation[[i]]
    
    results[i,1]= observed
    results[i,2]= demandforecast
  }
  return(results)
}


## Validation functions for return ####
directional_acc <- function(fitted, test) {
  same_dir <- fitted * test
  pct = sum(same_dir > 0)
  return(pct / length(same_dir))
}

best_return <- function(actual_return, cash) {
  for (i in actual_return) {
    if (i > 0) {
      cash <- cash * (1+i)
    }
  }
  return(cash)
}

strategy_return <- function(actual_return, fitted, cash) {
  for (i in 1:length(actual_return)) {
    if (fitted[i] > 0) {
      cash <- cash * (1 + actual_return[i])
    }
  }
  return(cash)
}

strategy_loss <- function(actual_return, fitted, cash) {
  return(strategy_return(actual_return, fitted, cash) / best_return(actual_return, cash))
}



# adjust for 24-hour seasonality
# close
tsClose_s24 <- ts(btc$close, frequency=24)
ts.plot(tsClose_s24)
tsOpen_s24 <- ts(btc$open, frequency=24)
ts.plot(tsOpen_s24)
acf(tsClose_s24)
pacf(tsClose_s24, lag.max = 1000, ylim=c(-0.03,0.03))

adf_test <- adf.test(tsClose_s24, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose_s24)
print(kpss_test)

## return ####
tsReturn_s24 <- (tsClose_s24 - tsOpen_s24) / tsOpen_s24
tsReturn_s24_2 <- ts(((btc$close - btc$open) / btc$open), frequency=24)
ts.plot(tsReturn_s24)
ts.plot(tsReturn_s24_2)
acf(tsReturn_s24, ylim=c(-0.2,0.2), lag.max=1000)
pacf(tsReturn_s24, lag.max=1000)

adf_test <- adf.test(tsReturn_s24_2, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn_s24_2)
print(kpss_test)

### Split test train ####
train <- btc[1:(0.9*nrow(btc)),]
test <- btc[(0.9*nrow(btc)+1):nrow(btc),]
tsReturn_s24_train <- window(tsReturn_s24, start=c(1,1), end=c(832,24))
tsReturn_s24_test <- window(tsReturn_s24, start=c(833,1))




## Modeling ####
# ARIMA
fit <- Arima(tsReturn_s24_train, order=c(2,0,2), seasonal=c(0,0,2))
fit_test <- Arima(tsReturn_s24_test, model=fit)
fitted(fit_test)
window(tsReturn_s24, start=751)
autoplot(tsReturn_s24) + autolayer(fitted(fit_test)) + xlim(c(830,834)) + ylim(c(-0.01,0.01))
# validation
MAPE(fitted(fit_test), tsReturn_s24_test)
directional_acc(fitted(fit_test), tsReturn_s24_test)


auto.arima(tsReturn_s24)
# ARIMA(2,0,2)(0,0,2)[24] with non-zero mean
auto.arima(tsReturn_s24[3:(nrow(btc)-20)], xreg=xreg[3:(nrow(btc)-20),1:3], seasonal=c(0,0,2))

## forecast using asta
sarima_forecast_asta = sarima.for(tsReturn_s24_train, n.ahead = length(tsReturn_s24_test),
                             p=2,d=0,q=2,P=0,D=0,Q=2,S=24)
autoplot(tsReturn_s24) + autolayer(sarima_forecast_asta$pred) + xlim(c(830,834)) + ylim(c(-0.01,0.01))

# ETS
ets_model = ets(tsReturn_s24_train, allow.multiplicative.trend = TRUE)
ets_forecast = forecast(ets_model, h=length(tsReturn_s24_test))
MAPE(ets_forecast$mean, tsReturn_s24_test) *100

# TBATS
tbats_model = tbats(tsReturn_s24_train)
tbats_forecast = forecast(tbats_model, h=length(tsReturn_s24_test))
autoplot(tsReturn_s24) + autolayer(tbats_forecast$mean) +autolayer(fitted(fit_test)) + xlim(c(832,834)) + ylim(c(-0.013,0.013))
MAPE(tbats_forecast$mean, tsReturn_s24_test) * 100








# Chunk for Friday 11/19 Office Hour ####
# return
tsReturn <- ts(btc$return)
ts.plot(tsReturn) # it's certainly messy
acf(tsReturn)
pacf(tsReturn, lag.max = 1000)

adf_test <- adf.test(tsReturn, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn)
print(kpss_test)

# return 200
btc_200 <- btc[(nrow(btc)-199):nrow(btc),]
btc_200$return <- (btc_200$close - btc_200$open) / btc_200$open
tsReturn_200 <- ts(btc_200$return)
ts.plot(tsReturn_200)
acf(tsReturn_200)
pacf(tsReturn_200, lag.max = 1000)

adf_test <- adf.test(tsReturn_200, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn_200)
print(kpss_test)

# return 8000
tsReturn_8k <- ts(btc_8k$return)
ts.plot(tsReturn_8k)

acf(tsReturn_8k)
pacf(tsReturn_8k, lag.max = 1000)

# return 1000
btc_1000 <- btc[(nrow(btc)-999):nrow(btc),]
btc_1000$return <- (btc_1000$close - btc_1000$open) / btc_1000$open
tsReturn_1000 <- ts(btc_1000$return)
ts.plot(tsReturn_1000)
acf(tsReturn_1000)
pacf(tsReturn_1000, lag.max = 1000)

adf_test <- adf.test(tsReturn_1000, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn_1000)
print(kpss_test)


# return 2000-1000
btc_2k1k <- btc[(nrow(btc)-1999):(nrow(btc)-1000),]
btc_2k1k$return <- (btc_2k1k$close - btc_2k1k$open) / btc_2k1k$open
tsReturn_2k1k <- ts(btc_2k1k$return)
ts.plot(tsReturn_2k1k)
acf(tsReturn_2k1k)
pacf(tsReturn_2k1k, lag.max = 1000)

adf_test <- adf.test(tsReturn_2k1k, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn_2k1k)
print(kpss_test)


#close
tsClose <- ts(btc$close)
ts.plot(tsClose_200)
acf(tsClose)
pacf(tsClose)

adf_test <- adf.test(tsClose, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose)
print(kpss_test)

#close 200
tsClose_200 <- ts(btc_200$close)
ts.plot(tsClose_200)
acf(tsClose_200)
pacf(tsClose_200)

adf_test <- adf.test(tsClose_200, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsClose_200)
print(kpss_test)

