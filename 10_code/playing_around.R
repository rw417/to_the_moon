library(tseries)
library(forecast)
library(ggplot2)

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



# Split test train ####
train <- btc[1:(0.9*nrow(btc)),]
test <- btc[(0.9*nrow(btc)+1):nrow(btc),]
tsReturn_s24_train <- window(tsReturn_s24, start=1, end=750)
tsReturn_s24_test <- window(tsReturn_s24, start=751)


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
             open = ts(btc$open))

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

# return
tsReturn_s24 <- (tsClose_s24 - tsOpen_s24) / tsOpen_s24
tsReturn_s24_2 <- ts(((btc$close - btc$open) / btc$open), frequency=24)
ts.plot(tsReturn_s24)
ts.plot(tsReturn_s24_2)
acf(tsReturn_s24, ylim=c(-0.2,0.2))
pacf(tsReturn_s24, lag.max=1000)

adf_test <- adf.test(tsReturn_s24_2, alternative = 'stationary')
print(adf_test)

kpss_test <- kpss.test(tsReturn_s24_2)
print(kpss_test)

fit <- Arima(tsReturn_s24_train, order=c(2,0,2), seasonal=c(1,0,0))
fit_test <- Arima(tsReturn_s24_test, model=fit)
fitted(fit_test)
window(tsReturn_s24, start=751)
autoplot(tsReturn_s24) + autolayer(fitted(fit_test)) + xlim(c(751,780)) + ylim(c(-0.01,0.01))

auto.arima(tsReturn_s24)
# ARIMA(2,0,2)(0,0,2)[24] with non-zero mean
auto.arima(tsReturn_s24[3:(nrow(btc)-20)], xreg=xreg[3:(nrow(btc)-20),1:3], seasonal=c(0,0,2))



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

