library(tseries)
library(forecast)
library(MLmetrics)
library(astsa)
library(ggplot2)
library(progress)
library(zoo)

# After 11/19 Office Hour ####
## one-step-ahead forecast ####

# Arima
pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = 100, clear = TRUE, width= 80)
one_step_ahead_arima <- function(data, ahead, st, ed){
  len = ahead
  results = matrix(ncol = 9, nrow = len)
  start_cal = st
  end_cal = ed
  
  for (i in 1:len){
    #pb$tick()
    
    training_observed = window(data, start = start_cal, end = end_cal)
    
    fit <- Arima(training_observed, order=c(2,0,2), seasonal=c(0,0,2))
    
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    
    observed = window(data, start=end_cal, end=end_cal)
    demandforecast = forecast(fit,1)$mean[1]
    lo80 = forecast(fit,1)$lower[1]
    hi80 = forecast(fit,1)$upper[1]
    lo95 = forecast(fit,1)$lower[2]
    hi95 = forecast(fit,1)$upper[2]
    aic = fit$aic
    bic = fit$bic
    MSE = MSE(fitted(fit), fit$x)
    
    results[i,1]= observed
    results[i,2]= demandforecast
    results[i,3]= lo80
    results[i,4]= hi80
    results[i,5]= lo95
    results[i,6]= hi95
    results[i,7]= aic
    results[i,8]= bic
    results[i,9]= MSE
  }
  colnames(results) <- c('obs', 'pred', 'lo80', 'hi80', 'low95', 'hi95', 'aic', 'bic', 'MSE')
  return(results)
}

# alternative arima
pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = 100, clear = TRUE, width= 80)
one_step_ahead_arima_alt <- function(data, ahead, st, ed){
  len = ahead
  results = matrix(ncol = 9, nrow = len)
  start_cal = st
  end_cal = ed
  
  for (i in 1:len){
    #pb$tick()
    
    training_observed = window(data, start = start_cal, end = end_cal)
    
    fit <- Arima(training_observed, order=c(0,0,2), seasonal=c(2,0,0))
    
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    
    observed = window(data, start=end_cal, end=end_cal)
    demandforecast = forecast(fit,1)$mean[1]
    lo80 = forecast(fit,1)$lower[1]
    hi80 = forecast(fit,1)$upper[1]
    lo95 = forecast(fit,1)$lower[2]
    hi95 = forecast(fit,1)$upper[2]
    aic = fit$aic
    bic = fit$bic
    MSE = MSE(fitted(fit), fit$x)
    
    results[i,1]= observed
    results[i,2]= demandforecast
    results[i,3]= lo80
    results[i,4]= hi80
    results[i,5]= lo95
    results[i,6]= hi95
    results[i,7]= aic
    results[i,8]= bic
    results[i,9]= MSE
  }
  colnames(results) <- c('obs', 'pred', 'lo80', 'hi80', 'low95', 'hi95', 'aic', 'bic', 'MSE')
  return(results)
}

# SARIMA
pb <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = 100, clear = TRUE, width= 80)
one_step_ahead_sarima <- function(data, ahead, st, ed) {
  len = ahead
  results = matrix(ncol = 3, nrow = len)
  start_cal = st
  end_cal = ed
  
  for (i in 1:len){
    #pb$tick()

    training_observed = window(data, start = start_cal, end = end_cal)

    forecasted.sarima <- sarima.for(training_observed, n.ahead = 1,
                                   p=2,d=0,q=2,P=0,D=0,Q=2,S=24)
  
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    
    observed = window(data, start=end_cal, end=end_cal)

    results[i,1]= observed
    results[i,2]= forecasted.sarima$pred[1]
    results[i,3]= forecasted.sarima$se[1]
  }
  colnames(results) <- c('obs', 'pred', 'se')
  return(results)
}


# Dynamic Harmonic Regression
# pb <- progress_bar$new(
#   format = "[:bar] :percent eta: :eta",
#   total = 100, clear = TRUE, width= 80)
one_step_ahead_dhr <- function(data, ahead, st, ed){
  len = ahead
  results = matrix(ncol = 10, nrow = len)
  start_cal = st
  end_cal = ed
  
  for (i in 1:len){
    #pb$tick()
    training_observed = window(data, start = start_cal, end = end_cal)
    
    fit <- Arima(training_observed, order=c(0,1,2), xreg=fourier(training_observed, K=1),
                 lambda=0)
    
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    
    observed = window(data, start=end_cal, end=end_cal)
    fc = forecast(fit,xreg=fourier(training_observed, K=1, h=1))
    demandforecast = fc$mean[1]
    lo80 = fc$lower[1]
    hi80 = fc$upper[1]
    lo95 = fc$lower[2]
    hi95 = fc$upper[2]
    aic = fit$aic
    bic = fit$bic
    MSE = sum(na.omit(rowSums(cbind(-fit$fitted, fit$x))**2)) / length(na.omit(rowSums(cbind(-fit$fitted, fit$x))**2))
    NA_pct = sum(is.na(fit$fitted)) / length(fit$fitted)
    
    results[i,1]= observed
    results[i,2]= demandforecast
    results[i,3]= lo80
    results[i,4]= hi80
    results[i,5]= lo95
    results[i,6]= hi95
    results[i,7]= aic
    results[i,8]= bic
    results[i,9]= MSE
    results[i,10]= NA_pct
  }
  colnames(results) <- c('obs', 'pred', 'lo80', 'hi80', 'low95', 'hi95','aic', 'bic', 'MSE', 'NA_pct')
  return(results)
}


# Lagged predictors
one_step_ahead_lagged <- function(data, ahead, st, ed){
  len = ahead
  results = matrix(ncol = 9, nrow = len)
  start_cal = st
  end_cal = ed
  
  for (i in 1:len){
    #pb$tick()
    reddit_posts_per_hour <- window(ts(btc[,"reddit_posts_per_hour"], frequency=24), start_cal, end_cal)
    reddit <- cbind(reddit_comments_per_hour_L1 = stats::lag(reddit_posts_per_hour,-1),
                    reddit_comments_per_hour_L2 = stats::lag(reddit_posts_per_hour,-2))
    training_observed = window(data, start = start_cal, end = end_cal)
    
    fit <- Arima(ts(training_observed[2:length(training_observed)], frequency=24), 
                 xreg=reddit[2:length(training_observed), 1:2], order=c(4,0,0), seasonal=c(2,0,0))
    
    start_cal = (st + c(0,i))
    end_cal = (ed + c(0,i))
    
    observed = window(data, start=end_cal, end=end_cal)
    fc = forecast(fit,xreg=window(reddit,end_cal, end_cal),h=1)
    demandforecast = fc$mean[1]
    lo80 = fc$lower[1]
    hi80 = fc$upper[1]
    lo95 = fc$lower[2]
    hi95 = fc$upper[2]
    aic = fit$aic
    bic = fit$bic
    MSE = MSE(fitted(fit), fit$x)
    
    results[i,1]= observed
    results[i,2]= demandforecast
    results[i,3]= lo80
    results[i,4]= hi80
    results[i,5]= lo95
    results[i,6]= hi95
    results[i,7]= aic
    results[i,8]= bic
    results[i,9]= MSE
  }
  colnames(results) <- c('obs', 'pred', 'lo80', 'hi80', 'low95', 'hi95','aic', 'bic', 'MSE')
  return(results)
}

# Holt Winters
hw_multi_d <- hw(train, damped=TRUE, seasonal="multiplicative")
hw_add_d <- hw(train, damped=TRUE, seasonal="additive")
hw_multi <- hw(train, damped=FALSE, seasonal="multiplicative")
hw_add <- hw(train, damped=FALSE, seasonal="additive")

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

assumed_value <- function(start_price, returns) {
  current_price <- start_price
  values <- c(current_price)
  for (i in 1:length(returns)) {
    current_price  <- current_price * (1+returns[i])
    values <- append(values, current_price)
  }
  return(values)
}


plot_assumed_value <- function(ed, returns) {
  start_price = window(tsClose_s24, start=ed, end=ed)
  ts_values = ts(assumed_value(start_price, returns), frequency=24, start=ed)
  
  p <- autoplot(tsClose_s24) + autolayer(ts_values, show.legend=FALSE) + 
    xlim(
      c(
        (ed[1])),
        (ed[1]+round(length(returns)/24)+2)
      )
  return(p)
}


portfolio_value <- function(start_value, actual_return, fitted) {
  current_value <- start_value
  values <- c(current_value)
  for (i in 1:length(actual_return)) {
    if (fitted[i] > 0) {
      current_value <- current_value * (1 + actual_return[i])
    }
    values <- append(values, current_value)
  }
  return(values)
}


plot_portfolio_value <- function(ed, actual_return, fitted) {
  start_price = window(tsClose_s24, start=ed, end=ed)
  #values = portfolio_value(start_price, actual_return, fitted)
  ts_values = ts(portfolio_value(start_price, actual_return, fitted), frequency=24, start=(ed))
  
  p <- autoplot(tsClose_s24) + autolayer(ts_values, show.legend=FALSE) + 
    xlim(
      c(
        (ed[1])),
      (ed[1]+round(length(actual_return)/24)+2)
    ) +
    ylim(
      c(
        min(ts_values, window(tsClose_s24, ed[1], (ed[1]+round(length(actual_return)/24)+2))) - 200,
        max(ts_values, window(tsClose_s24, ed[1], (ed[1]+round(length(actual_return)/24)+2))) + 200)
    )
  return(p)
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
acf(tsReturn_s24, ylim=c(-0.2,0.2))
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



## Modeling for real ####
# ARIMA
train_return <- window(tsReturn_s24, end=c(1036,18))
auto.arima(train_return)
# ARIMA(2,0,2)(0,0,2)[24] with non-zero mean
arima_forecast_8wk <- one_step_ahead_arima(tsReturn_s24 , 100, st=c(700,1), ed=c(756,24))
#model one-step-ahead using 8 weeks of data; do this for 100 hours

#returns?
best_return(arima_forecast_8wk[,1],1000)
#max return 31%
strategy_return(arima_forecast_8wk[,1],arima_forecast_8wk[,2], 1000)
#actual return 8% - not bad

directional_acc(arima_forecast_8wk[,1], arima_forecast_8wk[,2])
sum((arima_forecast_8wk[,1] > 0)*(arima_forecast_8wk[,2] > 0))
sum((arima_forecast_8wk[,1] < 0)*(arima_forecast_8wk[,2] < 0))
#model correctly predicted positive return 43 / 58 times;
#correctly predicted negative return 8 / 42 times;
#this might just be due to price increases more often than decreases though


# SARIMA
#return the same values
sarima_forecast <- one_step_ahead_sarima(tsReturn_s24 , 100, st=c(700,1), ed=c(756,24))


# Dynamic Harmonic Regression (model seasonality with Fourier terms)
#to deal with long seasonal lags
#training_observed <- window(tsClose_s24, start=c(433,1), end=c(500,24))
plots_lambda3 <- list()
for (i in seq(6)) {
  fit <- auto.arima(train_return, xreg=fourier(train_return, K=i),
                    seasonal=FALSE, lambda=3)
  plots_lambda3[[i]] <- autoplot(forecast(fit,xreg=fourier(train_return, K=i, h=5))) +
                                   xlab(paste("K=",i,"  AICC=",round(fit[["aicc"]],2))) +
                                   ylab("") +
                                   xlim(c(1036,1037))
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]],
  nrow=3)
gridExtra::grid.arrange(
  plots_lambdaNULL[[1]],plots_lambdaNULL[[2]],plots_lambdaNULL[[3]],
  plots_lambdaNULL[[4]],plots_lambdaNULL[[5]],plots_lambdaNULL[[6]],
  nrow=2)
gridExtra::grid.arrange(
  plots_lambda1[[1]],plots_lambda1[[2]],plots_lambda1[[3]],
  plots_lambda1[[4]],plots_lambda1[[5]],plots_lambda1[[6]],
  nrow=2)
gridExtra::grid.arrange(
  plots_lambda2[[1]],plots_lambda2[[2]],plots_lambda2[[3]],
  plots_lambda2[[4]],plots_lambda2[[5]],plots_lambda2[[6]],
  nrow=2)
gridExtra::grid.arrange(
  plots_lambda3[[1]],plots_lambda3[[2]],plots_lambda3[[3]],
  plots_lambda3[[4]],plots_lambda3[[5]],plots_lambda3[[6]],
  nrow=2)



#seems we should use ARIMA(1,1,1) with Fourier K=1
dhr_forecast <- one_step_ahead_dhr(tsReturn_s24 , 25, st=c(500,1), ed=c(556,24))
dhr_forecast <- one_step_ahead_dhr(tsReturn_s24 , 1000, st=c(600,1), ed=c(656,24))


# Lagged Predictors
# reddit posts
reddit_posts_per_hour <- window(ts(btc[,"reddit_posts_per_hour"], frequency=24), end=c(1036,18))
reddit <- cbind(reddit_comments_per_hour_L1 = stats::lag(reddit_posts_per_hour,-1),
             reddit_comments_per_hour_L2 = stats::lag(reddit_posts_per_hour,-2),
             reddit_comments_per_hour_L3 = stats::lag(reddit_posts_per_hour,-3))

#training_observed = window(tsReturn_s24, start = c(600,1), end = c(656,24))
fit_lagged1 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1])
####
fit_lagged2 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1:2]) # 2 lags much bettern than 1 lag
####
fit_lagged3 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1:3]) # 3 lags not much better than 2 lags
# (2,0,2)(0,0,2) no longer good with reddit data
Arima(ts(training_observed[4:1368], frequency=24), xreg=reddit[4:1368,1], order=c(2,0,2), seasonal=c(0,0,2))
Arima(ts(training_observed[4:1368], frequency=24), xreg=reddit[4:1368,1:2], order=c(2,0,2), seasonal=c(0,0,2))
Arima(ts(training_observed[4:1368], frequency=24), xreg=reddit[4:1368,1:3], order=c(2,0,2), seasonal=c(0,0,2))

# reddit active users
# not as good as reddit posts
# reddit comments has similar problem as reddit active users
reddit_active_users <- window(ts(btc[,"reddit_active_users"], frequency=24), c(600,1), c(656,24))
reddit_active <- cbind(reddit_comments_per_hour_L1 = stats::lag(reddit_active_users,-1),
                reddit_comments_per_hour_L2 = stats::lag(reddit_active_users,-2),
                reddit_comments_per_hour_L3 = stats::lag(reddit_active_users,-3))

auto.arima(ts(training_observed[4:1368], frequency=24), xreg=reddit_active[4:1368,1])
auto.arima(ts(training_observed[4:1368], frequency=24), xreg=reddit_active[4:1368,1:2])
auto.arima(ts(training_observed[4:1368], frequency=24), xreg=reddit_active[4:1368,1:3])
Arima(ts(training_observed[4:1368], frequency=24), xreg=reddit_active[4:1368,1:2], order=c(4,0,0), seasonal=c(2,0,0))
# NaNs produced when forcing seasonality - likely overfitting





# Large jobs ####
## does more data help?
# DHR
dhr_4wk <- one_step_ahead_dhr(tsClose_s24, 25, st=c(473,1), ed=c(500,24))
dhr_8wk <- one_step_ahead_dhr(tsReturn_s24, 25, st=c(445,1), ed=c(500,24))
dhr_12wk <- one_step_ahead_dhr(tsReturn_s24, 25, st=c(417,1), ed=c(500,24))
dhr_24wk <- one_step_ahead_dhr(tsReturn_s24, 25, st=c(333,1), ed=c(500,24))
saveRDS(dhr_4wk, file='20_intermediate_files/dhr_4wk.rds')
saveRDS(dhr_8wk, file='20_intermediate_files/dhr_8wk.rds')
saveRDS(dhr_12wk, file='20_intermediate_files/dhr_12wk.rds')
saveRDS(dhr_24wk, file='20_intermediate_files/dhr_24wk.rds')

#ARIMA
arima_4wk <- one_step_ahead_arima(tsReturn_s24, 25, st=c(473,1), ed=c(500,24))
saveRDS(arima_4wk, file='20_intermediate_files/arima_4wk.rds')
arima_8wk <- one_step_ahead_arima(tsReturn_s24, 25, st=c(445,1), ed=c(500,24))
saveRDS(arima_8wk, file='20_intermediate_files/arima_8wk.rds')
arima_12wk <- one_step_ahead_arima(tsReturn_s24, 25, st=c(417,1), ed=c(500,24))
saveRDS(arima_12wk, file='20_intermediate_files/arima_12wk.rds')
arima_24wk <- one_step_ahead_arima(tsReturn_s24, 25, st=c(333,1), ed=c(500,24))
saveRDS(arima_24wk, file='20_intermediate_files/arima_24wk.rds')

## how does model perform over time?
#DHR
dhr_overtime <- one_step_ahead_dhr(tsReturn_s24, 330, st=c(445,1), ed=c(500,24))
saveRDS(dhr_overtime, file='20_intermediate_files/dhr_overtime.rds')
#ARIMA
arima_overtime <- one_step_ahead_arima(tsReturn_s24, 330, st=c(445,1), ed=c(500,24))
saveRDS(arima_overtime, file='20_intermediate_files/arima_overtime.rds')
#########
arima_overtime_1 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(545,1), ed=c(600,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_1.rds')
arima_overtime_2 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(565,1), ed=c(620,24))
saveRDS(arima_overtime_2, file='20_intermediate_files/arima_overtime_2.rds')
arima_overtime_3 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(585,1), ed=c(640,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_3.rds')
arima_overtime_4 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(605,1), ed=c(660,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_4.rds')
arima_overtime_5 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(625,1), ed=c(680,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_5.rds')
arima_overtime_6 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(645,1), ed=c(700,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_6.rds')
arima_overtime_7 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(665,1), ed=c(720,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_7.rds')
arima_overtime_8 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(685,1), ed=c(740,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_8.rds')
arima_overtime_9 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(705,1), ed=c(760,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_9.rds')
arima_overtime_10 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(725,1), ed=c(780,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_10.rds')
arima_overtime_11 <- one_step_ahead_arima(tsReturn_s24, 480, st=c(745,1), ed=c(800,24))
saveRDS(arima_overtime_1, file='20_intermediate_files/arima_overtime_11.rds')
#########



#Lagged
## does more data help?
lagged_4wk <- one_step_ahead_lagged(tsReturn_s24, 25, st=c(473,1), ed=c(500,24))
saveRDS(lagged_4wk, file='20_intermediate_files/lagged_4wk.rds')
lagged_8wk <- one_step_ahead_lagged(tsReturn_s24, 25, st=c(445,1), ed=c(500,24))
saveRDS(lagged_4wk, file='20_intermediate_files/lagged_8wk.rds')
lagged_12wk <- one_step_ahead_lagged(tsReturn_s24, 25, st=c(417,1), ed=c(500,24))
saveRDS(lagged_4wk, file='20_intermediate_files/lagged_12wk.rds')
lagged_24wk <- one_step_ahead_lagged(tsReturn_s24, 25, st=c(333,1), ed=c(500,24))
saveRDS(lagged_4wk, file='20_intermediate_files/lagged_24wk.rds')
## how does model perform over time?
lagged_overtime <- one_step_ahead_lagged(tsReturn_s24, 330, st=c(445), ed=c(500,24))
saveRDS(lagged_overtime, file='20_intermediate_files/lagged_overtime.rds')

# 12/11/2021 ####
# DHR just doesn't work for return - discard
# Arima seems best
# lagged did worse than arima

## run arima over multiple time periods ####
arima_501_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(445,1), ed=c(500,24))
saveRDS(arima_501_8wk, file='20_intermediate_files/arima_501_8wk.rds')
arima_551_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(495,1), ed=c(550,24))
saveRDS(arima_551_8wk, file='20_intermediate_files/arima_551_8wk.rds')
arima_601_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(545,1), ed=c(600,24))
saveRDS(arima_601_8wk, file='20_intermediate_files/arima_601_8wk.rds')
arima_651_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(595,1), ed=c(650,24))
saveRDS(arima_651_8wk, file='20_intermediate_files/arima_651_8wk.rds')
arima_701_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(645,1), ed=c(700,24))
saveRDS(arima_701_8wk, file='20_intermediate_files/arima_701_8wk.rds')
arima_751_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(695,1), ed=c(750,24))
saveRDS(arima_751_8wk, file='20_intermediate_files/arima_751_8wk.rds')
arima_801_8wk <- one_step_ahead_arima(tsReturn_s24, 50, st=c(745,1), ed=c(800,24))
saveRDS(arima_801_8wk, file='20_intermediate_files/arima_801_8wk.rds')

arima_601_8wk <- readRDS(file="20_intermediate_files/arima_601_8wk.rds")
### plot returns ####
p <- list()
p[[1]] <- plot_portfolio_value(c(500,24), arima_501_8wk[,1], arima_501_8wk[,2])
p[[2]] <- plot_portfolio_value(c(550,24), arima_551_8wk[,1], arima_551_8wk[,2])
p[[3]] <- plot_portfolio_value(c(600,24), arima_601_8wk[,1], arima_601_8wk[,2])
p[[4]] <- plot_portfolio_value(c(650,24), arima_651_8wk[,1], arima_651_8wk[,2])
p[[5]] <- plot_portfolio_value(c(700,24), arima_701_8wk[,1], arima_701_8wk[,2])
p[[6]] <- plot_portfolio_value(c(750,24), arima_751_8wk[,1], arima_751_8wk[,2])
p[[7]] <- plot_portfolio_value(c(800,24), arima_801_8wk[,1], arima_801_8wk[,2])
gridExtra::grid.arrange(
  p[[1]],p[[2]],p[[3]],
  p[[4]], nrow=3)
gridExtra::grid.arrange(
  p[[5]],p[[6]],p[[7]],
  nrow=2)
gridExtra::grid.arrange(
  p[[1]],p[[2]],p[[3]],
  p[[4]], p[[5]],p[[6]],p[[7]], nrow=2)

### compare performance across different time periods ####
start_price1 = window(tsClose_s24, start=c(500,24), end=c(500,24))
start_price2 = window(tsClose_s24, start=c(550,24), end=c(550,24))
start_price3 = window(tsClose_s24, start=c(600,24), end=c(600,24))
start_price4 = window(tsClose_s24, start=c(650,24), end=c(650,24))
start_price5 = window(tsClose_s24, start=c(700,24), end=c(700,24))
start_price6 = window(tsClose_s24, start=c(750,24), end=c(750,24))
start_price7 = window(tsClose_s24, start=c(800,24), end=c(800,24))

autoplot(window(tsClose_s24, start=c(645,1), end=c(703,24))) +
  autolayer(ts(portfolio_value(start_price5,arima_701_8wk[,1], arima_701_8wk[,2]), frequency=24, start=c(701,1)),
            show.legend=FALSE)

t <- matrix(ncol=6, nrow=7)
t[1,1] <- rollapply(window(tsClose_s24, start=c(445,1), end=c(500,74)), width=1394, FUN=sd)
t[2,1] <- rollapply(window(tsClose_s24, start=c(495,1), end=c(550,74)), width=1394, FUN=sd)
t[3,1] <- rollapply(window(tsClose_s24, start=c(545,1), end=c(600,74)), width=1394, FUN=sd)
t[4,1] <- rollapply(window(tsClose_s24, start=c(595,1), end=c(650,74)), width=1394, FUN=sd)
t[5,1] <- rollapply(window(tsClose_s24, start=c(645,1), end=c(700,74)), width=1394, FUN=sd)
t[6,1] <- rollapply(window(tsClose_s24, start=c(695,1), end=c(750,74)), width=1394, FUN=sd)
t[7,1] <- rollapply(window(tsClose_s24, start=c(745,1), end=c(800,74)), width=1394, FUN=sd)

t[1,2] <- mean(arima_501_8wk[,7])
t[2,2] <- mean(arima_551_8wk[,7])
t[3,2] <- mean(arima_601_8wk[,7])
t[4,2] <- mean(arima_651_8wk[,7])
t[5,2] <- mean(arima_701_8wk[,7])
t[6,2] <- mean(arima_751_8wk[,7])
t[7,2] <- mean(arima_801_8wk[,7])

t[1,3] <- mean(arima_501_8wk[,9])
t[2,3] <- mean(arima_551_8wk[,9])
t[3,3] <- mean(arima_601_8wk[,9])
t[4,3] <- mean(arima_651_8wk[,9])
t[5,3] <- mean(arima_701_8wk[,9])
t[6,3] <- mean(arima_751_8wk[,9])
t[7,3] <- mean(arima_801_8wk[,9])

t[1,4] <- strategy_return(arima_501_8wk[,1], arima_501_8wk[,2], 1000)
t[2,4] <- strategy_return(arima_551_8wk[,1], arima_551_8wk[,2], 1000)
t[3,4] <- strategy_return(arima_601_8wk[,1], arima_601_8wk[,2], 1000)
t[4,4] <- strategy_return(arima_651_8wk[,1], arima_651_8wk[,2], 1000)
t[5,4] <- strategy_return(arima_701_8wk[,1], arima_701_8wk[,2], 1000)
t[6,4] <- strategy_return(arima_751_8wk[,1], arima_751_8wk[,2], 1000)
t[7,4] <- strategy_return(arima_801_8wk[,1], arima_801_8wk[,2], 1000)

t[1,5] <- tail(assumed_value(1000, arima_501_8wk[,1]), n=1)
t[2,5] <- tail(assumed_value(1000, arima_551_8wk[,1]), n=1)
t[3,5] <- tail(assumed_value(1000, arima_601_8wk[,1]), n=1)
t[4,5] <- tail(assumed_value(1000, arima_651_8wk[,1]), n=1)
t[5,5] <- tail(assumed_value(1000, arima_701_8wk[,1]), n=1)
t[6,5] <- tail(assumed_value(1000, arima_751_8wk[,1]), n=1)
t[7,5] <- tail(assumed_value(1000, arima_801_8wk[,1]), n=1)

t[,6] <- (t[,4] / t[,5] - 1) * 100

colnames(t) <- c('ts_sd', 'fit_aic', 'fit_mse', 'portfolio_value', 'noaction_value', 'performance')
# usually, the lower the sd, the more accurate the model is
# however, performance also depends on being able to predict the direction accurately
# e.g. in 701_8wk, although MSE was higher, we accurately predicted a huge dropoff, so performance was good


## Alternative ARIMA ####
# run arima over multiple time periods
arima_501_8wk_alt <- one_step_ahead_arima(tsReturn_s24, 50, st=c(445,1), ed=c(500,24))
saveRDS(arima_501_8wk_alt, file='20_intermediate_files/arima_501_8wk_alt.rds')
arima_551_8wk_alt <- one_step_ahead_arima(tsReturn_s24, 50, st=c(495,1), ed=c(550,24))
saveRDS(arima_551_8wk_alt, file='20_intermediate_files/arima_551_8wk_alt.rds')
arima_601_8wk_alt <- one_step_ahead_arima(tsReturn_s24, 50, st=c(545,1), ed=c(600,24))
saveRDS(arima_601_8wk_alt, file='20_intermediate_files/arima_601_8wk_alt.rds')
arima_651_8wk_alt <- one_step_ahead_arima(tsReturn_s24, 50, st=c(595,1), ed=c(650,24))
saveRDS(arima_651_8wk_alt, file='20_intermediate_files/arima_651_8wk_alt.rds')



## Modeling Scratch ####
# ARIMA
fit <- Arima(train_return, order=c(2,0,2), seasonal=c(0,0,2))
fit_test <- Arima(tsReturn_s24_test, model=fit)
fitted(fit_test)
window(tsReturn_s24, start=751)
autoplot(tsReturn_s24) + autolayer(fitted(fit_test)) + xlim(c(830,834)) + ylim(c(-0.01,0.01))
# validation
MAPE(fitted(fit_test), tsReturn_s24_test)
directional_acc(fitted(fit_test), tsReturn_s24_test)


auto.arima(tsReturn_s24, seasonal=TRUE, stepwise=TRUE, approximation=TRUE)
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