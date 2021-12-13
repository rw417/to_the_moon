library(tseries)
library(forecast)
library(MLmetrics)
library(ggplot2)
library(ggfortify)
library(grid)
library(gridExtra)
library(zoo)

# Read data and create ts ####
btc <- read.csv("~/GitHub/ids702_modeling/Final Project/20_intermediate_files/btc_prices_social_cleaned.csv")
firstDay <- as.Date("2019-07-25 11:00:00")-as.Date("2019-1-1 00:00:00")
tsClose_s24 <- ts(btc$close, frequency=24, start=c(firstDay, 12))

tsReturn_s24 <- ts((btc$close - btc$open) / btc$open
                   ,frequency=24, start=c(firstDay, 12)
                   )

# EDA ####
## raw
p_eda <- list()
p_eda[[1]] <- autoplot(tsReturn_s24) + labs(title="BTC Hourly Returns", y='Return') + xlim(c(205,1038))
p_eda[[2]] <- autoplot(tsClose_s24) + labs(title='BTC Hourly Close Prices', y='Close Price') + xlim(c(205,1038))
grid.arrange(p_eda[[1]], p_eda[[2]], nrow=2)

## acf, pacf
autoplot(stats::acf(tsClose_s24, plot=FALSE, lag.max=120))
autoplot(stats::acf(tsReturn_s24, plot=FALSE, lag.max=120))
autoplot(stats::pacf(tsReturn_s24, plot=FALSE, lag.max=120)
         #, ylim=c(-0.04,0.04)
        )
autoplot(stats::pacf(tsClose_s24, plot=FALSE, lag.max=120)
         #, ylim=c(-0.04,0.04)
         )

pacf(tsReturn_s24, plot=TRUE,  ylim=c(-0.04,0.04), lag.max=80)
pacf(tsClose_s24, plot=TRUE,  ylim=c(-0.04,0.04), lag.max=80)

## tests
adf_test <- adf.test(tsClose_s24, alternative = 'stationary')
kpss_test <- kpss.test(tsClose_s24)
print(adf_test) ; print(kpss_test)

adf_test <- adf.test(tsReturn_s24, alternative = 'stationary')
kpss_test <- kpss.test(tsReturn_s24)
print(adf_test) ; print(kpss_test)

## arrange tsClose and tsReturn
#tsClose
p_close <- list()
p_close[[1]] <- autoplot(tsClose_s24) + labs(title="BTC Hourly Close Prices", y='Close Price')
p_close[[2]] <- autoplot(stats::acf(tsClose_s24, plot=FALSE, lag.max=120)) + labs(title="ACF of Hourly Close Prices", y='ACF')
p_close[[3]] <- autoplot(stats::pacf(tsClose_s24, plot=FALSE, lag.max=120)) + labs(title="PACF of Hourly Close Prices", y='PACF')
p_close[[4]] <- autoplot(stats::pacf(tsClose_s24, plot=FALSE, lag.max=120)) + labs(title="PACF of Hourly Close Prices - Zoomed In", y='PACF') + ylim(c(-0.02, 0.02))
p_close[[5]] <- textGrob(print(c('Stationarity Tests: \n \n ADF test p-value: 0.69 \n KPSS test p-value: < 0.01')))
lay <- rbind(c(1,2),
             c(1,3),
             c(5,4))
grid.arrange(p_close[[1]], p_close[[2]], p_close[[3]], p_close[[4]], p_close[[5]], layout_matrix=lay)

#tsReturn
p_return <- list()
p_return[[1]] <- autoplot(tsReturn_s24) + labs(title="BTC Hourly Returns", y='Returns')
p_return[[2]] <- autoplot(stats::acf(tsReturn_s24, plot=FALSE, lag.max=120)) + labs(title="ACF of Hourly Returns", y='ACF')
p_return[[3]] <- autoplot(stats::acf(tsReturn_s24, plot=FALSE, lag.max=120)) + labs(title="ACF of Hourly Returns - Zoomed In", y='ACF') + ylim(c(-0.037, 0.03))
p_return[[4]] <- autoplot(stats::pacf(tsReturn_s24, plot=FALSE, lag.max=120)) + labs(title="PACF of Hourly Returns", y='PACF')
p_return[[5]] <- textGrob(print(c('Stationarity Tests: \n \n ADF test p-value: < 0.01 \n KPSS test p-value: > 0.1')))
lay <- rbind(c(1,2),
             c(1,3),
             c(5,4))
grid.arrange(p_return[[1]], p_return[[2]], p_return[[3]], p_return[[4]], p_return[[5]], layout_matrix=lay)


# Modelling ####
# ARIMA
train_return <- window(tsReturn_s24, end=c(1036,18))
fit_auto <- auto.arima(train_return)
final_model <- saveRDS(fit_auto, file='20_intermediate_files/final_model.rds')

# Lagged
# reddit posts
reddit_posts_per_hour <- window(ts(btc[,"reddit_posts_per_hour"], frequency=24), end=c(1036,18))
reddit <- cbind(reddit_comments_per_hour_L1 = stats::lag(reddit_posts_per_hour,-1),
                reddit_comments_per_hour_L2 = stats::lag(reddit_posts_per_hour,-2),
                reddit_comments_per_hour_L3 = stats::lag(reddit_posts_per_hour,-3))

#training_observed = window(tsReturn_s24, start = c(600,1), end = c(656,24))
fit_lagged1 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1])
####
fit_lagged2 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1:2]) # 2 lags much better than 1 lag
####
fit_lagged3 <- auto.arima(window(train_return, start=c(205, 15)), xreg=reddit[4:(length(reddit[,1])-50),1:3]) # 3 lags not much better than 2 lags

# DHR
plots_lambdaNULL <- list()
for (i in 5) {
  fit <- auto.arima(train_return, xreg=fourier(train_return, K=i),
                    seasonal=FALSE, lambda=NULL)
  plots_lambdaNULL[[i]] <- autoplot(forecast(fit,xreg=fourier(train_return, K=i, h=5))) +
    xlab(paste("K=",i,"  AICC=",round(fit[["aicc"]],2)," LL=",round(fit[["loglik"]],2))) +
    ylab("") +
    xlim(c(1036,1037))
}
gridExtra::grid.arrange(
  plots_lambdaNULL[[1]],plots_lambdaNULL[[2]],plots_lambdaNULL[[3]],
  plots_lambdaNULL[[4]],plots_lambdaNULL[[5]],plots_lambdaNULL[[6]],
  nrow=2)

fit_dhr5 <- auto.arima(train_return, xreg=fourier(train_return, K=5),
                  seasonal=FALSE, lambda=NULL)


# Model Validation ####
checkresiduals(fit_auto)
checkresiduals(fit_lagged2)
checkresiduals(fit_dhr5)
pacf(fit_auto$residuals)

t <- as.POSIXct(btc[1:19951,]$time)
# lagged vs residuals
ggplot(window(fit_auto$residuals, start=c(205,13)), 
       aes(
         x=window(stats::lag(fit_auto$x, -1), end=c(1036,18)), 
         y=window(fit_auto$residuals, start=c(205,13))
         )
       ) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") +
  labs(title="Residuals vs Lagged Return", x="Lagged Returns", y="Residuals") +
  scale_x_continuous() + scale_y_continuous()

# fitted vs residuals
ggplot(window(fit_auto$residuals, start=c(205,13)), 
       aes(
         x=window(stats::lag(fit_auto$x, -1), end=c(1036,18)), 
         y=window(fit_auto$residuals, start=c(205,13))
       )
) +
  geom_point(alpha = .5,colour="blue4") +
  geom_hline(yintercept=0,col="red3") +
  labs(title="Residuals vs Lagged Return", x="Fitted Returns", y="Residuals") +
  scale_x_continuous() + scale_y_continuous()

autoplot(ts(fit_auto$residuals))

# Back-Testing ####
arima_501_8wk <- readRDS('20_intermediate_files/arima_501_8wk.rds')
arima_551_8wk <- readRDS('20_intermediate_files/arima_551_8wk.rds')
arima_601_8wk <- readRDS('20_intermediate_files/arima_601_8wk.rds')
arima_651_8wk <- readRDS('20_intermediate_files/arima_651_8wk.rds')
arima_701_8wk <- readRDS('20_intermediate_files/arima_701_8wk.rds')
arima_751_8wk <- readRDS('20_intermediate_files/arima_751_8wk.rds')
arima_801_8wk <- readRDS('20_intermediate_files/arima_801_8wk.rds')

start_price1 = window(tsClose_s24, start=c(500,24)+c(205, 12), end=c(500,24)+c(205, 12))
start_price2 = window(tsClose_s24, start=c(550,24)+c(205, 12), end=c(550,24)+c(205, 12))
start_price3 = window(tsClose_s24, start=c(600,24)+c(205, 12), end=c(600,24)+c(205, 12))
start_price4 = window(tsClose_s24, start=c(650,24)+c(205, 12), end=c(650,24)+c(205, 12))
start_price5 = window(tsClose_s24, start=c(700,24)+c(205, 12), end=c(700,24)+c(205, 12))
start_price6 = window(tsClose_s24, start=c(750,24)+c(205, 12), end=c(750,24)+c(205, 12))
start_price7 = window(tsClose_s24, start=c(800,24)+c(205, 12), end=c(800,24)+c(205, 12))

{
t <- matrix(ncol=7, nrow=7)
t[1,1] <- rollapply(window(tsClose_s24, start=c(445,1)+c(205, 12), end=c(500,74)+c(205, 12)), width=1394, FUN=sd)
t[2,1] <- rollapply(window(tsClose_s24, start=c(495,1)+c(205, 12), end=c(550,74)+c(205, 12)), width=1394, FUN=sd)
t[3,1] <- rollapply(window(tsClose_s24, start=c(545,1)+c(205, 12), end=c(600,74)+c(205, 12)), width=1394, FUN=sd)
t[4,1] <- rollapply(window(tsClose_s24, start=c(595,1)+c(205, 12), end=c(650,74)+c(205, 12)), width=1394, FUN=sd)
t[5,1] <- rollapply(window(tsClose_s24, start=c(645,1)+c(205, 12), end=c(700,74)+c(205, 12)), width=1394, FUN=sd)
t[6,1] <- rollapply(window(tsClose_s24, start=c(695,1)+c(205, 12), end=c(750,74)+c(205, 12)), width=1394, FUN=sd)
t[7,1] <- rollapply(window(tsClose_s24, start=c(745,1)+c(205, 12), end=c(800,74)+c(205, 12)), width=1394, FUN=sd)

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

t[1,7] <-directional_acc(arima_501_8wk[,1], arima_501_8wk[,2])
t[2,7] <-directional_acc(arima_551_8wk[,1], arima_551_8wk[,2])
t[3,7] <-directional_acc(arima_601_8wk[,1], arima_601_8wk[,2])
t[4,7] <-directional_acc(arima_651_8wk[,1], arima_651_8wk[,2])
t[5,7] <-directional_acc(arima_701_8wk[,1], arima_701_8wk[,2])
t[6,7] <-directional_acc(arima_751_8wk[,1], arima_751_8wk[,2])
t[7,7] <-directional_acc(arima_801_8wk[,1], arima_801_8wk[,2])

colnames(t) <- c('ts_sd', 'fit_aic', 'fit_mse', 'portfolio_value', 'noaction_value', 'performance', 'directional_acc')
# usually, the lower the sd, the more accurate the model is
# however, performance also depends on being able to predict the direction accurately
# e.g. in 701_8wk, although MSE was higher, we accurately predicted a huge dropoff, so performance was good
}

## plot ####
tp <- list()
tp[[1]] <- plot_portfolio_value(c(501,1)+c(205,12), arima_501_8wk[,1],arima_501_8wk[,2]) + labs(title="12pm, Dec. 7th, 2020", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[2]] <- plot_portfolio_value(c(551,1)+c(205,12), arima_551_8wk[,1],arima_551_8wk[,2]) + labs(title="12pm, Jan. 26th, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[3]] <- plot_portfolio_value(c(601,1)+c(205,12), arima_601_8wk[,1],arima_601_8wk[,2]) + labs(title="12pm, Mar. 17th, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[4]] <- plot_portfolio_value(c(651,1)+c(205,12), arima_651_8wk[,1],arima_651_8wk[,2]) + labs(title="12pm, May. 6th, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[5]] <- plot_portfolio_value(c(701,1)+c(205,12), arima_701_8wk[,1],arima_701_8wk[,2]) + labs(title="12pm, Jun. 25th, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[6]] <- plot_portfolio_value(c(751,1)+c(205,12), arima_751_8wk[,1],arima_751_8wk[,2]) + labs(title="12pm, Aug. 14th, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
tp[[7]] <- plot_portfolio_value(c(801,1)+c(205,12), arima_801_8wk[,1],arima_801_8wk[,2]) + labs(title="12pm, Oct. 3rd, 2021", x='Days Since Jan 1st, 2019', y='USD Value')
gridExtra::grid.arrange(
  tp[[2]],tp[[3]],tp[[4]],
  tp[[5]],tp[[6]], tp[[7]],nrow=3)







