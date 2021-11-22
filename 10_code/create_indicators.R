# load packages
library(ggplot2)
library(TTR)
library(quantmod)
library(xts)

# load data
setwd("~/GitHub/ids702_modeling/Final Project")
btc<- read.csv("~/GitHub/ids702_modeling/Final Project/20_intermediate_files/btc_prices_social_cleaned.csv")

# modify column types
#btc$time <- as.POSIXct(btc$time, format = "%Y-%m-%d %H:%M:%S")

# turn df into a time series df
btc_ts <- xts(btc[,-1], order.by = as.Date(btc[,]$time))


# create indicators
# SMA, EMA, Bollinger Band, momentum, ROC, MACD, RSI

# SMA
# SMA 10
btc_ts$sma_10 <- SMA(btc_ts$close, n=10)
# SMA 20
btc_ts$sma_20 <- SMA(btc_ts$close, n=20)
# SMA 50
btc_ts$sma_50 <- SMA(btc_ts$close, n=50)
# SMA 200
btc_ts$sma_200 <- SMA(btc_ts$close, n=200)

# EMA
# EMA 9
btc_ts$ema_9 <- EMA(btc_ts$close, n=9)
# EMA 12
btc_ts$ema_12 <- EMA(btc_ts$close, n=12)
# EMA 26
btc_ts$ema_26 <- EMA(btc_ts$close, n=26)
# EMA 50
btc_ts$ema_50 <- EMA(btc_ts$close, n=50)
# EMA 200
btc_ts$ema_200 <- EMA(btc_ts$close, n=200)

# Bollinger Band
# 10-period
n = 10
bb <- BBands(btc_ts$close, n=n, sd=2)
colnames(bb) <- paste(colnames(bb), "_10", sep="")
btc_ts <- cbind(btc_ts, bb)

# Momentum
# n=5
btc_ts$momentum_5 <- momentum(btc_ts$close, n=5)
# n=10
btc_ts$momentum_10 <- momentum(btc_ts$close, n=10)
# n=20
btc_ts$momentum_20 <- momentum(btc_ts$close, n=20)
# n=50
btc_ts$momentum_50 <- momentum(btc_ts$close, n=50)

# Momentum
# n=5
btc_ts$momentum_5 <- momentum(btc_ts$close, n=5)
# n=10
btc_ts$momentum_10 <- momentum(btc_ts$close, n=10)
# n=20
btc_ts$momentum_20 <- momentum(btc_ts$close, n=20)
# n=50
btc_ts$momentum_50 <- momentum(btc_ts$close, n=50)

# ROC
# n=5
btc_ts$roc_5 <- ROC(btc_ts$close, n=5)
# n=10
btc_ts$roc_10 <- ROC(btc_ts$close, n=10)
# n=20
btc_ts$roc_20 <- ROC(btc_ts$close, n=20)
# n=50
btc_ts$roc_50 <- ROC(btc_ts$close, n=50)

# MACD
# 12, 26, 9
btc_ts$macd_1 <- MACD(btc_ts$close, nFast = 12, nSlow = 26, nSig = 9, percent=FALSE)[,1]
btc_ts$signal_1 <- MACD(btc_ts$close, nFast = 12, nSlow = 26, nSig = 9, percent=FALSE)[,2]

# RSI
# n = 2
btc_ts$rsi_2 <- RSI(btc_ts$close, n=2)
# n = 6
btc_ts$rsi_6 <- RSI(btc_ts$close, n=6)
# n = 14
btc_ts$rsi_14 <- RSI(btc_ts$close, n=14)


# Drop first 200 rows because they have NA
btc_ts <- btc_ts[-(1:200),]


