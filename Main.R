# library to be used
library(TTR) # Technical Trading Rules
library(forecast) # Time Series Forecasting Function
library(tseries) # Time Series Analysis

# Files to be used
source("Functions.R")

fil <-c("AORD_cleaned.csv", "KLCI_cleaned.csv", "STI_cleaned.csv")
nm <- c("AORD", "KLCI", "STI")
misc_col <- 8


# Naive Long System
run_NaiveLongSystem <- function (fil, nm){
  df10 <- as.data.frame(matrix(seq(8), nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- NaiveLongSystem(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1, ]
  return(df10)
}

res1 <- run_NaiveLongSystem (fil, nm)
res1[misc_col] <- 'Naive Long'

# for summary results
total_res <- res1


# Naive Reverse System
run_NaiveReversePrev <- function (fil, nm){
  df10 <- as.data.frame(matrix(seq(8) ,nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i], stringsAsFactors =F)
    a <- NaiveReversePrev(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1, ]
  return(df10)
}

res2 <- run_NaiveReversePrev(fil, nm)
res2[misc_col] <- 'Reverse Prev'

# Add to total results
total_res <- rbind(total_res, res2)


# SMA System
run_SMA_sys <- function(fil, nm){
  df10 <- as.data.frame (matrix(seq(8), nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- SMA_sys(Mkt, 5, nm[i])
    b <- SMA_sys(Mkt, 25, nm[i])
    c <- SMA_sys(Mkt, 50, nm[i])
    d <- SMA_sys(Mkt, 100, nm[i])
    e <- SMA_sys(Mkt, 200, nm[i])
    df10 <- rbind(df10, a, b, c, d, e)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1, ]
  return(df10)
}

res3 <- run_SMA_sys(fil, nm)

# Add to total results
total_res <- rbind(total_res, res3)


# MACD
run_MACD_sys <- function(fil, nm){
  df10 <- as.data.frame(matrix(seq(8), nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    ma <- MACD(Mkt[,"Open"], 12, 26, 9, maType = "EMA") #calc MACD values
    Mkt <- cbind(Mkt, ma)
    a <- MACD_sys(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res4 <- run_MACD_sys(fil, nm)
res4[misc_col] <- 'MACD'

# Add to total results
total_res <- rbind(total_res, res4)


# Aroon Indicator
run_aroon_sys <- function(fil, nm){
  df10 <- as.data.frame (matrix(seq(8), nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    ar <- aroon(Mkt[c(3, 4)], n = 20) #calc Aroon values
    Mkt <- cbind(Mkt, ar) #Add Aroon values to original data set
    a <- aroon_sys(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res5 <- run_aroon_sys(fil, nm)
res5[misc_col] <- 'Aroon'

# Add to total results
total_res <- rbind(total_res, res5)


# Parabolic SAR
run_sar_sys <- function(fil, nm){
  df10 <- as.data.frame (matrix(seq(8), nrow=1, ncol=8))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    sar <- SAR(Mkt[c(3, 4)])
    Mkt <- cbind(Mkt, sar)
    a <- sar_sys(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[ -1,]
  return(df10)
}

res6 <- run_sar_sys(fil, nm)
res6 [misc_col] <- 'SAR'

# Add to total results
total_res <- rbind(total_res, res6)


# Daily Breakout System
run_breakout_sys <- function(fil, nm){
  df10 <- as.data.frame(matrix(seq(8), nrow=1, ncol=8))
  for(i in 1: length(fil)){
    Mkt <- read.csv(fil[i])
    a <- breakout_sys(Mkt, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res7 <- run_breakout_sys(fil, nm)
res7[misc_col] <- 'Daily Breakout'

# Add to total results
total_res <- rbind(total_res, res7)


# Auto ARIMA Model
arim_mod_fnc <- function(fil, nm){
  dfres <- dfres <- t(c('a', 'b'))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_train <- ts(Mkt$Close)
    arima_train_mod <- auto.arima(Mkt_train, max.p=30, max.q=30)
    dfres <- rbind(dfres, c(nm[i], forecast(arima_train_mod)$method))
  }
  return(dfres)
}

fg <- arim_mod_fnc(fil, nm)
fg <- fg[-1, ]
colnames(fg) <- c('Market', 'ARIMA Model')

df10 <- as.data.frame(matrix(seq(8), nrow=1, ncol=8))
ts_1_fnc <- function(fil, nm, ts1){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_ts <- ts(Mkt$Close)
    Mkt_train <- window(Mkt_ts, end=1499.99)
    Mkt_test <- window(Mkt_ts, start=1500)
    arima_train_mod <- auto.arima(Mkt_train)
    arima_fcast <- forecast(arima_train_mod, Mkt_test)
    arima_test_mod <- Arima(Mkt_test, model=arima_train_mod) # 1 step fcast on future data ...
    arima_test_fcast <- forecast(arima_test_mod)
    fitted.data <- as.data.frame(arima_test_fcast$fitted);
    ln <- nrow(Mkt)
    lw <- nrow(fitted.data)
    Mkt_test_df <- Mkt[(ln-lw+1):ln, ]
    Mkt_test_df <- cbind(Mkt_test_df, fitted.data)
    colnames(Mkt_test_df) <- c("Date", "Open", "High", "Low", "Close", "p")
    if(ts1 == TRUE){
      a <- ts_1(Mkt_test_df, nm[i]) # System 1
    } else {
      a <- ts_2(Mkt_test_df, nm[i]) # System 2
    }
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1), ]
  return(df10)
}

# apply System 1 to the auto.arima data
res8 <- ts_1_fnc(fil, nm, TRUE)
res8[misc_col] <- 'ARIMA - System 1'

# Add to total results
total_res <- rbind(total_res, res8)

# apply System 2 to auto.arima data
res9 <- ts_1_fnc(fil, nm, FALSE)
res9[misc_col] <- 'ARIMA - System 2'

# Add to total results
total_res <- rbind(total_res, res9)


# ARIMA/ANN Model
# Retrieve prediction from RapidMiner
fil <- c("arima515_ann_AORD.csv",
         "arima515_ann_KLCI.csv",
         "arima515_ann_STI.csv")

# a. System 1
res10 <- ts_1_2_fnc_ar(fil, nm, TRUE)
res10[misc_col] <- 'ARIMA/ANN System 1'
# Add to total results
total_res <- rbind(total_res, res10)

# a. System 2
res11 <- ts_1_2_fnc_ar(fil, nm, FALSE)
res11[misc_col] <- 'ARIMA/ANN System 2'
# Add to total results
total_res <- rbind(total_res, res11)


# ARIMA/k-NN Model
# retrieve prediction from RapidMiner
fil <- c("arima515_knn_KLCI.csv",
         "arima515_knn_STI.csv",
         "arima515_knn_AORD.csv")

# a. System 1
res12 <- ts_1_2_fnc_ar(fil, nm, TRUE)
res12[misc_col] <- 'ARIMA/k-NN System 1'
# Add to total results
total_res <- rbind(total_res, res12)

# a. System 2
res13 <- ts_1_2_fnc_ar(fil, nm, FALSE)
res13[misc_col] <- 'ARIMA/k-NN System 2'

# Add to total results
total_res <- rbind(total_res, res13)


# View the results
View(total_res)

#END