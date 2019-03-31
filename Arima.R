# Manual generation of ARIMA model

# 1. Read data, split into training and testing
Mkt <- read.csv("STI_cleaned.csv")
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, end=1499.99)
Mkt_test <- window(Mkt_ts, start=1500)

# 2. Check whether the data is stationary or not
plot.ts(Mkt_train)
adf.test(Mkt_train, alternative="stationary")
# 2a. If the data is not stationary, apply differencing
adf.test(diff(Mkt_train), alternative="stationary")
plot.ts(diff(Mkt_train))

#3. Plot acf and pacf to determine the order
plot(Acf(diff(Mkt_train), main=""))
plot(Pacf(diff(Mkt_train), main="")) 

#4. Try ARIMA Model and find AIC, AICC and BIC
mod_ar <- function(Mkt_ts, ord, nm){
  res <- t(as.data.frame(rep(0, 4)))
  mod <- Arima(Mkt_ts, order=ord)
  res[1, 1] <- nm
  res[1, 2] <- round(mod$aic, 1)
  res[1, 3] <- round(mod$aicc, 1)
  res[1, 4] <- round(mod$bic, 1)
  return(res)
}

results <- t(as.data.frame(rep(0, 4)))
colnames(results) <- c('Model', 'AIC', 'AICc', 'BIC')

res <- mod_ar(Mkt_train, c(2, 1, 1), 'ARIMA(2, 1, 1)')
results <- rbind(results, res)
res <- mod_ar(Mkt_train, c(2, 1, 2), 'ARIMA(2, 1, 2)')
results <- rbind(results, res)
res <- mod_ar(Mkt_train, c(2, 1, 3), 'ARIMA(2, 1, 3)')
results <- rbind(results, res)
results <- results[-1, ]

# 5. Apply into testing model
model_used_for_res <- Arima(Mkt_ts, order=c(2, 1, 2))
model_name <- forecast(model_used_for_res)$method

arima_man_fcast <- forecast(model_used_for_res, Mkt_test)
fitted.data <- as.data.frame(arima_man_fcast$fitted);
Mkt_test_df <- cbind(Mkt, fitted.data)
colnames(Mkt_test_df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Forecast')

# 6. View forecast result
View(Mkt_test_df)