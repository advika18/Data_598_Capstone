require(tsibble)
require(tidyverse)
require(feasts)
require(fpp3)
require(scoringRules)
require(forecast)
require(car)
require(mltools)
library(lightgbm)

## reading the data
df <- read.csv("./data/stock_series_train.csv")
df$Date <- as.Date(df$Date,"%m/%d/%Y")
## cleaning stock 6 and 21
df[df$stock_id==6,]$Date <- as.Date(df[df$stock_id==6,]$Date) - 3
df[df$stock_id==21 & df$Date=='2019-10-28',]$Date <- as.Date(df[df$stock_id==21 & df$Date=='2019-10-28',]$Date) + 4
## convert to a tsibble 
stocks <- as_tsibble(df, index = Date, key =stock_id)

stocks %>% autoplot(Close)

is_stationary <- stocks %>% features(Close, unitroot_kpss)
diff_required <- stocks %>% features(Close, unitroot_ndiffs)

## filling in NAs for missing dates of stocks and cleaning the data
min_date = min(stocks$Date)
max_date = max(stocks$Date)
stocks <- stocks %>% group_by(stock_id) %>% complete(Date = seq.Date(min_date, max_date, by="week"))


lgb_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w) %>% drop_na()
  test <- stocks %>% filter(week == w)
  
  X_train <- as.data.frame(train %>% select(stock_id, week, log_close, last_week_close, 
                                            last_week_diff, last_1_week_close, last_1_week_diff))
  X_test <- as.data.frame(test %>% select(stock_id, week, log_close, last_week_close, 
                                          last_week_diff, last_1_week_close, last_1_week_diff))
  y_train <- as.data.frame(train %>% select(stock_id, Close))
  y_test <- as.data.frame(test %>% select(stock_id, Close))
  #LightGBM regressor
  lgbm <- lightgbm(as.matrix(X_train), 
                   y_train$Close,
                   nrounds = 100, 
                   objective='regression', 
                   num_threads = 2,
                   verbose = -1)
  p <- predict(lgbm, dat = as.matrix(X_test))
  error <- rmsle(p, y_test$Close, na.rm = TRUE)
  return(error) 
}

baseline_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w)
  test <- stocks %>% filter(week == w)
  p <- test$last_week_close
  error <- rmsle(p, test$Close, na.rm = TRUE)
  return(error)
}

light_gradient_boosting <- function(stocks){
  stocks <- stocks %>% 
    group_by(stock_id) %>%
    mutate(log_close = log(Close),
           last_week_close = lag(Close, order_by = stock_id),
           last_week_diff = Close - last_week_close,
           last_1_week_close = lag(last_week_close),
           last_1_week_diff = last_week_close - last_1_week_close,
           week = row_number()
    ) 
  ## baseline metric
  n_test = 13
  n_week = 201
  mean_err = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    error <- baseline_iteration(stocks, w)
    print(paste('Week ', w, "error: ", error))
    mean_err[w-(n_week+1)] = error
  }
  print(paste('Baseline Mean Error = ', mean(mean_err)))
  
  ###filtering stocks because the rest of them have NAs
  stocks <- stocks %>% filter(stock_id==6 | stock_id ==8)
  mean_err_l = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    error <- lgb_iteration(stocks, w)
    print(paste('Week ', w, "error: ", error))
    mean_err_l[w-(n_week+1)] = error
  }
  print(paste('lgbm Mean Error = ', mean(mean_err_l)))
  
}

light_gradient_boosting(stocks)






#### individual ets or arima
# stock_2 <- stocks %>%
#   filter(stock_id==2)
# 
# fit <- stock_2 %>% filter(year(Date)<='2018') %>%
#   model(arima = ARIMA(Close))
# report(fit)
# 
# fore <- fit %>% forecast(h=43)
# fore %>% autoplot(stock_2) + autolayer(filter(stocks,stock_id==2,year(Date)==2019))
# accuracy(fore,stock_2)
# 
# crps_sample(stock_2$Close[158:200],matrix(fore$Close,nrow = 43,ncol=1))
# calc_crps = function(actual_path, sample_paths) {
#   
#   # Compute accuracy term
#   acc_diff <- t(apply(sample_paths, 1, function(x) x - actual_path))
#   acc_norm <- apply(acc_diff, 1, function(x) norm(x, "2"))
#   acc_term <- mean(acc_norm)
#   
#   # Compute dispersion term
#   combinations <- expand.grid(rep(list(1:nrow(sample_paths)), 2))
#   dis_diff <- t(apply(combinations, 1,
#                       function(x) sample_paths[x[1], ] - sample_paths[x[2], ]))
#   dis_norm <- apply(dis_diff, 1, function(x) norm(x, "2"))
#   dis_term <- mean(dis_norm)
#   
#   # Compute CRPS
#   crps = acc_term - 1/2 * dis_term
#   
#   return(crps)
#   
# }
# 
# x <- rep(NA,43)
# for (i in 1:43){
#   x[i] <- calc_crps(stock_2$Close[i+157], matrix(fore$Close[i],nrow = 1,ncol=1))
# }
# print(x)






