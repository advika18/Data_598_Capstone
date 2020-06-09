require(tsibble)
require(tidyverse)
require(feasts)
require(fpp3)
require(scoringRules)
require(forecast)
require(car)
require(mltools)
library(lightgbm)
library(randomForest)
require(xgboost)

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


### creating new variables for gradient boosting and random forests
stocks_new <- stocks %>% 
  group_by(stock_id) %>%
  mutate(#log_close = log(Close),
         last_week_close = lag(Close, order_by = stock_id),
         #last_week_diff = Close - last_week_close,
         last_1_week_close = lag(last_week_close),
         week = row_number()
  ) 

lgb_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w) %>% drop_na()
  test <- stocks %>% filter(week == w)
  
  X_train <- as.data.frame(train %>% select(stock_id, week, last_week_close, 
                                            last_1_week_close))
  X_test <- as.data.frame(test %>% select(stock_id, week, last_week_close, 
                                          last_1_week_close))
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
  crps_error <- crps_sample(test$Close,matrix(p,nrow = length(p),ncol=1))
  return(list(error = error, crps_error = crps_error))
}


xgb_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w)
  test <- stocks %>% filter(week == w)
  
  X_train <- as.data.frame(train %>% select(stock_id, week, last_week_close, 
                                            last_1_week_close))
  X_test <- as.data.frame(test %>% select(stock_id, week, last_week_close, 
                                          last_1_week_close))
  y_train <- as.data.frame(train %>% select(stock_id, Close))
  y_test <- as.data.frame(test %>% select(stock_id, Close))
  
  param <- list(booster = "gblinear"
                , objective = "reg:linear"
  )
  
  X_train[is.na(X_train)]<-(-1)
  y_train[is.na(y_train)]<-(-1)
  xgbm <- xgboost(
    as.matrix(X_train),
    y_train$Close,
    nrounds = 100,
    missing = -1,
    params =
      param,
    num_threads = 2,
    verbose = 0
  )
  p <- predict(xgbm, as.matrix(X_test))
  error <- rmsle(p, y_test$Close, na.rm = TRUE)
  crps_error <- crps_sample(test$Close,matrix(p,nrow = length(p),ncol=1))
  return(list(error = error, crps_error = crps_error))
}

rf_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w) %>% drop_na()
  test <- stocks %>% filter(week == w)
  
  X_train <- as.data.frame(train %>% select(stock_id, week, last_week_close, 
                                            last_1_week_close))
  X_test <- as.data.frame(test %>% select(stock_id, week, last_week_close, 
                                          last_1_week_close))
  y_train <- as.data.frame(train %>% select(stock_id, Close))
  y_test <- as.data.frame(test %>% select(stock_id, Close))
  #randomforest regressor
  rf <- randomForest(x = as.matrix(X_train), 
                   y = y_train$Close,
                   num_threads = 2,
                   verbose = -1)
  p <- predict(rf, as.matrix(X_test))
  error <- rmsle(p, y_test$Close, na.rm = TRUE)
  crps_error <- crps_sample(test$Close,matrix(p,nrow = length(p),ncol=1))
  return(list(error = error, crps_error = crps_error))
}

baseline_iteration <- function(stocks, w){
  train <- stocks %>% filter(week < w)
  test <- stocks %>% filter(week == w)
  p <- test$last_week_close
  error <- rmsle(p, test$Close, na.rm = TRUE)
  crps_error <- crps_sample(test$Close,matrix(p,nrow = length(p),ncol=1))
  return(list(error = error, crps_error = crps_error))
}

run_methods <- function(stocks){
  ## baseline metric
  n_test = 12 #change to 13
  n_week = 201
  
  print("baseline")
  stocks_fil <- stocks %>% filter(stock_id == 6)
  #stocks_fil <- stocks %>% filter(stock_id==6 | stock_id ==8)
  
  mean_err = rep(NA, n_test)
  mean_crps = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    err <- baseline_iteration(stocks_fil, w)
    print(paste('Week ', w, "error: ", err$error, "crps: ", err$crps_error))
    mean_err[w-(n_week+1)] = err$error
    mean_crps[w - (n_week+1)] = err$crps_error
  }
  print(paste('Baseline Mean Error = ', mean(mean_err), ' Mean crps = ', mean(mean_crps)))

  ###lgbm method
  print("light gradient boosting")
  mean_err_l = rep(NA, n_test)
  mean_crps_l = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    err <- lgb_iteration(stocks_fil, w)
    print(paste('Week ', w, "error: ", err$error, "crps: ", err$crps_error))
    mean_err_l[w-(n_week+1)] = err$error
    mean_crps_l[w - (n_week+1)] = err$crps_error
  }
  print(paste('lgbm Mean Error = ', mean(mean_err_l), ' Mean crps = ', mean(mean_crps_l)))
  
  ### random forest regressor
  print("random forest regressor")
  mean_err_r = rep(NA, n_test)
  mean_crps_r = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    err <- rf_iteration(stocks_fil, w)
    print(paste('Week ', w, "error: ", err$error, "crps: ", err$crps_error))
    mean_err_r[w-(n_week+1)] = err$error
    mean_crps_r[w - (n_week+1)] = err$crps_error
  }
  print(paste('random forest Mean Error = ', mean(mean_err_r), ' Mean crps = ', mean(mean_crps_r)))

  ### xgboost
  print("xgboost")
  mean_err_x = rep(NA, n_test)
  mean_crps_x = rep(NA, n_test)
  for (w in n_week:(n_test+n_week)){
    err <- xgb_iteration(stocks_fil, w) #running on all stocks
    print(paste('Week ', w, "error: ", err$error, "crps: ", err$crps_error))
    mean_err_x[w-(n_week+1)] = err$error
    mean_crps_x[w - (n_week+1)] = err$crps_error
  }
  print(paste('xgboost Mean Error = ', mean(mean_err_x), ' Mean crps = ', mean(mean_crps_x)))
}

run_methods(stocks_new)

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
# crps_sample(stock_2$Close[158:200], matrix(fore$Close,nrow = 43,ncol=1))
# x <- rep(NA,43)
# for (i in 1:43){
#   x[i] <- calc_crps(stock_2$Close[i+157], matrix(fore$Close[i],nrow = 1,ncol=1))
# }
# print(x)






