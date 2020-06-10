require(tsibble)
require(tidyverse)
require(feasts)
require(fpp3)
require(scoringRules)
require(forecast)
require(mltools)
library(randomForest)
require(xgboost)
library(mlr)

## reading the data
df <- read.csv("./data/stock_series_train.csv")
df$Date <- as.Date(df$Date,"%m/%d/%Y")
## cleaning stock 6 and 21
df[df$stock_id==6,]$Date <- as.Date(df[df$stock_id==6,]$Date) - 3
df[df$stock_id==21 & df$Date=='2019-10-28',]$Date <- as.Date(df[df$stock_id==21 & df$Date=='2019-10-28',]$Date) + 4
## convert to a tsibble 
stocks <- as_tsibble(df, index = Date, key = stock_id)

stocks %>% autoplot(Close)

## filling in NAs for missing dates of stocks and cleaning the data
min_date = min(stocks$Date)
max_date = max(stocks$Date)
stocks <- stocks %>% group_by(stock_id) %>% complete(Date = seq.Date(min_date, max_date, by="week"))

stocks <- stocks %>% filter(Date < '2019-11-01') 

###in case you want to use log transformation uncomment this
# stocks_new1 <- stocks %>% select(-c(Volume, Low, High)) %>%
#   group_by(stock_id) %>%
#   mutate(#log_close = log(Close),
#     week = row_number()
#   )
#stocks$Close <- log(stocks$Close)
### creating new features for gradient boosting

stocks_new <- stocks %>% select(-c(Volume, Low, High)) %>% 
  group_by(stock_id) %>%
  mutate(
    last_week_close = lag(Close, order_by = stock_id),
    last_1_week_close = lag(last_week_close),
    last_2_week_close = lag(last_1_week_close),
    week = row_number(),
    moy = month(Date)
  ) 


xgb_iteration <- function(stocks_train, stocks_test, s){
  
  X_train <- as.data.frame(stocks_train %>% select(stock_id, 
                                                   week, 
                                                   last_week_close,
                                                   last_1_week_close,
                                                   last_2_week_close,
                                                   moy
  ))
  X_test <- as.data.frame(stocks_test %>% select(stock_id,
                                                 week, 
                                                 last_week_close,
                                                 last_1_week_close,
                                                 last_2_week_close,
                                                 moy
  ))
  y_train <- as.data.frame(stocks_train %>% select(stock_id, Close))
  y_test <- as.data.frame(stocks_test %>% select(stock_id, Close))
  
  param <- list(booster = "gblinear",
                objective = "reg:linear",
                subsample = 0.3
  )
  
  X_train[is.na(X_train)]<-(-1)
  y_train[is.na(y_train)]<-(-1)
  xgbm <- xgboost(
    as.matrix(X_train),
    y_train$Close,
    nrounds = 500,
    missing = -1,
    params = param,
    num_threads = 2,
    verbose = 0,
    seed = s
  )
  p <- predict(xgbm, as.matrix(X_test))
  error <- rmsle(p, y_test$Close, na.rm = TRUE)
  
  return(list(y_pred =p, error = error))
}

##predicting for 5 last points before 2019-11-01
n_test = 4
n_week = 196
n_paths = 5
#rudimentary way of handling NAs
stocks_new <- stocks_new %>% group_by(stock_id) %>%
  fill(-c('stock_id','Date')) %>% #default direction down
  fill(-c('stock_id','Date'),.direction = "updown") %>% ungroup()

###uncomment for log close
#stocks_tester <- stocks_new1 %>% filter(week >= n_week)
stocks_tester <- stocks_new %>% filter(week >= n_week)

##initialize final matrix of all sample paths
mat_final1 = matrix(numeric(105*n_paths), nrow = 105, ncol = n_paths)

for (j in 1:n_paths){
  print(j)
  mean_err_x = rep(NA, n_test)
  mean_crps_x = rep(NA, n_test)
  mat_sample = matrix(numeric(21*5), nrow = 21, ncol = 5)
  for (w in n_week:(n_test+n_week)){
    stocks_train <- stocks_new %>% filter(week < n_week)
    stocks_test <- stocks_new %>% filter(week == w)
    
    err <- xgb_iteration(stocks_train, stocks_test, s=j+3) #running on all stocks
    y_pred <- err$y_pred
    mat_sample[,(w+1)-n_week] = y_pred
  }
  mat_final1[,j] = as.vector(t(mat_sample))
}

###uncomment for log data
#mat_final <- exp(mat_final)
final <- stocks_tester$Close

mean(crps_sample(final, mat_final1))

### ALLLLL DATA last 5 points before prediction period xgboost 
## without log tranforms
##lags 3 and nrounds = 50 
## crps = 7.18

##lags 3 and ntrees = 50 and 6s and moy
## crps = fails

##lags 3 and ntrees = 100 and 6s and moy
## crps = 7.38

### with log tranforms

##lags 3 and ntrees = 50 and 6s and log transform
## crps = 

##lags 3 and ntrees = 50 and 6s and moy and log
## crps = 

##lags 3 and ntrees = 100 and 6s and moy and log
## crps = 



#write.csv(mat_final,"samples.csv")


########### code for cv 
############ checking for optimal number of rounds
# xgbcv <- xgb.cv(params=param, data = as.matrix(X_train), label = y_train$Close,
#                 nrounds = 200, nfold = 5, showsd = T, stratified = T, 
#                 print_every_n = 10, early_stop_round = 20, maximize = F)



