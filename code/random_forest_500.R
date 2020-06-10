require(tsibble)
require(tidyverse)
require(feasts)
require(fpp3)
require(scoringRules)
require(forecast)
require(mltools)
library(randomForest)
require(xgboost)

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

### creating new features for gradient boosting
stocks_new <- stocks %>% select(-c(Volume, Low, High)) %>% 
  group_by(stock_id) %>%
  mutate(#log_close = log(Close),
    last_week_close = lag(Close, order_by = stock_id),
    last_1_week_close = lag(last_week_close),
    last_2_week_close = lag(last_1_week_close),
    week = row_number(),
    moy = month(Date)
  ) 


rf_iteration <- function(stocks_train, stocks_test){
  
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
  #randomforest regressor
  rf <- randomForest(x = as.matrix(X_train), 
                     y = y_train$Close,
                     num_threads = 2,
                     ntree = 50,
                     nodesize = 4,
                     verbose = -1)
  p <- predict(rf, as.matrix(X_test))
  error <- rmsle(p, y_test$Close, na.rm = TRUE)

  return(list(y_pred =p, error = error))
}



n_test = 13
n_week = 201
n_paths = 100
#rudimentary way of handling NAs
stocks_new <- stocks_new %>% group_by(stock_id) %>%
  fill(-c('stock_id','Date')) %>% #default direction down
  fill(-c('stock_id','Date'),.direction = "updown") %>% ungroup()

stocks_tester <- stocks_new %>% filter(week >= n_week)
##initialize final matrix of all sample paths
mat_final = matrix(numeric(294*n_paths), nrow = 294, ncol = n_paths)

for (j in 1:n_paths){
  print(j)
  mean_err_x = rep(NA, n_test)
  mean_crps_x = rep(NA, n_test)
  mat_sample = matrix(numeric(21*14), nrow = 21, ncol = 14)
  for (w in n_week:(n_test+n_week)){
    stocks_train <- stocks_new %>% filter(week < n_week)
    stocks_test <- stocks_new %>% filter(week == w)

    err <- rf_iteration(stocks_train, stocks_test) #running on all stocks
    y_pred <- err$y_pred
    mat_sample[,(w+1)-n_week] = y_pred
  }
  mat_final[,j] = as.vector(t(mat_sample))
}

### test for stock id 6 and 8
temp <- rbind(mat_final[71:84,], mat_final[99:112,])

final <- (stocks_tester %>% filter(stock_id==6 | stock_id ==8))$Close
final[14] = 48.77
mean(crps_sample(final, temp))

### write to a file

write.csv(mat_final,"final_rf.csv")

##lags 3 and ntrees = 20 and 4s
## crps = 2.5636

##lags 3 and ntrees = 50 and 6s
## crps = 2.33217

##lags 3 and ntrees = 100 and 12s
## crps = 2.63217

##lags 2 and ntrees = 200 and 15s
## crps = 3.0417

##lags 3 and ntrees = 200 and 21s
## crps = 2.684

##lags 2 and ntrees = 500 and 55s
## crps = 3.0724




