require(tsibble)
require(tidyverse)
require(feasts)
require(scoringRules)
require(forecast)
require(car)
require(mltools)
require(urca)

## reading the data
df <- read.csv("./data/stock_series_train.csv")
df$Date <- as.Date(df$Date,"%m/%d/%Y")
## cleaning stock 6 and 21
df[df$stock_id==6,]$Date <- as.Date(df[df$stock_id==6,]$Date) - 3
df[df$stock_id==21 & df$Date=='2019-10-28',]$Date <- as.Date(df[df$stock_id==21 & df$Date=='2019-10-28',]$Date) + 4
## convert to a tsibble 
stocks <- as_tsibble(df, index = Date, key =stock_id)

is_stationary <- stocks %>% features(Close, unitroot_kpss)
diff_required <- stocks %>% features(Close, unitroot_ndiffs)

min_date = min(stocks$Date)
max_date = max(stocks$Date)
stocks <- stocks %>% group_by(stock_id) %>% complete(Date = seq.Date(min_date, max_date, by="week"))

##pivot stocks to be in a suitable format
stocks <- stocks %>% 
  select(stock_id, Date, Close) %>%
  group_by(stock_id) %>% 
  mutate(n = 1:n()) %>% 
  spread(stock_id, Close) %>% 
  select(-n)

stocks_train <- stocks %>% filter(Date<'2019-11-01')
stocks_test <- stocks %>% filter(Date>='2019-11-01')

##handling na values
stocks_train <- stocks_train %>% 
  fill(-'Date') %>% #default direction down
  fill(-'Date',.direction = "updown")

## check johanson test of cointegration. Not cointegrated can use var
summary(ca.jo(select(stocks,2,3,4,5), type="trace", K=2, ecdet="none", spec="longrun"))


##requires differencing to make it stationary
num_diff <- 1 #single differencing for most of the series
stocks_train_diff <- stocks_train #%>% 
  #select(-c('16','17','18','19','20','21')) %>% 
  #drop_na() ##only checking for some stocks right now, how to tackle NA
#stocks_train_diff <- stocks_train %>% drop_na() 

stocks_train_diff <- diff(as.matrix(select(stocks_train_diff,-Date)), differences = num_diff)


require(vars) ##installing it later because it clashes with dplyr select
var_model <- VAR(stocks_train_diff, lag.max = 2)

future <- predict(var_model, n.ahead=14)

### testing for two stocks
just_one <- data.frame(future$fcst$X8)
test_one <- just_one$fcst
#last value of train set
pred_one <- 133.54 + cumsum(test_one)
crps_sample(stocks_test$`8`, matrix(pred_one,nrow = length(pred_one),ncol=1))

base_one <- rep(133.54, 14)
crps_sample(stocks_test$`8`, matrix(base_one,nrow = length(base_one),ncol=1))

future <- predict(var_model, n.ahead=13)
just_one <- data.frame(future$fcst$X6)
test_one <- just_one$fcst
#last value of train set
pred_one <- 53.24 + cumsum(test_one)
crps_sample(stocks_test$`6`[1:13], matrix(pred_one,nrow = length(pred_one),ncol=1), )

base_one <- rep(53.24, 13)
crps_sample(stocks_test$`6`[1:13], matrix(base_one,nrow = length(base_one),ncol=1))

  