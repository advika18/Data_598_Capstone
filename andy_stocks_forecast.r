library(TSA)
library(forecast)
library(tseries)
library(scoringRules)

# load data and separate stocks
data = read.csv('stock_series_train.csv')
d_split <- split(data, data$stock_id)

# list to store time series for each stock
stocks = list()

# create a time series object for each stock
for (i in 1:length(d_split))
{
  temp = as.data.frame(d_split[i])
  dates = temp[[2]]
  
  # get the start date in numeric form
  start_date = toString(dates[1])
  start_date = as.Date(start_date, format = "%m/%d/%Y")
  s_year <- as.numeric(format(start_date, '%Y'))
  s_month <- as.numeric(format(start_date, '%m'))
  s_day <- as.numeric(format(start_date, '%d'))
  
  # get the end date in numeric form
  end_date = toString(dates[length(dates)])
  end_date = as.Date(end_date, format = "%m/%d/%Y")
  e_year <- as.numeric(format(end_date, '%Y'))
  e_month <- as.numeric(format(end_date, '%m'))
  e_day <- as.numeric(format(end_date, '%d'))
  
  # create and store time series object
  ts = ts(temp[[6]], start = c(s_year, s_month, s_day), end = c(e_year, e_month, e_day), frequency = 52)
  stocks[[i]] = ts
}


# display plots for all the stocks
par(mfrow = c(5, 5))
par(mar = c(1, 1, 1, 1))
for (i in 1:length(stocks))
{
  ts.plot(stocks[[i]])
}


# arima with selected fourier basis for each stock
v_dim = 1:7
models = list()
k = list()

for(i in 19:(length(d_split)-1))
{
  v_aic = c()
  stock = stocks[[i]]
  
  # find best basis
  for(v in v_dim){
    if (v > length(stock)){v = 1}
    fit = auto.arima(stock, xreg=fourier(stock, K=v), seasonal = FALSE) 
    v_aic = c(v_aic, AIC(fit))
  }
  
  k_best = v_dim[which(v_aic==min(v_aic))[1]]
  
  # model with best basis
  fit = auto.arima(stock, xreg=fourier(stock, K=k_best), seasonal = FALSE) 
  models = list(models, fit)
  k = list(k, k_best)
}


# Evaluate models with CRPS and combine
# PLACEHOLDER FORECASTING CODE
best_model = models[1]
k_best = k[1]
v_forecast = forecast(best_model, level =90, h=14, xreg=fourier(stocks[[1]], K=k_best, h=14))
lower = as.numeric(v_forecast$lower)
upper = as.numeric(v_forecast$upper)


