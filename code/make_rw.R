#' Generates a random walk prediction of length n with log-t likelihood from a
#' given time series
#' 
#' @description 
#' The make_rw() function gnerates a random walk of length num_steps with 
#' log-t likelihood.
#' The degrees of freedom value df is derived from the input time series ts
#' using maximum log likelihood estimation (MLE)
#'
#' @param ts Time series to predict.
#' @param num_steps Number of steps to predict.
#' 

make_rw <- function(ts, num_steps) {
  
  # Center data
  ts0 <- scale(ts, scale = TRUE)
  
  # Function to calculate the log-likelihhod
  LL <- function(df) sum(dt(ts0, df, log = TRUE))
  
  # Derive degress of freedom df
  max_LL <- optimize(LL, interval = c(1, length(ts0)-1), maximum = TRUE)
  df <- round(max_LL$maximum)
  
  # Generate noise terms from t distribution
  x <- rt(num_steps, df)
  
  # Generate random walk
  rw <- cumsum(x)
  
  # Add start point of walk to noise terms
  rw <- rw + tail(ts, 1)
  
  return(rw)
}
