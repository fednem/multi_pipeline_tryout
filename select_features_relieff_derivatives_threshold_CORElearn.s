library(tidyverse)
library(CORElearn)
library(numDeriv)
library(quantmod)



deriv <- function(x,y) {
  output <- diff(y)/diff(x)
  return(output)
}

middle_pts <- function(x){
  pts <- x[-1] - diff(x) / 2
  return(pts)
}

calculate_features_threshold_based_on_second_derivative <- function(x,y, to_plot = TRUE) {
  smoothed_y <- loess(y ~ x,
                      data.frame(y = y, x = x, model = T), span = .1) %>% predict(.)
  
  second_d <- deriv(middle_pts(x), deriv(x, smoothed_y))
  smooth_second_d <- loess(second_d ~ midpts,
                           data.frame(second_d = second_d, midpts = middle_pts(middle_pts(x))), model = T, span = .1)
  otp <- predict(smooth_second_d)
  thr <- y[findValleys(otp)[1]]
  
  if(to_plot) {
    plot(x,y)
    points(findValleys(otp)[1],y[findValleys(otp)[1]], pch = 15, cex = 2, col = "magenta")
  }
  return(list(smoothed_second_derivatives = smooth_second_d, second_derivative_values = otp, threshold = thr))
}


select_features_relieff_derivatives_threshold_CORElearn <- function(df, outcome, estimator) {
  
  
  
  print("Performing relieff algorithm")
  
  rf_weights <- attrEval(formula = ncol(df), df, estimator = estimator)
  
  print("Done relieff algorithm - calculating threshold")
  
  ordered_weights <- sort(rf_weights, decreasing = TRUE)
  
  thr <- calculate_features_threshold_based_on_second_derivative(seq(1,length(ordered_weights)), ordered_weights)$threshold
  
  selected_weights <- names(rf_weights) [rf_weights > thr]
  
  output <- df %>%
    select(one_of(c(outcome,selected_weights)))
  
}