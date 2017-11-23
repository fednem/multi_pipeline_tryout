#this function perform feature selection by means of variance thresholding
#it retains only those features that have variance above the quant quantile
library(magrittr)
library(dplyr)


var_vectorized <- function(mat) {
  
  mean_r <- colSums(mat)/nrow(mat)
  mat_sub_mean <- (t(mat) - mean_r)^2
  var_r <- rowSums(mat_sub_mean)/(ncol(mat_sub_mean)-1)
}

sd_thresholding_for_categorical_outcome_variables_vec <- function(df, quant) {

  
var_each_features <- var_vectorized(df)

var_thr <- quantile(var_each_features, quant)

features_above_treshold <- which(var_each_features > var_thr)

output_df <- df %>%
  select(., features_above_treshold)

return(output_df)
}