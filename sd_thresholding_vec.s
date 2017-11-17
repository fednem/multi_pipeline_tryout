#this function perform feature selection by means of std thresholding
#following the method outlined in Meng et al., 2017, NeuroImage
library(magrittr)
library(dplyr)

var_vectorized <- function(mat) {
  
  mean_r <- colSums(mat)/nrow(mat)
  mat_sub_mean <- (t(mat) - mean_r)^2
  var_r <- rowSums(mat_sub_mean)/(ncol(mat_sub_mean)-1)
}


sd_thresholding_vec <- function(df, outcome) {

  mean_outcome <- mean(outcome)
  sd_outcome <- sd(outcome)
  
  
  sd_threshold_for_each_feature <- 0.5 * (sd_outcome/mean_outcome) * mean(as.matrix(df))
  
  
  sd_each_features <- sqrt(var_vectorized(df))

  features_above_treshold <- which(sd_each_features > sd_threshold_for_each_feature)

  output_df <- df %>%
    select(., features_above_treshold)

  return(output_df)
}