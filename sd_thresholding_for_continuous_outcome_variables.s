#this function perform feature selection by means of variance thresholding
#it retains only those features that have variance above the quant quantile
library(magrittr)
sd_thresholding_for_continuous_outcome_variables <- function(df, quant) {

  
var_each_features <- sapply(df, var)

var_thr <- quantile(var_each_features, quant)

features_above_treshold <- which(var_each_features > var_thr)

output_df <- df %>%
  select(., features_above_treshold)

return(output_df)
}