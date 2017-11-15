#this function perform feature selection by means of std thresholding
#following the method outlined in Meng et al., 2017, NeuroImage
library(magrittr)
sd_thresholding <- function(df, outcome) {

  mean_outcome <- mean(outcome)
  sd_outcome <- sd(outcome)
  mean_each_features <- df %>% 
    lapply(.,mean)
  
sd_threshold_for_each_feature <- sapply(mean_each_features, function(x) 0.5 * (sd_outcome/mean_outcome) 
                       * x)

sd_each_features <- sapply(df, sd)

features_above_treshold <- which(sd_each_features > sd_threshold_for_each_feature)

output_df <- df %>%
  select(., features_above_treshold)

return(output_df)
}