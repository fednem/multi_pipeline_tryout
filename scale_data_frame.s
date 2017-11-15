library(tidyverse)

scale_data_frame <- function(df, cols, should_center = TRUE, should_scale = TRUE) {
  to_scale <- select(df, cols)
  scaled <- scale(to_scale, center = should_center, scale = should_scale)
  new_df <- df
  new_df[, cols] <- scaled
  new_df <- as_data_frame(new_df)
  return(new_df)
}