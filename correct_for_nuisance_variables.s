library(purrr)
library(tidyverse)
residuals_on_a_vector <- function(vec, iv_as_a_dataframe, formula = NULL) {
  if (is.null(formula)) {formula <- as.formula("dependent ~ .")} else {formula <- as.formula(paste("dependent ~ ",formula))}
  df <- iv_as_a_dataframe
  df$dependent <- vec
  model <- lm(formula = formula, data = df)
  res <- residuals(model)
  return(res)
}

residuals_on_a_dataframe <- function(df, iv_as_a_dataframe, formula = NULL) {
  
  out <- map(df,residuals_on_a_vector, iv_as_a_dataframe, formula)
  return(as_data_frame(out))
  
}