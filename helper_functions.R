#load library
library(stringr)
library(tidyverse)
library(purrr)
library(oro.nifti)
library(neurobase)
library(CORElearn)
library(numDeriv)
library(quantmod)


#correct for nuisance variables
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


#extract weights from different models
extract_weights_from_SMO <- function(model) {
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  raw_output <- capture.output(model)
  trimmed_output <- raw_output[-c(1:11,(length(raw_output) - 4): length(raw_output))]
  df <- data_frame(features_name = vector(length = length(trimmed_output) + 1, "character"), 
                   features_weight = vector(length = length(trimmed_output) + 1, "numeric"))
  
  for (line in 1:length(trimmed_output)) {
    
    
    string_as_vector <- trimmed_output[line] %>%
      str_split(string = ., pattern = " ") %>%
      unlist(.)
    
    
    numeric_element <- trimmed_output[line] %>%
      str_split(string = ., pattern = " ") %>%
      unlist(.) %>%
      as.numeric(.)
    
    position_mul <- string_as_vector[is.na(numeric_element)] %>%
      str_detect(string = ., pattern = "[*]") %>%
      which(.)
    
    numeric_element <- numeric_element %>%
      `[`(., c(1:position_mul))
    
    text_element <- string_as_vector[is.na(numeric_element)]
    
    
    there_is_plus <- string_as_vector[is.na(numeric_element)] %>%
      str_detect(string = ., pattern = "[+]") %>%
      sum(.)
    
    if (there_is_plus) { sign_is <- "+"} else { sign_is <- "-"}
    
    
    
    feature_weight <- numeric_element[!is.na(numeric_element)]
    
    if (sign_is == "-") {df[line, "features_weight"] <- feature_weight * -1} else {df[line, "features_weight"] <- numeric_element[!(is.na(numeric_element))]}
    
    df[line, "features_name"] <- paste(text_element[(position_mul + 1): length(text_element)], collapse = " ")
    
  }
  
  intercept_line <- raw_output[length(raw_output) - 4]
  
  
  there_is_plus_intercept <- intercept_line %>%
    str_detect(string = ., pattern = "[+]") %>%
    sum(.)
  
  if (there_is_plus_intercept) { intercept_sign_is <- "+"} else { intercept_sign_is <- "-"}
  
  numeric_intercept <- intercept_line %>%
    str_split(string = ., pattern = " ") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    `[`(., length(.))
  
  df[nrow(df), "features_name"] <- "intercept"
  
  if (intercept_sign_is == "-") {df[nrow(df), "features_weight"] <- numeric_intercept * -1} else {df[nrow(df), "features_weight"] <- numeric_intercept}
  
  options(warn = oldw)
  
  
  df <- df %>%
    arrange(desc(abs(features_weight)))
  
  return(df)
}

extract_weights_from_SMOreg <- function(model) {
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  raw_output <- capture.output(model)
  trimmed_output <- raw_output[-c(1:3,(length(raw_output) - 4): length(raw_output))]
  df <- data_frame(features_name = vector(length = length(trimmed_output) + 1, "character"), 
                   features_weight = vector(length = length(trimmed_output) + 1, "numeric"))
  
  for (line in 1:length(trimmed_output)) {
    
    
    string_as_vector <- trimmed_output[line] %>%
      str_split(string = ., pattern = " ") %>%
      unlist(.)
    
    
    numeric_element <- trimmed_output[line] %>%
      str_split(string = ., pattern = " ") %>%
      unlist(.) %>%
      as.numeric(.)
    
    position_mul <- string_as_vector[is.na(numeric_element)] %>%
      str_detect(string = ., pattern = "[*]") %>%
      which(.)
    
    numeric_element <- numeric_element %>%
      `[`(., c(1:position_mul))
    
    text_element <- string_as_vector[is.na(numeric_element)]
    
    
    there_is_plus <- string_as_vector[is.na(numeric_element)] %>%
      str_detect(string = ., pattern = "[+]") %>%
      sum(.)
    
    if (there_is_plus) { sign_is <- "+"} else { sign_is <- "-"}
    
    
    
    feature_weight <- numeric_element[!is.na(numeric_element)]
    
    if (sign_is == "-") {df[line, "features_weight"] <- feature_weight * -1} else {df[line, "features_weight"] <- numeric_element[!(is.na(numeric_element))]}
    
    df[line, "features_name"] <- paste(text_element[(position_mul + 1): length(text_element)], collapse = " ")
    
  }
  
  intercept_line <- raw_output[length(raw_output) - 4]
  
  
  there_is_plus_intercept <- intercept_line %>%
    str_detect(string = ., pattern = "[+]") %>%
    sum(.)
  
  if (there_is_plus_intercept) { intercept_sign_is <- "+"} else { intercept_sign_is <- "-"}
  
  numeric_intercept <- intercept_line %>%
    str_split(string = ., pattern = " ") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    `[`(., length(.))
  
  df[nrow(df), "features_name"] <- "intercept"
  
  if (intercept_sign_is == "-") {df[nrow(df), "features_weight"] <- numeric_intercept * -1} else {df[nrow(df), "features_weight"] <- numeric_intercept}
  
  options(warn = oldw)
  
  
  df <- df %>%
    arrange(desc(abs(features_weight)))
  
  return(df)
}

#normalize matrix range
normalize_matrix_range <- function(matrix) {
  range_mat <- range(matrix)
  new_mat <- (matrix - range_mat[1])/(range_mat[2] - range_mat[1])
  return(new_mat)
}

#reshape images
reshape_images_for_pipeline <- function (image_dir, mask, pattern_for_search) {
  
  this_wd <- getwd()
  setwd(image_dir)
  
  list <- dir(pattern = pattern_for_search)
  number_of_subjects <- length(list)
  print("reading mask")
  mask_struct <- readNIfTI2(mask)
  mask_sparse <- which(mask_struct@.Data > 0)
  dimension <- length(mask_sparse)
  n_by_v_matrix <- matrix(data = NA, nr = number_of_subjects, nc = Reduce(`*`, dimension))
  
  for (image_index in 1:number_of_subjects) {
    print(paste("reading and processing subject", image_index))
    img_struct <- readNIfTI2(list[image_index])
    n_by_v_matrix[image_index,] <- img_struct@.Data[mask_sparse]
    
  }
  
  colnames(n_by_v_matrix) <- paste("X", mask_sparse, sep = "")
  setwd(this_wd)
  n_by_v_matrix <- as_data_frame(n_by_v_matrix)
  list(n_by_v_matrix = n_by_v_matrix, dim_img = dim(mask_struct@.Data), img_struct = mask_struct)
  #print("returning")
  #return(n_by_v_matrix)
  
}

#scale dataframe
scale_data_frame <- function(df, cols, should_center = TRUE, should_scale = TRUE) {
  to_scale <- select(df, cols)
  scaled <- scale(to_scale, center = should_center, scale = should_scale)
  new_df <- df
  new_df[, cols] <- scaled
  new_df <- as_data_frame(new_df)
  return(new_df)
}

#sd thresholding for categorical outcome variable
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

#sd thresholding
sd_thresholding_vec <- function(df, outcome) {
  
  mean_outcome <- mean(outcome)
  sd_outcome <- sd(outcome)
  
  
  sd_threshold_for_each_feature <- 0.5 * (sd_outcome/mean_outcome) * mean(as.matrix(df))
  
  
  #sd_each_features <- sqrt(var_vectorized(df))
  
  features_above_treshold <- which(sd_each_features > sd_threshold_for_each_feature)
  
  output_df <- df %>%
    select(., features_above_treshold)
  
  return(output_df)
}

#relieff feat selection

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

