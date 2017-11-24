#capture output
library(stringr)
library(tidyverse)

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

