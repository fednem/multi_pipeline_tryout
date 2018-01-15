#for each modality user mmust enter a vector with three elements: path to the directory where images are stored, name of the mask to use (that 
#must be in the same directory, pattern to search for the files (e.g. "s8" or "ressampled"), everything as string, these vectors must be named
#arguments, where the name is the final name the user want to be given to the matrix )

source("helper_functions.R")
library(tidyverse)

extract_and_normalize_matrix <- function(...) {
  
  arguments <- list(...)
  
  for (arg in 1:length(arguments)) {
    
    info <- reshape_images_for_pipeline(arguments[[arg]][1], arguments[[arg]][2], arguments[[arg]][3])
    matrix <- info$n_by_v_matrix
    matrix <- normalize_matrix_range(matrix)
    img_dim <- info$dim_img
    out <- list(matrix = matrix, img_dim = img_dim)
    assign(names(arguments)[arg], out)
  }
  
  return(mget(names(arguments)))
}

