library(oro.nifti)
library(neurobase)
library(tidyverse)
library(stringr)


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
  #print("returning")
  #return(n_by_v_matrix)
  
}