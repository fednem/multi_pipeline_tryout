source("reshape_images_for_pipeline.s")
source("correct_for_nuisance_variables.s")
library(tidyverse)
library(foreach)
library(doParallel) 
library(RWeka)
library(magrittr)
library(Biocomb)
library(FNN)
source("sd_thresholding_for_continuous_outcome_variables.s")
source("calculate_features_threshold_based_on_second_derivative_fselector.s")
source("scale_data_frame.s")

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_matrix <- reshape_images_for_pipeline("/mnt/d/multi_pipeline_tryout_imaging/gm/", "gm_mask.nii.gz", "s8")
gm_matrix_selected <- gm_matrix[,sample(ncol(gm_matrix), floor(ncol(gm_matrix)*.5))]

nuisance_and_outcome_variables <- read_delim("/mnt/d/multi_pipeline_tryout_imaging/nuisance_and_outcome_variables.txt",delim = "\t")

#extract outcome variable 
outcome <- nuisance_and_outcome_variables %>% 
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)

gm_matrix_selected$outcome <- as.factor(outcome$outcome)

ReliefF_eval <- make_Weka_attribute_evaluator("weka/attributeSelection/ReliefFAttributeEval")

start_time <- Sys.time()
aa <- ReliefF_eval(outcome ~ ., gm_matrix_selected, control = Weka_control(M = 10, K = 5))
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
aa <- attrEval(formula = 69904, gm_matrix, estimator = "ReliefFequalK")
end_time <- Sys.time()
end_time - start_time


# start_time_complete <- Sys.time()
# aa <- ReliefF_eval(outcome ~ ., gm_matrix, control = Weka_control(M = 10, K = 5))
# end_time_complete <- Sys.time()
# end_time_complete - start_time_complete
