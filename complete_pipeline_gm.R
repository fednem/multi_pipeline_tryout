source("reshape_images_for_pipeline.s")
source("sd_thresholding_for_categorical_outcome_variables_vec.s")
source("select_features_relieff_derivatives_threshold_CORElearn.s")
library(tidyverse)
library(CORElearn)

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_matrix <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/gm", "gm_mask.nii.gz", "s8")

nuisance_and_outcome_variables <- read_delim("E:/multi_pipeline_tryout-improvement_on_relieff/gm/nuisance_and_outcome_variables.txt",delim = "\t")

#extract outcome variable
outcome <- nuisance_and_outcome_variables %>%
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)

gm_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(gm_matrix, .25)

gm_var_thr$outcome <- as.factor(outcome$outcome)

start_time <- Sys.time()
gm_relieff <- select_features_relieff_derivatives_threshold_CORElearn(gm_var_thr, "outcome", estimator = "ReliefFequalK")
end_time <- Sys.time()
end_time - start_time



