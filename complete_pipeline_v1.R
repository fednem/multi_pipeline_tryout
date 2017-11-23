source("reshape_images_for_pipeline.s")
source("correct_for_nuisance_variables.s")
library(tidyverse)
library(foreach)
library(doParallel) 
library(magrittr)
source("sd_thresholding_for_continuous_outcome_variables.s")
source("select_features_relieff_derivatives_threshold_CORElearn.s")
source("scale_data_frame.s")

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/gm/", "gm_mask.nii.gz", "s8")
# wm_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/wm/", "wm_mask.nii.gz", "s8")
# rs_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/rs_dmn_mpfc/", "rs_mask.nii.gz", "resampled")
print("finished preparing matrix")
#factor out nuisance variable
#read nuisance variables file
print("preparing variables")
nuisance_and_outcome_variables <- read_delim("D:/multi_pipeline_tryout_imaging/nuisance_and_outcome_variables.txt",delim = "\t")

#select nusaince for rs
nuisance_variables_rs <- nuisance_and_outcome_variables %>%
  mutate(centre = if_else(centre_1a == 1, "pre-upgrade", "post_upgrade")) %>%
  select(., centre, QA_InvalidScans, QA_MeanMotion, QA_MeanGlobal, QA_GCOR_rest)

#select nuisance for structural
nuisance_variables_struct <- nuisance_and_outcome_variables %>%
  mutate(centre = if_else(centre_1a == 1, "pre-upgrade", "post_upgrade")) %>%
  select(., centre)
print("finished preparing variables")

#extract outcome variable 
outcome <- nuisance_and_outcome_variables %>% 
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)



#GM features selection steps
print("variance thresholding")
gm_thr_var_outcome <- sd_thresholding_for_continuous_outcome_variables(gm_matrix, .25)

gm_for_analysis <- gm_thr_var_outcome %>%
  scale_data_frame(., seq(1,length(.)))


features_with_na_gm <- (sapply(lapply(gm_for_analysis,is.na), sum) != 0) %>% which()

if (length(features_with_na_gm) != 0) {
gm_for_analysis <- gm_for_analysis %>%
  select(., -(one_of(names(features_with_na_gm)))) }

gm_for_analysis$outcome <- outcome$outcome
print("finished variance thresholding")

print("relieff selection")
gm_for_analysis_after_relieff <- select_features_relieff_derivatives_threshold(gm_for_analysis, "outcome")
print("finished relieff selection")



wm_thr_var_outcome <- sd_thresholding_for_continuous_outcome_variables(wm_matrix_res, .25)
rs_thr_var_outcome <- sd_thresholding_for_continuous_outcome_variables(rs_matrix_res, .25)



#NB MOVE THIS STEP AT THE VERY END, when you will have an infinite amount less of features!
#take residuals over nuisance
# gm_matrix_res <- residuals_on_a_dataframe(gm_matrix, nuisance_variables_struct)
# wm_matrix_res <- residuals_on_a_dataframe(wm_matrix, nuisance_variables_struct)
# rs_matrix_res <- residuals_on_a_dataframe(rs_matrix, nuisance_variables_rs)

SMO_classifier <- make_Weka_classifier("weka/classifiers/functions/SMO")