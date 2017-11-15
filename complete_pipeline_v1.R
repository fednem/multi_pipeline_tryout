source("reshape_images_for_pipeline.s")
source("correct_for_nuisance_variables.s")

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix

gm_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/gm/", "gm_mask.nii.gz", "s8")
wm_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/wm/", "wm_mask.nii.gz", "s8")
rs_matrix <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/rs_dmn_mpfc/", "rs_mask.nii.gz", "resampled")

#factor out nuisance variable
#read nuisance variables file
nuisance_and_outcome_variables <- read_delim("D:/multi_pipeline_tryout_imaging/nuisance_and_outcome_variables.txt",delim = "\t")

#select nusaince for rs
nuisance_variables_rs <- nuisance_and_outcome_variables %>%
  mutate(centre = if_else(centre_1a == 1, "pre-upgrade", "post_upgrade")) %>%
  select(., centre, QA_InvalidScans, QA_MeanMotion, QA_MeanGlobal, QA_GCOR_rest)

#select nuisance for structural
nuisance_variables_struct <- nuisance_and_outcome_variables %>%
  mutate(centre = if_else(centre_1a == 1, "pre-upgrade", "post_upgrade")) %>%
  select(., centre)

#take residuals over nuisance
gm_matrix_res <- residuals_on_a_dataframe(gm_matrix, nuisance_variables_struct)
