source("reshape_images_for_pipeline.s")
source("sd_thresholding_for_categorical_outcome_variables_vec.s")
source("select_features_relieff_derivatives_threshold_CORElearn.s")
library(tidyverse)
library(CORElearn)

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_matrix <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/gm", "gm_mask.nii.gz", "s8")
gm_matrix <- gm_matrix$n_by_v_matrix
gm_img_dim <- gm_matrix$dim_img
nuisance_and_outcome_variables <- read_delim("E:/multi_pipeline_tryout-improvement_on_relieff/gm/nuisance_and_outcome_variables.txt",delim = "\t")

#extract outcome variable
outcome <- nuisance_and_outcome_variables %>%
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)

gm_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(gm_matrix, .25)


gm_var_thr$outcome <- as.factor(outcome$outcome)


gm_relieff <- select_features_relieff_derivatives_threshold_CORElearn(gm_var_thr, "outcome", estimator = "ReliefFequalK")

coordinates_from_features_colnames <- gm_relieff[,-1] %>%
  colnames(.) %>%
  str_split(., pattern = "X") %>%
  map_chr(~`[`(.,2)) %>%
  as.numeric(.) %>%
  arrayInd(., gm_img_dim) %>%
  as_data_frame(.)

coordinates_from_features_colnames$index <- gm_relieff[,-1] %>%
  colnames(.) %>%
  str_split(., pattern = "X") %>%
  map_chr(~`[`(.,2)) %>%
  as.numeric(.)

bb <- box3(range(coordinates_from_features_colnames[,1]),
           range(coordinates_from_features_colnames[,2]), range(coordinates_from_features_colnames[,3]))
object.pp3 <- pp3(coordinates_from_features_colnames$V1,
                  coordinates_from_features_colnames$V2, coordinates_from_features_colnames$V3, bb)
object.pp3_labelled <- connected(object.pp3, R = 1.8)
coordinates_from_features_colnames$cluste_id <- marks(object.pp3_labelled)

gm_cluster_img <- gm_matrix$img_struct
gm_cluster_img@.Data <- array(0,gm_img_dim)

start_vec <- Sys.time()

for (cl in 1:nrow(coordinates_from_features_colnames)) {
  gm_cluster_img@.Data[as.matrix(coordinates_from_features_colnames[coordinates_from_features_colnames$cluster_id == cl, 1:3])] <- cl
}
end_vec <- Sys.time()

end_vec - start_vec