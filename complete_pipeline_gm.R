source("reshape_images_for_pipeline.s")
source("sd_thresholding_for_categorical_outcome_variables_vec.s")
source("select_features_relieff_derivatives_threshold_CORElearn.s")
library(tidyverse)
library(CORElearn)
library(spatstat)
library(Biocomb)
#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_info <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/gm", "gm_mask.nii.gz", "s8")
gm_matrix <- gm_info$n_by_v_matrix
gm_img_dim <- gm_info$dim_img
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
coordinates_from_features_colnames$cluster_id <- marks(object.pp3_labelled)

gm_cluster_img <- gm_info$img_struct

gm_cluster_img@.Data <- array(0,gm_img_dim)

start_vec <- Sys.time()

for (cl in 1:nrow(coordinates_from_features_colnames)) {
  gm_cluster_img@.Data[as.matrix(coordinates_from_features_colnames[coordinates_from_features_colnames$cluster_id == cl, 1:3])] <- cl
}
end_vec <- Sys.time()


writeNIfTI(gm_cluster_img, "prova_gm_pipeline", verbose=TRUE)


gm_df_clusters <- gm_relieff %>%
  select(., -outcome) %>%
  mutate(subject = row_number()) %>%
  gather(., feature, value, -subject) %>%
  mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
  left_join(., select(coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
  group_by(subject, cluster_id) %>%
  summarise(mean_of_cluster = mean(value)) %>%
  spread(cluster_id, mean_of_cluster) %>%
  ungroup(.) %>%
  select(-subject) %>%
  mutate(outcome = outcome$outcome)

gm_df_clusters_selected <- gm_df_clusters %>%
  select(., select.cfs(gm_df_clusters)$Index, outcome)







test <- independent_variables_no_ol[Fold == fold_index,] %>%
  select(., colnames(independent_variables_thr_death_rate)) %>%
  scale_data_frame(., seq(1, length(.))) %>%
  select(., -(one_of((names(features_with_na))))) %>%
  select(., colnames(select(df_for_analysis_after_relieff, -outcome))) %>%
  mutate(country  = outcome_variables$CountryCode[Fold == fold_index]) %>%
  gather(., feature, value, -country) %>%
  left_join(., clusters_df, by = "feature") %>%
  group_by(country, cluster_index) %>%
  summarise(mean_of_cluster = mean(value)) %>%
  spread(cluster_index, mean_of_cluster) %>%
  ungroup(.) %>%
  select(-country) %>%
  select(colnames(select(df_clusters_selected, -outcome)))
