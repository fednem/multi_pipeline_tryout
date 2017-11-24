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

gm_test <- gm_matrix[-c(1,2,22,23),]
gm_train <- gm_matrix[c(1,2,22,23),]

gm_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(gm_test, .25)


gm_var_thr$outcome <- as.factor(outcome$outcome[-c(1,2,22,23)])

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
  mutate(outcome = as.factor(outcome$outcome[-c(1,2,22,23)]))

gm_df_clusters_selected <- gm_df_clusters %>%
  select(., select.cfs(gm_df_clusters)$Index, outcome)



gm_test_selected <- gm_train %>%
  select(., colnames(gm_relieff[-1])) %>%
  mutate(subject = row_number()) %>%
  gather(., feature, value, -subject) %>%
  mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
  left_join(., select(coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
  mutate(cluster_id = as.character(cluster_id)) %>%
  filter(., cluster_id %in% head(colnames(gm_df_clusters_selected),-1)) %>%
  group_by(subject, cluster_id) %>%
  summarise(mean_of_cluster = mean(value)) %>%
  spread(cluster_id, mean_of_cluster) %>%
  ungroup(.) %>%
  select(-subject) %>%
  mutate(outcome = as.factor(outcome$outcome[c(1,2,22,23)]))

