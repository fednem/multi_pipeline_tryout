source("reshape_images_for_pipeline.s")
source("sd_thresholding_for_categorical_outcome_variables_vec.s")
source("select_features_relieff_derivatives_threshold_CORElearn.s")
source("extract_weights_from_SMO.s")
library(tidyverse)
library(CORElearn)
library(spatstat)
library(Biocomb)
library(doParallel) 
library(RWeka)

SMO_classifier <- make_Weka_classifier("weka/classifiers/functions/SMO")

#preparing all images modalities for following steps: i.e. reshape all images modalities to n by v matrix
print("preparing matrix")
gm_info <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/gm", "gm_mask.nii.gz", "s8")
gm_matrix <- gm_info$n_by_v_matrix
gm_img_dim <- gm_info$dim_img
gm_img_struct <- gm_info$img_struct
rm(gm_info)

wm_info <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/wm", "wm_mask.nii.gz", "s8")
wm_matrix <- wm_info$n_by_v_matrix
wm_img_dim <- wm_info$dim_img
wm_img_struct <- gm_info$img_struct
rm(wm_info)

rs_info <- reshape_images_for_pipeline("E:/multi_pipeline_tryout-improvement_on_relieff/rs_dmn_mpfc",
                                       "gm_mask.nii.gz", "resampled")
rs_matrix <- rs_info$n_by_v_matrix
rs_img_dim <- rs_info$dim_img
rs_img_struct <- gm_info$img_struct
rm(rs_info)

nuisance_and_outcome_variables <- read_delim("E:/multi_pipeline_tryout-improvement_on_relieff/gm/nuisance_and_outcome_variables.txt",delim = "\t")

#extract outcome variable
outcome <- nuisance_and_outcome_variables %>%
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)

#initalize fold and set things up for parallel computing
fold <- caret::createFolds(outcome$outcome, k = 10, list = FALSE)
cl <- makeCluster(10, type='PSOCK')
registerDoParallel(cl)

#start loop

out <- foreach(fold_index = 1:max(fold), .inorder = FALSE, 
               .packages = c("tidyverse","dplyr", "CORElearn", "spatstat", "numDeriv", "quantmod", "Biocomb", "RWeka"),
               .export = c("sd_thresholding", "select_features_relieff_derivatives_threshold_CORElearn",
                           "scale_data_frame", "extract_weights_from_SMO", "SMO_classifier")) %dopar% {
                             
  gm_train <- gm_matrix[fold != fold_index,]
  wm_train <- wm_matrix[fold != fold_index,]
  rs_train <- rs_matrix[fold != fold_index,]

  gm_test <- gm_matrix[fold == fold_index,]
  wm_test <- wm_matrix[fold == fold_index,]
  rs_test <- rs_matrix[fold == fold_index,]
  
  #variance thresholding
  gm_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(gm_train, .25)
  wm_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(wm_train, .25)
  rs_var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(rs_train, .25)

  gm_var_thr$outcome <- as.factor(outcome$outcome)
  wm_var_thr$outcome <- as.factor(outcome$outcome)
  rs_var_thr$outcome <- as.factor(outcome$outcome)

  #relieff
  gm_relieff <- select_features_relieff_derivatives_threshold_CORElearn(gm_var_thr, "outcome", 
                                                                      estimator = "ReliefFequalK")
  rm(gm_var_thr)
  wm_relieff <- select_features_relieff_derivatives_threshold_CORElearn(wm_var_thr, "outcome", 
                                                                      estimator = "ReliefFequalK")
  rm(wm_var_thr)
  rs_relieff <- select_features_relieff_derivatives_threshold_CORElearn(rs_var_thr, "outcome", 
                                                                      estimator = "ReliefFequalK")
  rm(rs_var_thr)

  #coordinates finding 
  gm_coordinates_from_features_colnames <- gm_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.) %>%
    arrayInd(., gm_img_dim) %>%
    as_data_frame(.)

  gm_coordinates_from_features_colnames$index <- gm_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.)

  gm_bb <- box3(range(gm_coordinates_from_features_colnames[,1]),
            range(gm_coordinates_from_features_colnames[,2]), range(gm_coordinates_from_features_colnames[,3]))
  gm_object.pp3 <- pp3(gm_coordinates_from_features_colnames$V1,
                    gm_coordinates_from_features_colnames$V2, gm_coordinates_from_features_colnames$V3, gm_bb)
  gm_object.pp3_labelled <- connected(gm_object.pp3, R = 1.8)
  gm_coordinates_from_features_colnames$cluster_id <- marks(gm_object.pp3_labelled)
  rm(gm_bb, gm_object.pp3, gm_object.pp3_labelled)

  wm_coordinates_from_features_colnames <- wm_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.) %>%
    arrayInd(., wm_img_dim) %>%
    as_data_frame(.)

  wm_coordinates_from_features_colnames$index <- wm_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.)

  wm_bb <- box3(range(wm_coordinates_from_features_colnames[,1]),
                range(wm_coordinates_from_features_colnames[,2]), range(wm_coordinates_from_features_colnames[,3]))
  wm_object.pp3 <- pp3(wm_coordinates_from_features_colnames$V1,
                      wm_coordinates_from_features_colnames$V2, wm_coordinates_from_features_colnames$V3, wm_bb)
  wm_object.pp3_labelled <- connected(wm_object.pp3, R = 1.8)
  wm_coordinates_from_features_colnames$cluster_id <- marks(wm_object.pp3_labelled)
  rm(wm_bb, gm_object.pp3, wm_object.pp3_labelled)
  
  rs_coordinates_from_features_colnames <- rs_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.) %>%
    arrayInd(., rs_img_dim) %>%
    as_data_frame(.)

  rs_coordinates_from_features_colnames$index <- rs_relieff[,-1] %>%
    colnames(.) %>%
    str_split(., pattern = "X") %>%
    map_chr(~`[`(.,2)) %>%
    as.numeric(.)

  rs_bb <- box3(range(rs_coordinates_from_features_colnames[,1]),
                range(rs_coordinates_from_features_colnames[,2]), range(rs_coordinates_from_features_colnames[,3]))
  rs_object.pp3 <- pp3(rs_coordinates_from_features_colnames$V1,
                      rs_coordinates_from_features_colnames$V2, rs_coordinates_from_features_colnames$V3, rs_bb)
  rs_object.pp3_labelled <- connected(rs_object.pp3, R = 1.8)
  rs_coordinates_from_features_colnames$cluster_id <- marks(rs_object.pp3_labelled)
  rm(rs_bb, rs_object.pp3, rs_object.pp3_labelled)

  #averaging of clusters
  gm_df_clusters <- gm_relieff %>%
    select(., -outcome) %>%
    mutate(subject = row_number()) %>%
    gather(., feature, value, -subject) %>%
    mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
    left_join(., select(gm_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
    group_by(subject, cluster_id) %>%
    summarise(mean_of_cluster = mean(value)) %>%
    spread(cluster_id, mean_of_cluster) %>%
    ungroup(.) %>%
    select(-subject)
  rm(gm_relieff)

  wm_df_clusters <- wm_relieff %>%
    select(., -outcome) %>%
    mutate(subject = row_number()) %>%
    gather(., feature, value, -subject) %>%
    mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
    left_join(., select(wm_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
    group_by(subject, cluster_id) %>%
    summarise(mean_of_cluster = mean(value)) %>%
    spread(cluster_id, mean_of_cluster) %>%
    ungroup(.) %>%
    select(-subject)
  rm(wm_relieff)

  rs_df_clusters <- rs_relieff %>%
    select(., -outcome) %>%
    mutate(subject = row_number()) %>%
    gather(., feature, value, -subject) %>%
    mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
    left_join(., select(rs_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
    group_by(subject, cluster_id) %>%
    summarise(mean_of_cluster = mean(value)) %>%
    spread(cluster_id, mean_of_cluster) %>%
    ungroup(.) %>%
    select(-subject)
  rm(rs_relieff)

  #change names of features for easiness of recognition later on
  colnames(gm_df_clusters) <- paste("gm", colnames(gm_df_clusters),sep ="_")
  colnames(wm_df_clusters) <- paste("wm", colnames(wm_df_clusters),sep ="_")
  colnames(rs_df_clusters) <- paste("rs", colnames(rs_df_clusters),sep ="_")

  merged_modalities_df <- bind_cols(gm_df_clusters, wm_df_clusters, rs_df_clusters)

  merged_modalities_df$outcome <- as.factor(outcome$outcome)

  merged_modalities_df_selected <- merged_modalities_df %>%
    select(., select.cfs(merged_modalities_df)$Index, outcome)



  model_SMO <- SMO_classifier(outcome ~ ., data = merged_modalities_df_selected)

  classification <- predict(model_SMO)



