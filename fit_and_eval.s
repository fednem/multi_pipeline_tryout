fit_and_eval <- function(list_of_modalities, outcome, fold_to_evaluate) {
  
  out <- foreach(fold_index = fold_to_evaluate, .inorder = FALSE, 
                 .packages = c("tidyverse","dplyr", "CORElearn", "spatstat", "numDeriv", "quantmod", "Biocomb", "RWeka"),
                 .export = c("sd_thresholding_for_categorical_outcome_variables_vec", "select_features_relieff_derivatives_threshold_CORElearn",
                             "extract_weights_from_SMO", "SMO_classifier")) %do% {
                               
                               all_mods <- list()
                               
                               for (mod in 1:length(list_of_modalities)) {
                               
                               train <- list_of_modalities[[mod]]$matrix[fold != fold_index,]
                               test <- list_of_modalities[[mod]]$matrix[fold == fold_index,]
                               img_dim <- list_of_modalities[[mod]]$img_dim
                               name_of_mod <- names(list_of_modalities)[mod]
                                 
                               #variance thresholding
                               var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(train, .25)
                               
                               var_thr$outcome <- outcome_train
                               
                               #relieff
                               relieff <- select_features_relieff_derivatives_threshold_CORElearn(var_thr, "outcome", 
                                                                                                     estimator = "ReliefFequalK")
                               rm(var_thr)
                               
                               #coordinates finding 
                               coordinates_from_features_colnames <- relieff[,-1] %>%
                                 colnames(.) %>%
                                 str_split(., pattern = "X") %>%
                                 map_chr(~`[`(.,2)) %>%
                                 as.numeric(.) %>%
                                 arrayInd(., img_dim) %>%
                                 as_data_frame(.)
                               
                               coordinates_from_features_colnames$index <- relieff[,-1] %>%
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
                               rm(bb, object.pp3, object.pp3_labelled)
                               
                               #averaging of clusters
                               df_clusters <- relieff %>%
                                 select(., -outcome) %>%
                                 mutate(subject = row_number()) %>%
                                 gather(., feature, value, -subject) %>%
                                 mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
                                 left_join(., select(coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
                                 group_by(subject, cluster_id) %>%
                                 summarise(mean_of_cluster = mean(value)) %>%
                                 spread(cluster_id, mean_of_cluster) %>%
                                 ungroup(.) %>%
                                 select(-subject)
                               relieff_features <- colnames(relieff)[-1]
                               rm(relieff)
                               
                               #change names of features for easiness of recognition later on
                               colnames(df_clusters) <- paste(name_of_mod, colnames(df_clusters),sep ="_")
                               all_mods[[mod]] <- df_clusters
                               names(all_mods)[mod] <- name_of_mod}
                               
                               #####TILL HERE
                               
                               
                               
                               merged_modalities_df <- bind_cols(gm_df_clusters, wm_df_clusters, rs_df_clusters)
                               
                               merged_modalities_df$outcome <- outcome_train
                               
                               merged_modalities_df_selected <- merged_modalities_df %>%
                                 select(., select.cfs(merged_modalities_df)$Index, outcome)
                               
                               rm(merged_modalities_df)
                               
                               #cluster and select test set
                               
                               gm_test_selected <- gm_test %>%
                                 select(., gm_relieff_features) %>%
                                 mutate(subject = row_number()) %>%
                                 gather(., feature, value, -subject) %>%
                                 mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
                                 left_join(., select(gm_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
                                 group_by(subject, cluster_id) %>%
                                 summarise(mean_of_cluster = mean(value)) %>%
                                 spread(cluster_id, mean_of_cluster) %>%
                                 ungroup(.) %>%
                                 select(-subject)
                               
                               wm_test_selected <- wm_test %>%
                                 select(., wm_relieff_features) %>%
                                 mutate(subject = row_number()) %>%
                                 gather(., feature, value, -subject) %>%
                                 mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
                                 left_join(., select(wm_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
                                 group_by(subject, cluster_id) %>%
                                 summarise(mean_of_cluster = mean(value)) %>%
                                 spread(cluster_id, mean_of_cluster) %>%
                                 ungroup(.) %>%
                                 select(-subject)
                               
                               rs_test_selected <- rs_test %>%
                                 select(., rs_relieff_features) %>%
                                 mutate(subject = row_number()) %>%
                                 gather(., feature, value, -subject) %>%
                                 mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
                                 left_join(., select(rs_coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
                                 group_by(subject, cluster_id) %>%
                                 summarise(mean_of_cluster = mean(value)) %>%
                                 spread(cluster_id, mean_of_cluster) %>%
                                 ungroup(.) %>%
                                 select(-subject)
                               
                               colnames(gm_test_selected) <- paste("gm", colnames(gm_test_selected),sep ="_")
                               colnames(wm_test_selected) <- paste("wm", colnames(wm_test_selected),sep ="_")
                               colnames(rs_test_selected) <- paste("rs", colnames(rs_test_selected),sep ="_")
                               
                               
                               merged_modalities_df_test <- bind_cols(gm_test_selected, wm_test_selected, rs_test_selected) %>%
                                 select(., head(colnames(merged_modalities_df_selected),-1))
                               
                               
                               
                               model_SMO <- SMO_classifier(outcome ~ ., data = merged_modalities_df_selected)
                               
                               SMO_weights <- extract_weights_from_SMO(model_SMO)
                               
                               classification <- predict(model_SMO, merged_modalities_df_test)
                               
                               clusters_list <- list(gm = gm_coordinates_from_features_colnames, wm = wm_coordinates_from_features_colnames,
                                                     rs = rs_coordinates_from_features_colnames)
                               accuracy <- data_frame(classification = classification, ground = outcome_test)
                               centres <- list(centre_test = centre_test, centre_training = centre_train)
                               out <- list(clusters = clusters_list, accuracy = accuracy, weights = SMO_weights, centres = centres)
                               
                             }
  
}
  
  