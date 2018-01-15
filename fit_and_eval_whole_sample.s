library(spatstat)
fit_and_eval <- function(list_of_modalities, outcome) {
  
  
  SMO_classifier <- make_Weka_classifier("weka/classifiers/functions/SMO")
  
  for (mod in 1:length(list_of_modalities)) {
    
     train <- list_of_modalities[[mod]]$matrix
     outcome_train <- outcome
     img_dim <- list_of_modalities[[mod]]$img_dim
     name_of_mod <- names(list_of_modalities)[[mod]]
    
     #variance thresholding
     print(paste("variance thresholding, modality is", name_of_mod, "modality", mod, "of", length(list_of_modalities), sep = " "))
     var_thr <- sd_thresholding_for_categorical_outcome_variables_vec(train, .25)
    
     var_thr$outcome <- outcome_train
    
     #relieff
     print(paste("relieff, modality is", name_of_mod, "modality", mod, "of", length(list_of_modalities), sep = " "))
     relieff <- select_features_relieff_derivatives_threshold_CORElearn(var_thr, "outcome", 
                    estimator = "ReliefFequalK")
     rm(var_thr)
    
     #coordinates finding 
     print(paste("coordinates finding, modality is", name_of_mod, "modality", mod, "of", length(list_of_modalities), sep = " "))
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
    
     print(paste("clustering, modality is", name_of_mod, "modality", mod, "of", length(list_of_modalities), sep = " "))
     coordinates_from_features_colnames <- cluster_voxels(coordinates_from_features_colnames)
    
    
     #averaging of clusters
     df_clusters <- relieff %>%
      select(., -outcome) %>%
      mutate(subject = row_number()) %>%
      gather(., feature, value, -subject) %>%
      mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
      inner_join(., select(coordinates_from_features_colnames, index, cluster_id), by = "index") %>%
      group_by(subject, cluster_id) %>%
      summarise(mean_of_cluster = mean(value)) %>%
      spread(cluster_id, mean_of_cluster) %>%
      ungroup(.) %>%
      select(-subject)
     
     relieff_features <- colnames(relieff)[-1]
     rm(relieff)
    
     #change names of features for easiness of recognition later on
     colnames(df_clusters) <- paste(name_of_mod, colnames(df_clusters),sep ="_")
     all_mods_train[[mod]] <- df_clusters
     names(all_mods_train)[mod] <- name_of_mod 
     all_relieff_features[[mod]] <- relieff_features
     names(all_relieff_features)[mod] <- name_of_mod 
     all_coordinates[[mod]] <- coordinates_from_features_colnames
     names(all_coordinates)[mod] <- name_of_mod }
    
     merged_modalities_df <- Reduce(bind_cols, all_mods_train)
    
     merged_modalities_df$outcome <- outcome_train
    
     merged_modalities_df_selected <- merged_modalities_df %>%
      select(., select.cfs(merged_modalities_df)$Index, outcome)
    
    model_SMO <- SMO_classifier(as.factor(outcome) ~ ., data = merged_modalities_df_selected)
    
    SMO_weights <- extract_weights_from_SMO(model_SMO)
    
    classification <- predict(model_SMO)
    
    
    accuracy <- data_frame(classification = classification, ground = outcome_test)
    out <- list(all_coordinates, accuracy = accuracy, weights = SMO_weights)
    
  }
  

  
  