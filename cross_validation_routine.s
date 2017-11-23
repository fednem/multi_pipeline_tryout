options(java.parameters = "-Xmx8000m")
library(tidyverse)
library(foreach)
library(doParallel) 
library(RWeka)
library(magrittr)
library(Biocomb)
library(FNN)
load("data_for_svr.RData")
source("sd_thresholding.s")
source("select_features_relieff_derivatives_threshold.s")
source("scale_data_frame.s")

SMOreg_classifier <- make_Weka_classifier("weka/classifiers/functions/SMOreg")
Fold <- caret::createFolds(1:nrow(independent_variables_no_ol), k = 10, list = FALSE)
cl <- makeCluster(10, type='PSOCK')
registerDoParallel(cl)


#parallel
start_parallel <- Sys.time()

out <- foreach(fold_index = 1:max(Fold), .inorder = FALSE, 
               .packages = c("tidyverse","dplyr", "FSelector", "numDeriv", "quantmod", "Biocomb"),
               .export = c("sd_thresholding", "select_features_relieff_derivatives_threshold",
                           "scale_data_frame")) %dopar% {

  deve <- independent_variables_no_ol[Fold != fold_index,]
  
  independent_variables_thr_death_rate <- sd_thresholding(deve, 
                                                         outcome_variables$`Death rate, crude (per 1,000 people)`[Fold != fold_index])
 
  df_for_analysis <- independent_variables_thr_death_rate %>%
    scale_data_frame(., seq(1,length(.)))
  
  
  features_with_na <- (sapply(lapply(df_for_analysis,is.na), sum) != 0) %>% which()
  
  df_for_analysis <- df_for_analysis %>%
    select(., -(one_of(names(features_with_na))))
  
  
  df_for_analysis$outcome <- outcome_variables$`Death rate, crude (per 1,000 people)`[Fold != fold_index]
  
  #IMPORTANT STEP 2: use relieff method and use derivatives method from Meng et al 2017 to find threshold
  
  df_for_analysis_after_relieff <- select_features_relieff_derivatives_threshold(df_for_analysis, "outcome")
  
  
  #IMPORTANT STEP 3: use csf evluation for subsetting
  set.seed(42)
  actual_clusters <- kmeans(as.matrix(t(select(df_for_analysis_after_relieff, -outcome))), centers = 10, nstart = 200)
  clusters_df <- data_frame(cluster_index = actual_clusters$cluster, feature = colnames(select(df_for_analysis_after_relieff, -outcome)))
  #put again the data in long format, add cluster index, summarise by country and clusters, back in wide format
  
  df_clusters <- df_for_analysis_after_relieff %>%
    select(., -outcome) %>%
    mutate(country  = outcome_variables$CountryCode[Fold != fold_index]) %>%
    gather(., feature, value, -country) %>%
    left_join(., clusters_df, by = "feature") %>%
    group_by(country, cluster_index) %>%
    summarise(mean_of_cluster = mean(value)) %>%
    spread(cluster_index, mean_of_cluster) %>%
    ungroup(.) %>%
    select(-country) %>%
    mutate(outcome = df_for_analysis$outcome)
  
   df_clusters_selected <- df_clusters %>%
    select(., select.cfs(df_clusters)$Index, outcome)
   
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
   
     
  
  model_SMOreg <- SMOreg_classifier(outcome ~ ., data = df_clusters_selected)
  list(prediction = data_frame(prediction = predict(model_SMOreg, test), 
                               ground = outcome_variables$`Death rate, crude (per 1,000 people)`[Fold == fold_index]),
       selected_features = colnames(test))}  
end_parallel <- Sys.time()


time_parallel <- end_parallel - start_parallel


#sequential


start_seq <- Sys.time()
out <- foreach(fold_index = 1:max(Fold), .combine = bind_rows, .inorder = FALSE, 
               .packages = c("dplyr", "FSelector", "numDeriv", "quantmod"),
               .export = c("sd_thresholding", "select_features_relieff_derivatives_threshold")) %do% {
                 
                 deve <- independent_variables_no_ol[Fold != fold_index,]
                 test <- independent_variables_no_ol[Fold == fold_index,]
                 
                 independent_variales_thr_death_rate <- sd_thresholding(deve, 
                                                                        outcome_variables$`Death rate, crude (per 1,000 people)`[Fold != fold_index])
                 
                 df_for_analysis <- independent_variales_thr_death_rate
                 
                 df_for_analysis$outcome <- outcome_variables$`Death rate, crude (per 1,000 people)`[Fold != fold_index]
                 
                 #IMPORTANT STEP 2: use relieff method and use derivatives methid from Meng et al 2017 to find threshold
                 
                 df_for_analysis_after_relieff <- select_features_relieff_derivatives_threshold(df_for_analysis, "outcome")
                 model_SMOreg <- SMOreg_classifier(outcome ~ ., data = df_for_analysis_after_relieff)
                 data_frame(prediction = predict(model_SMOreg, test), ground = outcome_variables$`Death rate, crude (per 1,000 people)`[Fold == fold_index]) }  
end_seq <- Sys.time()
  
time_seq <- end_seq - start_seq

out_dataframe <- out %>%
  lapply(`[[`, 1) %>%
  bind_rows()
  
out_features <- out %>%
  lapply(`[[`, 2)




#####################################END GOOD CODE##################################################

#parallel no feature selection
start_parallel_no_features_selction <- Sys.time()

out_no_features_selection <- foreach(fold_index = 1:max(Fold), .combine = bind_rows, .inorder = FALSE, 
               .packages = c("dplyr", "FSelector", "numDeriv", "quantmod"),
               .export = c("sd_thresholding", "select_features_relieff_derivatives_threshold")) %dopar% {
                 
                 deve <- independent_variables_no_ol[Fold != fold_index,]
                 test <- independent_variables_no_ol[Fold == fold_index,]
                 
                 
                 df_for_analysis <- deve
                 
                 df_for_analysis$outcome <- outcome_variables$`Death rate, crude (per 1,000 people)`[Fold != fold_index]
                 
                 #IMPORTANT STEP 2: use relieff method and use derivatives methid from Meng et al 2017 to find threshold
                 
                 
                 model_SMOreg <- SMOreg_classifier(outcome ~ ., data = df_for_analysis)
                 data_frame(prediction = predict(model_SMOreg, test), ground = outcome_variables$`Death rate, crude (per 1,000 people)`[Fold == fold_index]) }  
end_parallel_no_features_selection <- Sys.time()

time_parallel_no_features_selection <- end_parallel_no_features_selection - start_parallel_no_features_selction

#better results without features selection - as strange as it may seems

model_SMOreg <- SMOreg_classifier(outcome ~ ., data = df_for_analysis)


test$outcome <- outcome_variables$`Death rate, crude (per 1,000 people)`[Fold == fold_index]

aa <- evaluate_Weka_classifier(model_SMOreg)



