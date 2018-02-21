library(tidyverse)
library(Biocomb)
library(RWeka)
library(foreach)

load("tests_file_for_fitting_all_combinations.RData")

outcome <- c(rep("PAMS",29), rep("HC", 26))

SMO_classifier <- make_Weka_classifier("weka/classifiers/functions/SMO")

modalities <- names(all_mods_train)
n_modalities <- length(modalities)

#calculate all possible combinations and their total number excluding the last combination
#that is the one including all the modalities (i.e. you already have it and it is indeed all_mods_train)
combination_list = list()
total_n_combinations <- 0
for (mod in 1:(n_modalities-1)){
  
  combination_list[[mod]] <- combn(modalities,mod)
  total_n_combinations <- total_n_combinations + factorial(n_modalities)/(factorial(mod)*factorial(n_modalities - mod)) 
}

#iterate over combination and create set of string to be used with the function select(df, match()) in order to
#make easier the fitting process
outer_list_of_modalities <- list()
for(sublist in 1:length(combination_list)) {
  inner_list_of_modalities = list()
  for(combination in 1:ncol(combination_list[[sublist]])) {
    this_combination <- combination_list[[sublist]][,combination]
    inner_list_of_modalities[[combination]] <- this_combination
  }
  outer_list_of_modalities[[sublist]] <- inner_list_of_modalities
}

all_mods_test_df <- Reduce(bind_cols, all_mods_test)
all_mods_train_df <- Reduce(bind_cols, all_mods_train)

#function that calculate the combination and put them in a workable shape
list_of_modalities_combinations <- function(list_of_modalities_for_training) {
  
  
  modalities <- names(list_of_modalities_for_training)
  n_modalities <- length(modalities)
  
  combination_list = list()
  total_n_combinations <- 0
  for (mod in 1:(n_modalities-1)){
    
    combination_list[[mod]] <- combn(modalities,mod)
    total_n_combinations <- total_n_combinations + factorial(n_modalities)/(factorial(mod)*factorial(n_modalities - mod)) 
  }
  
  #iterate over combination and create set of string to be used with the function select(df, match()) in order to
  #make easier the fitting process
  outer_list_of_modalities <- list()
  for(sublist in 1:length(combination_list)) {
    inner_list_of_modalities = list()
    for(combination in 1:ncol(combination_list[[sublist]])) {
      this_combination <- combination_list[[sublist]][,combination]
      inner_list_of_modalities[[combination]] <- this_combination
    }
    outer_list_of_modalities[[sublist]] <- inner_list_of_modalities
  }
  out <- unlist(outer_list_of_modalities, recursive = FALSE)
  return(out)
}

outer_list_of_modalities <- list_of_modalities_combinations(all_mods_train)

final_list_of_modalities_string <- outer_list_of_modalities %>% 
    sapply(., function(x) if(length(x) == 1) {return(x)} else {paste(x, collapse = "|")})

#fake outcome just for testing
outcome_train <- outcome[1:50]

#iterate over modalities and fit model
results_of_the_combinations <- list()
for (string_combinations in 1:length(final_list_of_modalities_string)) {
  print(final_list_of_modalities_string[string_combinations])
  this_combination <- select(all_mods_train_df, matches(final_list_of_modalities_string[string_combinations])) %>%
    mutate(outcome = outcome_train)
  out<- foreach (fold_index = 1:nrow(this_combination), 
           .inorder = FALSE, 
           .packages = c("tidyverse","dplyr", "Biocomb", "RWeka"),
           .export = c( "SMO_classifier", "list_of_modalities", "outcome", "fold_to_evaluate"), .combine = "c") %do% {
  
    print(fold_index)
    this_combination_loo <- this_combination[-fold_index,] %>%
      select(., c(select.cfs(.)$Index, ncol(.)))
    this_model <- SMO_classifier(as.factor(outcome) ~ ., this_combination_loo)
    loo_prediction <- predict(this_model, this_combination[fold_index,])}
  df_for_accuracy <- data_frame(classification = out, ground = outcome_train)
  table_for_accuracy <- table(df_for_accuracy)
  balanced_accuracy <- ((table_for_accuracy[1,1]/sum(table_for_accuracy[1,])) + table_for_accuracy[2,1]/sum(table_for_accuracy[2,]))/2
  results_of_the_combinations[[string_combinations]] = list(df_selected = this_combination_loo, model = this_model, acc = balanced_accuracy)
  names(results_of_the_combinations)[string_combinations] <- final_list_of_modalities_string[string_combinations]
}


#iterate over modalities in the test dataframe, select columns based on training and predict
list_of_test <- list()
for(string_combinations in 1:length(final_list_of_modalities_string)) {
  this_df <- select(all_mods_test_df, one_of(colnames(list_of_process[[string_combinations]]$df_selected)))
  this_pred <- predict(list_of_process[[string_combinations]]$model, this_df)
  list_of_test[[string_combinations]] <- list(df_selected = this_df, prediction = this_pred)
  names(list_of_test)[string_combinations] <- final_list_of_modalities_string[string_combinations]
}


