library(tidyverse)
library(Biocomb)
library(RWeka)
library(foreach)


#function that calculate the combination and put them in a workable shape
#notice that the input of this function is the list of modalities BEFORE Reducing it to a data frame with cbind !
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
  out_1 <- unlist(outer_list_of_modalities, recursive = FALSE)
  
  out_2 <- out_1 %>% 
    sapply(., function(x) if(length(x) == 1) {return(x)} else {paste(x, collapse = "|")})
  
  return(out_2)
}

#create a function that given a df with ALL the clusters from ALL modalities return a dataframe 
#with the combination of modalities (and thus of cluster) that give the best accuracy within a LOO framework
#note that the LOO framework is implemented using only the test fold in the outer N-fold scheme
#also note that the solution that give the best accuracy in the LOO is then refitted to the whole sample
#in order to have a unique set to use for testing

select_best_combination_of_modalities <- function(vector_of_combinations, df_training, outcome, n_fold = 10, seed = NULL) {

  SMO_classifier <- make_Weka_classifier("weka/classifiers/functions/SMO")
  if (length(seed) != 0) {seed = seed} else {seed = sample(5000,1)
  print(paste("your seed is", seed, "you may want to write it down", sep = " "))}
  
  set.seed(seed)
  
  fold <- caret::createFolds(outcome, k = n_fold, list = FALSE)
  
  print(paste("you are running an", n_fold, "inner CV scheme", sep = " "))
  
  #initialize an empty vector to store the results of the computations
  #note that the vector is equal to length of the combinations vector + one in order to have the place
  #for the full combination which is not take into account in the function that calculate the combinations
  results_of_the_combinations <- vector(mode = "numeric", length = length(vector_of_combinations) + 1)
  for (string_combinations in 1:length(vector_of_combinations)) {
    print(vector_of_combinations[string_combinations])
    this_combination <- select(df_training, matches(vector_of_combinations[string_combinations])) %>%
      mutate(outcome = outcome)
    out<- foreach (fold_index = 1:max(fold), 
                   .inorder = FALSE, 
                   .packages = c("tidyverse","dplyr", "Biocomb", "RWeka"),
                   .export = c( "SMO_classifier", "list_of_modalities", "outcome", "fold_to_evaluate")) %do% {
                     
                     print(fold_index)
                     this_combination_loo <- this_combination[fold != fold_index,] %>%
                       select(., c(select.cfs(.)$Index, ncol(.)))
                     this_model <- SMO_classifier(as.factor(outcome) ~ ., this_combination_loo)
                     this_df_loo <- data_frame(predictions = predict(this_model, df_training[fold == fold_index,]),
						ground = outcome[fold == fold_index])}
    full_loo_df <- bind_rows(out)
    table_for_accuracy <- table(full_loo_df)
    balanced_accuracy <- ((table_for_accuracy[1,1]/sum(table_for_accuracy[,1])) + table_for_accuracy[2,2]/sum(table_for_accuracy[,2]))/2
    results_of_the_combinations[string_combinations] = balanced_accuracy
    names(results_of_the_combinations)[string_combinations] <- vector_of_combinations[string_combinations]
  }
  
  df_training <- df_training %>%
    mutate(outcome = outcome)
  #perform loo also for full combination of modalities
  out<- foreach (fold_index = 1:max(fold), 
                 .inorder = FALSE, 
                 .packages = c("tidyverse","dplyr", "Biocomb", "RWeka"),
                 .export = c( "SMO_classifier", "list_of_modalities", "outcome", "fold_to_evaluate")) %do% {
                   
                   print(fold_index)
                   this_combination_loo <- df_training[fold != fold_index, ] %>%
                     select(., c(select.cfs(.)$Index, ncol(.)))
                   this_model <- SMO_classifier(as.factor(outcome) ~ ., this_combination_loo)
                   this_df_loo <- data_frame(predictions = predict(this_model, df_training[fold == fold_index,]),
					ground = outcome[fold == fold_index])}
  full_loo_df <- bind_rows(out)
  table_for_accuracy <- table(full_loo_df)
  balanced_accuracy <- ((table_for_accuracy[1,1]/sum(table_for_accuracy[,1])) + table_for_accuracy[2,2]/sum(table_for_accuracy[,2]))/2
  results_of_the_combinations[length(results_of_the_combinations)] <- balanced_accuracy
  names(results_of_the_combinations)[length(results_of_the_combinations)] <- "full"
  
  best_combo <- names(which.max(results_of_the_combinations))
  
  if (best_combo == "full") {final_training_df <- df_training} else {
    final_training_df <- select(df_training, matches(best_combo)) %>%
      mutate(outcome = outcome)
  }
  
  final_training_df_selected <- final_training_df %>%
    select(., c(select.cfs(.)$Index, ncol(.)))
  return(list(final_df = final_training_df_selected, best_combo = best_combo))
  
}
