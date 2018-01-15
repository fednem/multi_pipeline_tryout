library(tidyverse)
setwd("E:/multi_pipeline_tryout/")
nuisance_and_outcome_variables <- read_delim("E:/multi_pipeline_tryout-improvement_on_relieff/gm/nuisance_and_outcome_variables.txt",delim = "\t")
#extract outcome variable
outcome <- nuisance_and_outcome_variables %>%
  mutate(outcome = if_else(group_1 == 1, "HC", "NF1")) %>%
  select(outcome)
outcome <- data_frame(outcome = c(outcome$outcome, c(rep("HC",21),rep("NF1",17))))
fold <- caret::createFolds(outcome$outcome, k = 10, list = FALSE)
save(file = "fold.RData", fold)
