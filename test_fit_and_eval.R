library(doParallel)
library(RWeka)
setwd("D:/multi_pipeline_tryout")
source("D:/multi_pipeline_tryout/helper_functions.R")
gm_matrix_vec <- c("D:/MultiPAMS/normalized_222_PARKvsHC/gm/","gm_mask.nii.gz","s8")
wm_matrix_vec <- c("D:/MultiPAMS/normalized_222_PARKvsHC/wm/","wm_mask.nii.gz","s8")
codio <- extract_and_normalize_matrix(gm = gm_matrix_vec, wm = wm_matrix_vec)
codio$gm$matrix <- codio$gm$matrix[, sample(5000)]
codio$wm$matrix <- codio$wm$matrix[, sample(5000)]
load("D:/MultiPAMS/normalized_222_PARKvsHC/script/fold.RData")
source("fit_and_eval.s")
fold[fold <= 3] = 1
fold[fold > 3 & fold <= 6] = 2
fold[fold > 6] = 3

outcome <- c(rep("Park",26), rep("HC", 26))

mod_out <- fit_and_eval(codio,outcome,fold, 1)

