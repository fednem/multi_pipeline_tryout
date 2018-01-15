source("helper_functions.R")
gm_info <- reshape_images_for_pipeline("D:/MultiPAMS/normalized_222/gm/", "gm_mask.nii.gz", "s8")
gm_matrix_park <- gm_info$n_by_v_matrix
rm(gm_info)
outcome_park <- c(outcome = c(rep("Park",26), rep("PAMS",29), rep("HC", 26)))

gm_selected <- gm_matrix[,sample(5000)]

calculate_t_stat_by_cols <- function(matrix, outcome) {
  
  out <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome)$statistic))
  out_p <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome)$p.value))
  
  out <- sort(out, decreasing = TRUE)
  out_p <- sort(out_p)
  
  out_norm <- normalize_matrix_range(out)
  

  
}

matrix <- gm_matrix_park

out_park <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome_park)$statistic))
out_p_park <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome_park)$p.value))

out_park <- sort(out_park, decreasing = TRUE)
out_p_park <- sort(out_p_park)

out_norm_park <- normalize_matrix_range(out_park)

plot(out_p_park)
points(out_norm_park, col = "red")
abline(h = .05, col = "green")

ans_p_park <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_park)),out_p_park)
ans_F_park <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_park)),out_park)


gm_info <- reshape_images_for_pipeline("D:/multi_pipeline_tryout_imaging/gm/", "gm_mask.nii.gz", "s8")
gm_matrix <- gm_info$n_by_v_matrix
rm(gm_info)

outcome <- c(rep("HC",21), rep("NF1", 21))

matrix <- gm_matrix

out_nf <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome)$statistic))
out_p_nf <- sapply(matrix, function(x) unname(oneway.test(x ~ outcome)$p.value))

out_nf <- sort(out_nf, decreasing = TRUE)
out_p_nf <- sort(out_p_nf)

out_norm_nf <- normalize_matrix_range(out_nf)

plot(out_p_nf)
points(out_norm_nf, col = "red")
abline(h = .05, col = "green")


ans_p_nf <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_nf)),out_p_nf)
ans_F_nf <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_nf)),out_nf)

plot(normalize_matrix_range(ans_p_park$second_derivative_values))
points(normalize_matrix_range(ans_p_nf$second_derivative_values), col = "red")

plot(normalize_matrix_range(ans_F_park$second_derivative_values))
points(normalize_matrix_range(ans_F_nf$second_derivative_values), col = "red")

plot(out_park)
points(out_nf, col = "red")

ans_F_nf$threshold
ans_F_park$threshold

pdf("example_t_test_selection.pdf", w = 6, h = 4)
plot(sort(out_p_park, decreasing = TRUE), ylab = "p value")
points(sort(out_p_nf, decreasing = TRUE), col = "red")
abline(h = .05, col = "green", lwd = 2)
abline(h = .2, col = "magenta", lwd = 2)
dev.off()

ans_p_nf_ascending <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_nf)),sort(out_p_nf, decreasing = TRUE))
ans_p_park_ascending <- calculate_features_threshold_based_on_second_derivative(seq(1,length(out_p_park)),sort(out_p_park, decreasing = TRUE))

plot(normalize_matrix_range(ans_p_nf_ascending$second_derivative_values))
points(normalize_matrix_range(ans_p_park_ascending$second_derivative_values), col = "red")
hist(out_p_nf)
hist(out_p_park)
