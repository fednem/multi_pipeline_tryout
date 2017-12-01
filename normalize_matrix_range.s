normalize_matrix_range <- function(matrix) {
  range_mat <- range(matrix)
  new_mat <- (matrix - range_mat[1])/(range_mat[2] - range_mat[1])
  return(new_mat)
}

