library(oro.nifti)
library(neurobase)
library(tidyverse)
library(spatstat)
grid <- expand.grid(0:4,0:4,0:4)
bb <- box3(range(grid[,1]), range(grid[,2]), range(grid[,3]))
grid$id <- 1:nrow(grid)
set.seed(42)
a <- grid[sample(nrow(grid), 20),]
x <- pp3(a[,1], a[,2], a[,3], bb)
x_labelled <- connected(x, R = 1.8)
df <- data.frame(cluster_id = marks(x_labelled), point_id = a$id)
df[order(df$cluster_id, df$point_id),]


setwd("/MultiPAMS/rs-fmri/ROIs/")
#read_roi
roi <- readNIfTI("bilateral_SN_Wu_2012.nii")
roi_img <- cal_img(roi)
array_img <- roi@.Data
non_zero_voxels <- which(array_img != 0, TRUE) %>%
  as_data_frame(.) %>%
  select(x = dim1, y = dim2, z = dim3) %>%
  bind_cols(., which(array_img != 0) %>% as_data_frame) %>%
  select(x, y, z, index = value)

bb <- box3(range(non_zero_voxels[,1]), range(non_zero_voxels[,2]), range(non_zero_voxels[,3]))
x <- pp3(non_zero_voxels$x, non_zero_voxels$y, non_zero_voxels$z, bb)
x_labelled <- connected(x, R = 1.8)
df <- cbind(cluster_id = marks(x_labelled), non_zero_voxels)

new_img <- roi
new_img@.Data <- array(0,dim(array_img))

for (cl in 1:nrow(df)) {
  new_img@.Data[df %>% filter(., cluster_id == cl) %>% select(x, y, z) %>% as.matrix] <- cl
}

writeNIfTI(new_img, "prova_answer2", verbose=TRUE)


start <- Sys.time()
roi <- readNIfTI("image_to_cluster.nii")
roi_img <- cal_img(roi)
array_img <- roi@.Data
non_zero_voxels <- which(array_img != 0, TRUE) %>%
  as_data_frame(.) %>%
  select(x = dim1, y = dim2, z = dim3) %>%
  bind_cols(., which(array_img != 0) %>% as_data_frame) %>%
  select(x, y, z, index = value)

start <- Sys.time()
bb <- box3(range(non_zero_voxels[,1]), range(non_zero_voxels[,2]), range(non_zero_voxels[,3]))
x <- pp3(non_zero_voxels$x, non_zero_voxels$y, non_zero_voxels$z, bb)
x_labelled <- connected(x, R = 1.8)
df <- cbind(cluster_id = marks(x_labelled), non_zero_voxels)
end <- Sys.time()

end - start

new_img <- roi
new_img@.Data <- array(0,dim(array_img))
#dplyr solution
start_dplyr <- Sys.time()
for (cl in 1:nrow(df)) {
  new_img@.Data[df %>% filter(., cluster_id == cl) %>% select(x, y, z) %>% as.matrix] <- cl
}
end_dplyr <- Sys.time()

end_dplyr - start_dplyr

start_vec <- Sys.time()

for (cl in 1:nrow(df)) {
  new_img@.Data[as.matrix(df[df$cluster_id == cl, 2:4])] <- cl
}
end_vec <- Sys.time()

end_vec - start_vec

start_index <- Sys.time()

for (cl in 1:nrow(df)) {
  new_img@.Data[as.vector(df[df$cluster_id == cl, 5])] <- cl
}
end_index <- Sys.time()

end_index - start_index


writeNIfTI(new_img, "prova_answer2", verbose=TRUE)
