cluster_test_set <- function(test_set, features_selected_after_relieff, coordinates_cluster_df, clusters_selected) {
  
  gm_test_selected <- gm_train %>%
    select(., features_selected_after_relieff) %>%
    mutate(subject = row_number()) %>%
    gather(., feature, value, -subject) %>%
    mutate(., index = str_split(feature, pattern = "X") %>% map(~`[`(.,2)) %>% unlist() %>% as.numeric()) %>%
    left_join(., select(coordinates_cluster_df, index, cluster_id), by = "index") %>%
    mutate(cluster_id = as.character(cluster_id)) %>%
    filter(., cluster_id %in% clusters_selected) %>%
    group_by(subject, cluster_id) %>%
    summarise(mean_of_cluster = mean(value)) %>%
    spread(cluster_id, mean_of_cluster) %>%
    ungroup(.) %>%
    select(-subject)
  
}