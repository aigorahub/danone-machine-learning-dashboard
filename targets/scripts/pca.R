# data <- tar_read(product_data_t_overall_likeability_product_glmnet_earth)

#' Get Rotation Matrix
#'
#' Set of vectors that give the rotations of the principal component axes. Those vectors are the eigenvectors. A single eigenvalue and its corresponding eigenvector give the extent and direction of a principal component.
#'
#' @param data data.frame with product skin data 
#' 
#' @return data.fram contains rotation matrix from PCA


getRotationMatrix <- function(data) {
  
  pca_data <- data %>%
    column_to_rownames("product") %>%
    select_if(is.numeric) %>%
    select(-target) %>%
    select(where(~sd(.) > 0)) %>%
    as.matrix() %>%
    missMDA::imputePCA()
  
  if(is.list(pca_data)) {
    pca_data <- pca_data %>% 
      .$completeObs %>%
      pmax(0)
  } else {
    pca_data <- pca_data %>% pmax(0)
  }
  
  # use full rank PCA
  num_rel_comps <- min(nrow(pca_data), ncol(pca_data)) - 1
  
  res_pca <- prcomp(pca_data, scale = TRUE, rank = num_rel_comps)
  
  var_coord_tbl <- res_pca %>% 
    factoextra::get_pca_var() %>% 
    pluck("coord") %>% 
    .[,-ncol(.)]
  
  t((res_pca$rotation %*% t(var_coord_tbl))*res_pca$scale) %>% 
    as.data.frame() %>%
    rownames_to_column("Attribute") %>%
    group_by(Attribute) %>% 
    nest() %>% 
    mutate(adj_vect = map2(Attribute, data, function(x,y){y / y[[x]]})) %>% 
    select(Attribute, adj_vect) %>% 
    unnest(cols = c(adj_vect)) %>% 
    ungroup()
}