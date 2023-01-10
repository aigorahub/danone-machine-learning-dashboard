# library(reticulate)
# conda_create("r-jnj", python_version=3.9)
# conda_install("r-jnj", "numpy")
# conda_install("r-jnj", "miniball", pip=TRUE)
# 

model = tar_read("product_model_glmnet_earth_sensory_t_overall_liking_forearm.face_10min.after_liking_product")
data <- tar_read("product_data_sensory_t_overall_liking_forearm.face_10min.after_liking_product")
model_variables <- tar_read("model_variables_glmnet_earth_sensory_t_overall_liking_forearm.face_10min.after_liking_product")
use_pca = T


#' Find optimal profile
#'
#' Function for finding optimal profile
#'
#' @param model object with fitted model for ... data 
#' @param data data.frame with ... data 
#' @param model_variables 
#' @param use_pca () T/F
#' @param global_min 
#' @param global_max 

#' @return object


find_optimal_profile <- function(model, data, model_variables, use_pca = FALSE,
                                 global_min = 0, global_max = 10) {
  if(use_pca) {
    find_optimal_profile_fun <- find_optimal_profile_pca
  } else {
    find_optimal_profile_fun <- find_optimal_profile_imp_att
  }
  
  find_optimal_profile_fun(
    model, 
    data,
    model_variables,
    global_min = global_min, 
    global_max = global_max
  )
}

#' Find optimal profile PCA
#'
#' Function
#'
#' @param model object with fitted model for ... data 
#' @param data data.frame with ... data 
#' @param model_variables 
#' @param use_pca () T/F
#' @param global_min 
#' @param global_max 
#' 
#' #' @return object

find_optimal_profile_pca <- function(model, data, model_variables,
                                     global_min = 0, global_max = 10, margin = 0.1) {
  # browser()
  res_pca <- data %>%
    select(-product, -target) %>%
    select(where(is.numeric)) %>%
    prcomp(center = TRUE, scale. = TRUE)
  
  # adt_vars <- data %>%
  #   select(-product, -target) %>%
  #   select(-where(is.numeric)) %>%
  #   distinct() %>%
  #   expand(!!! rlang::syms(names(.)))
  
  # input_pca <- predict(res_pca, data[1,]) %>% as.vector()
  # input_pca <- res_pca$x %>% as.data.frame() %>% map_dbl(median)
  
  use_condaenv("r-jnj")
  miniball <- import("miniball")
  ball <- miniball$get_bounding_ball(res_pca$x)
  
  max_attributes <- data %>%
    # select(-product, -target, -all_of(names(adt_vars))) %>%
    select(-product, -target) %>%
    summarise(across(everything(), max, na.rm = TRUE)) %>% 
    # mutate(across(everything(), ~ min(global_max, . + margin * (. - global_min)))) %>%
    unlist() 
  
  min_attributes <- data %>%
    # select(-product, -target, -all_of(names(adt_vars))) %>%
    select(-product, -target) %>%
    summarise(across(everything(), min, na.rm = TRUE)) %>% 
    # mutate(across(everything(), ~ min(global_max, . + margin * (. - global_min)))) %>%
    unlist() 
  
  neg_predict_liking_pca <- partial(neg_predict_liking_pca_full, 
                                    model = model, 
                                    res_pca = res_pca,
                                    ball = ball,
                                    max_attributes = max_attributes,
                                    min_attributes = min_attributes,
                                    margin = margin)
  
  # neg_predict_liking_pca(input_pca)
  
  # prepare for optimization
  
  min_vals <- res_pca$x %>%
    as.data.frame() %>%
    map_dbl(min, na.rm = TRUE)
  
  max_vals <- res_pca$x %>%
    as.data.frame() %>%
    map_dbl(max, na.rm = TRUE)
  
  med_vals <- res_pca$x %>%
    as.data.frame() %>%
    map_dbl(median, na.rm = TRUE)
  
  res <- data.frame()
  
  # for(i in 1:nrow(adt_vars)) {
  #   adt_data <- adt_vars[i,]
  #   label <- adt_data %>% unlist() %>% paste(collapse = "_") %>% sprintf(fmt = "Opt-%s")
    label <- "Opt"
    
    # create function for product vector as function of components
    best_prod_profile <- data %>%
      # select(-all_of(names(adt_vars))) %>%
      # bind_cols(adt_data) %>%
      mutate(.pred = predict(model, .)) %>%
      slice_max(.pred) %>%
      select(starts_with("p_"))
    
    input_vect <- best_prod_profile %>%
      predict(object = res_pca) %>%
      as.numeric()
    
    # debug(neg_predict_liking_pca_full)
    # neg_predict_liking_pca(input_vect)
    
    # optim form stats
    opt_res <- optim(input_vect, fn = neg_predict_liking_pca,
                     lower = min_vals, upper = max_vals, 
                     method = "L-BFGS-B", 
                     control = list(trace = 2, 
                                    ndeps = rep(1e-2, length(input_vect))))
    
    res <- opt_res$par %>%
      # neg_predict_liking_pca()
      matrix(1, ) %>%
      inverse_pca(res_pca) %>%
      as.data.frame() %>%
      mutate(product = label) %>%
      # bind_cols(adt_data) %>%
      relocate(product, .before = everything()) %>%
      bind_rows(res)
    
  # }
  
  res
}

# find_optimal_profile_imp_att <- function(model, data, model_variables, 
  #                                        global_min = 0, global_max = 10) {
  # 
  # best_profile <- data %>%
  #   mutate(.pred = predict(model, .)) %>%
  #   slice_max(.pred) %>%
  #   select(starts_with("p_")) %>%
  #   pivot_longer(everything(), values_to = "best")
  # 
  # data_mat <- data %>%
  #   select(starts_with("p_")) %>%
  #   map(~data.frame(
  #     min = max(global_min, grDevices::extendrange(.x)[[1]]),
  #     max = min(global_max, grDevices::extendrange(.x)[[2]]),
  #     med = median(.x)
  #   )) %>%
  #   enframe() %>%
  #   unnest(cols = value) %>%
  #   inner_join(best_profile)
  # 
  # data_mat_model_vars <- data_mat %>% filter(name %in% model_variables)
  # data_mat_rest <- data_mat %>% filter(! name %in% model_variables)
  # 
  # base_data <- data_mat_rest %>% 
  #   select(name, value = med) %>%
  #   pivot_wider()
  # 
  # neg_predict_liking <- partial(
  #   neg_predict_liking_full, 
  #   model = !! model, 
  #   model_variables = !! data_mat_model_vars$name, 
  #   base_data = !! base_data
  # )
  # 
  # # input_vect <- data_mat_model_vars$best
  # # neg_predict_liking(input_vect)
  # 
  # # optimize
  # 
  # opt_res <- optim(
  #   par = data_mat_model_vars$best, 
  #   fn = neg_predict_liking,
  #   lower = data_mat_model_vars$min, 
  #   upper = data_mat_model_vars$max,
  #   method = "L-BFGS-B", 
  #   control = list(trace = 6)
  # )
  # 
  # optimal_profile_median <- combine_data(
  #   base_data,
  #   opt_res$par,
  #   data_mat_model_vars$name
  # ) %>% 
  #   mutate(product = "Optimum")
  # 
  # impute_vars <- data_mat_rest$name
  # 
  # pre_impute_data <- optimal_profile_median %>%
  #   mutate(across(all_of(impute_vars), ~NA)) %>%
  #   bind_rows(data) %>% 
  #   select(-target, -product)
  # 
  # imp_res <- pre_impute_data %>%
  #   missMDA::imputePCA() %>%
  #   pluck("completeObs") %>%
  #   head(1) %>%
  #   as.data.frame() %>%
  #   select(all_of(impute_vars))
  # 
  # optimal_profile_imputed <- optimal_profile_median %>%
  #   select(-all_of(impute_vars)) %>% 
  #   bind_cols(imp_res) %>%
  #   relocate(product)
  # 
  # optimal_profile_imputed
# }



combine_data <- function(base_data, adt_vals, adt_names) {
  bind_cols(
    base_data,
    set_names(adt_vals, adt_names) %>% bind_rows()
  )
}


neg_predict_liking_full <- function(input_vect, model, model_variables, base_data){
  
  model_data <- combine_data(base_data, input_vect, model_variables) 
  
  suppressWarnings(output <- -predict(model, model_data))
  
  return(output)
  
}


#' neg predict liking pca full
#'
#' Function for 
#'
#' @param input_pca 
#' @param res_pca 
#' @param ball
#' @param margin
#' @param max_attributes 
#' @param min_attributes 
#' @param adt_data

#' @return output


neg_predict_liking_pca_full <- function(input_pca, model, 
                                        res_pca, ball = NULL, margin = 0.1,
                                        max_attributes, min_attributes,
                                        adt_data = data.frame()){
  
  data_vec <- inverse_pca(matrix(input_pca, ncol = ncol(res_pca$x)), res_pca)
  
  suppressWarnings(output <- -predict(model, 
                                      bind_cols(
                                        as.data.frame(data_vec),
                                        # adt_data
                                      )))
  # output <- output * min(1, (global_max - data_vec)/(global_max - max_attributes))
  
  output <- output * (1 - max(0, (data_vec - max_attributes)/(max_attributes-min_attributes)) )
  
  if(!is.null(ball)) {
    output <- output/max(1, dist_to_ball(input_pca, ball) - margin)
  }
  
  return(output)
}

get_comp_number <- function(res_pca, var_exp) {
  res_pca %>%
    summary %>%
    pluck("importance") %>%
    .[3,] %>%
    `>`(var_exp) %>%
    which() %>%
    min()
}

subset_pca <- function(res_pca, var_exp) {
  n_comp <- get_comp_number(res_pca, var_exp)
  
  res_pca$rotation <- res_pca$rotation[,1:n_comp]
  res_pca$x <- res_pca$x[,1:n_comp]
  
  res_pca
}


#' Inverse pca
#'
#' Function for  reverse PCA and reconstruct original variables from several principal components
#'
#' @param data data.frame with input data for pca
#' @param pca PCA object
#' @param n_comp number of pca components


inverse_pca <- function(data, pca, n_comp = NULL) {
  if(is.null(n_comp))
    n_comp <- ncol(pca$rotation)
  
  inv <- t(t(data[,1:n_comp] %*% t(pca$rotation[,1:n_comp])) * pca$scale + pca$center)
  
  inv %>%
    pmax(0) %>%
    set_names(names(inv))
}

is_in_convex_hull <- function(data, res_pca, ch) {
  inhulln(ch = ch, predict(res_pca, data))
}

dist_to_ball <- function(x, ball) {
  sum((as.vector(x) - as.vector(ball[[1]]))^2) / ball[[2]]
}
