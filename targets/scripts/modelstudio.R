# library(tidyverse)
# library(tidymodels)
# library(aigoraOpen)
# library(DALEX)
# library(modelStudio)
# library(DALEXtra)
# source("_targets.R")
# # 

#' Get model vars
#'
#' Function for collecting model variable
#'
#' @param model object with fitted model

#' @return list of model variables

get_model_vars <- function(model){
  model$model$pre$actions$recipe$recipe$var_info$variable
}

# model <- targets::tar_read(product_model_glmnet_earth_t_overall_liking_forearm.face_10min.after_liking_product)
# data <- targets::tar_read(product_data_t_overall_liking_forearm.face_10min.after_liking_product)
# target_type <- target_types$liking


#' Make model studio
#'
#' Function for creates an interactive serverless D3.js dashboard for exploration of predictive models.
#'
#' @param model object with fitted model for product data 
#' @param data data.frame with product skin data 
#' @param output_dir output directory
#' @param output_name output name (tar_name() ?)
#' @param new_data data.frame 
#' @param target_type model target ?

#' @return D3.js object

make_modelstudio <- function(model, data, output_dir, output_name, 
                             new_data = NULL, target_type) {
  model_vars <- model$model$pre$actions$recipe$recipe$var_info$variable
  if(is.null(model_vars)) {
    model_vars <- data %>%
      select(-target, -product) %>%
      colnames()
  }
  
  predict_fun <- partial(predict)
  
  expl <- explain(
    model = model,
    data = data %>%
      select(-target, -product) %>%
      select(any_of(model_vars)),
    predict_function = predict_fun,
    y = data$target
  )
  
  if(is.null(new_data)) {
    new_data <- data 
  }
  
  ms <- modelStudio(
    explainer = expl,
    new_observation = new_data %>%
      column_to_rownames(var = "product") %>%
      select(-target) %>%
      select(any_of(model_vars)),
    new_observation_y = round(data$target, 2),
    N = 25,
    B = 5,
    parallel = FALSE
  )
  
  output_dir <- here::here(output_dir, "modelstudio")
  output_file <- here::here(output_dir, sprintf("%s.html", output_name))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  r2d3::save_d3_html(ms, output_file)
  
  output_file
}