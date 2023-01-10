
# library(tidymodels)
# library(targets)
# library(tidyverse)
# 
# target_mapping <- tar_read(target_mapping, 1)[[1]]
# merged_data <- tar_read(merged_data, 1)[[1]]
# merged_mapping <- tar_read(merged_mapping, 1)[[1]]
# composed_model <- tar_read(composed_model, 1)[[1]]
# data <- merged_data
# model <- composed_model


#' Make resampling cv
#'
#' Function for resampling that uses different portions of data to test and train a model on different iterations.
#'
#' @param data data.frame with all variables 
#' @param type target variables (consumer - only consumer data,product - only product data, product_consumer - both)
#' @param v the number of partitions of the data set (default - 5)

#' @return data.frame with resampling data

make_resampling_cv <- function(data, type, v = 5){
  
  if(type == "consumer") {
    aigoraML::cartesian_resampling(data, cons_id = rsample::vfold_cv(v = !! v))
  } else if(type == "product") {
    aigoraML::cartesian_resampling(data, product = rsample::loo_cv())
  } else if(type == "product_consumer") {
    aigoraML::cartesian_resampling(
      data, 
      product = rsample::loo_cv(), 
      cons_id = rsample::vfold_cv(v = !! v)
    )
  } else {
    stop("Wrong CV type")
  }
  
}

#' Make product model factor
#'
#' Function for ?
#'
#' @param model object with fitted model 
#' @param cv_predictions  data.frame with value of predictions based on resampling data
#' @param target_vals ?

#' @return list

make_product_model_factor <- function(model, cv_predictions, target_vals = NULL) {
  
  consumer_data <- get_consumer_data(cv_predictions)
  
  if(is.null(target_vals)) target_vals <- levels(cv_predictions$target)
  
  structure(
    list(
      model = model,
      consumer_data = consumer_data,
      target_vals = target_vals
    ), class = "product_model_factor"
  )
}

#' Make product model
#'
#' Function for ?
#'
#' @param model object with fitted model 
#' @param cv_predictions  data.frame with value of predictions based on resampling data
#' @param target_range ?

#' @return list

make_product_model <- function(model, cv_predictions, target_range = NULL) {
  
  consumer_data <- get_consumer_data(cv_predictions)
  
  if(is.null(target_range)) target_range <- range(cv_predictions$target)
  
  structure(
    list(
      model = model,
      consumer_data = consumer_data,
      target_range = target_range
    ), class = "product_model"
  )
}

# library(tidyverse)
# library(tidymodels)
# library(aigoraOpen)
# object <- targets::tar_read(product_model_t_overall_liking_product_ordinal_reg_earth)
# product_data <- targets::tar_read(product_data_t_overall_liking_product_ordinal_reg_earth)





predict.product_model_factor <- function(object, product_data, 
                                         consumer_data = NULL, 
                                         category = NULL, ...) {
  model <- object$model
  if(is.null(consumer_data)) consumer_data <- object$consumer_data
  
  predictions <- product_data %>%
    dplyr::mutate(rn = dplyr::row_number()) %>%
    dplyr::inner_join(consumer_data, by = character()) %>%
    dplyr::bind_cols(stats::predict(object = model, ., type = "prob")) %>%
    dplyr::group_by(.data$rn) %>%
    dplyr::summarise(dplyr::across(dplyr::matches("\\.pred_"), mean)) %>%
    dplyr::arrange(.data$rn) %>%
    dplyr::select(-rn)
  
  if(!is.null(category)) {
    predictions <- pull(predictions, sprintf(".pred_%s", category))
  }
  
  predictions
}

# object <- targets::tar_read(product_model_glmnet_earth_t_overall_liking_forearm.face_10min.after_liking_product)
# model_vars <- object$model$pre$actions$recipe$recipe$var_info$variable
# 
# new_data <- targets::tar_read(product_data_t_overall_liking_forearm.face_10min.after_liking_product) %>%
#   select(-target, -product) %>%
#   select(any_of(model_vars))






predict.product_model <- function(object, new_data, consumer_data = NULL, ...) {
  
  model_obj <- object$model
  target_min <- min(object$target_range)
  target_max <- max(object$target_range)
  if(is.null(consumer_data)) consumer_data <- object$consumer_data
  
  tmp_data <- new_data %>%
    mutate(rn = row_number()) %>%
    inner_join(consumer_data, by = character())
  
  tmp_data %>%
    predict(object = model_obj) %>%
    pull(".pred") %>%
    pmin(target_max) %>%
    pmax(target_min) %>%
    split(tmp_data$rn) %>%
    map_dbl(mean, na.rm = TRUE)
}

fit_resamples.product_model <- function(object, ...) {
  fit_resamples(object$model, ...)
}

# resampling <- targets::tar_read(resampling_cv_t_texture_liking_liking_product)
# object <- models_config(config$model_config$models_config_name[[2]])
fit.workflow_config <- function(
  object, 
  resampling, 
  predictors = NULL, 
  target = "target", 
  adt_cols = c("product", "cons_id"),
  keep_only_impact_preds = FALSE
) {
  
  metric <- get_model_metric(object)
  data <- resampling$splits[[1]]$data
  
  rcp <- object$pre_rcp(
    data = data,
    target = target,
    predictors = predictors,
    adt_cols = adt_cols
  )
  
  wflow <- workflow() %>%
    add_recipe(rcp) %>%
    add_model(object$model)
  
  grid_search <- tune_grid(
    wflow,
    resamples = resampling,
    grid = object$grid,
    metrics = eval(parse(text = sprintf("metric_set(%s)", metric)))
  )
  
  model_fit <- grid_search %>%
    select_best(metric = metric) %>%
    finalize_workflow(x = wflow) %>%
    fit(data)
  
  if(keep_only_impact_preds) {
    # browser()
    preds_to_keep <- find_impact_preds(model_fit, data)
    
    new_rcp <- object$pre_rcp(
      data = data,
      target = target,
      predictors = preds_to_keep,
      adt_cols = adt_cols
    )
    
    model_fit <- grid_search %>%
      select_best(metric = metric) %>%
      finalize_workflow(x = update_recipe(wflow, new_rcp)) %>%
      fit(data)
  }
  
  model_fit
}

# data <- tar_read(model_data_t_overall_likeability_product_glmnet_earth)
# model <- tar_read(model_fit_t_overall_likeability_product_glmnet_earth)
# model <- tar_read(product_model_glmnet_earth_t_overall_liking_forearm.face_10min.after_liking_product)
# data <- tar_read(product_data_t_overall_liking_forearm.face_10min.after_liking_product)
# model$fit$fit$spec$mode <- model$model$fit$fit$spec$mode


#' Find impact of predictors
#'
#' Function for calculate the impact input variables on output variable - target
#'
#' @param model object with fitted model for product data 
#' @param data data.frame with product skin data
#' @param ...  target ?

#' @return list of predictors

find_impact_preds <- function(model, data, ...) {
  
  if(model$fit$fit$spec$mode == "classification") {
    find_impact_preds_classification(model, data, ...) 
  } else {
    find_impact_preds_regression(model, data, ...) 
  }
}

#' Find impact of predictors regression
#'
#' Function for calculate the impact input variables on output variable (target) in regression approach 
#'
#' @param model object with fitted model for product data 
#' @param data data.frame with product skin data
#' @param ... target ?
#' 
#' 
#' @return list of predictors from model regression

find_impact_preds_regression <- function(model, data, ...) {
  baseline_preds <- predict(model, data, ...)
  
  partial_predict <- partial(predict, ...)
  
  colnames(data) %>%
    keep(~n_distinct(data[[.x]]) != 1) %>%
    keep(~{
      tmp <- data
      while(TRUE) {
        print(.x)
        tmp[[.x]] <- sample(tmp[[.x]], nrow(tmp))
        
        if(any(tmp[[.x]] != data[[.x]])) break
      }
      any(partial_predict(model, tmp) != baseline_preds)
    })
}

#' Find impact of predictors classification
#'
#' Function for calculate the impact input variables on output variable (target) in classification approach 
#'
#' @param model object with fitted model for product data 
#' @param data data.frame with product skin data
#' @param ... target ?
#' 
#' 
#' @return list of predictors from model classification


find_impact_preds_classification <- function(model, data, ...) {
  baseline_preds <- unlist(predict(model, data, ..., type = "prob"))
  
  partial_predict <- partial(predict, ..., type = "prob")
  
  colnames(data) %>%
    keep(~n_distinct(data[[.x]]) != 1) %>%
    keep(~{
      tmp <- data
      while(TRUE) {
        print(.x)
        tmp[[.x]] <- sample(tmp[[.x]], nrow(tmp))
        
        if(any(tmp[[.x]] != data[[.x]])) break
      }
      any(unlist(partial_predict(model, tmp)) != baseline_preds)
    })
}

get_model_metric <- function(object) {
  switch (object$model$mode,
          "classification" = "roc_auc",
          "regression" = "rmse"
  )
}
