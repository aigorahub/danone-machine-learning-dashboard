
label_to_title <- function(x) {
  x %>%
    gsub(pattern = "[^a-zA-Z\\d\\s:]", replacement = " ") %>%
    gsub(pattern = "([^\\s:])([A-Z])", replacement = "\\1 \\2") %>%
    stringr::str_to_title() %>%
    stringr::str_squish()
}

# model <- targets::tar_read(model_fit_t_overall_likeability_product_glmnet_earth)
# resampling <- targets::tar_read(resampling_cv_t_overall_likeability_product_glmnet_earth)

#' Get cv prediction
#'
#' Function for collecting value of predictions based on resampling data
#'
#' @param model object with fitted model
#' @param resampling data.frame with resampling data
#' 
#' 
#' return data.frame include results of predictions based on resampling data


get_cv_predictions <- function(model, resampling) {
  cv_pred <- fit_resamples(
    model,
    resamples = resampling,
    control = control_resamples(save_pred = TRUE)
  )
  
  cv_pred %>%
    collect_predictions(summarize = TRUE) %>%
    select(-target) %>%
    bind_cols(resampling$splits[[1]]$data)
}


#' Get goodness of fit
#'
#' The goodness of fit of a statistical model describes how well it fits a set of observations - summarize the discrepancy between observed values and the values expected under the model
#'
#' @param cv_predictions data.frame with value of predictions based on resampling data
#' @param target_name target variable
#' @param resampling_type data.frame with resampling data
#' @param model_vars model variables
#' 
#' @return ggplot object with graphical representation of the quality of the model fit

# tar_load(cv_predictions_glmnet_earth_analytical_t_ad_leaves_skin_looking_glowing_face_10min.after_liking_product)

get_goodness_of_fit <- function(
  cv_predictions, 
  target_name,
  resampling_type,
  model_vars
) {
  target_label <- target_name %>%
    str_remove("^t_")
    # label_to_title()
  # rescale = ifelse(grepl("ad_", target_name), T, F) 
  
  # if(rescale){
  #   cv_predictions <- cv_predictions %>% 
  #     mutate(.target = rescale(.target, to = c(1,5), from = c(1, 10))) %>% 
  #     mutate(.pred = rescale(.pred, to = c(1,5), from = c(1, 10)))
  # }
  
  gof_data <- cv_predictions %>%
    rename(.target = target) %>%
    group_by(product) %>%
    summarise(across(c(.pred, .target), mean))
  
  # adj_r2 <- gof_data %>%
  #   lm(formula = .target ~ .pred) %>%
  #   summary() %>%
  #   .$adj.r.squared
  
  R2 = yardstick::rsq_vec(gof_data$.target, gof_data$.pred)
  
  n_vars <- length(setdiff(model_vars, "target"))
  
  rmse <- rmse_vec(gof_data$.target, gof_data$.pred)
  
  gof_data %>%
    ggplot(aes(.target, .pred, label = product)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggrepel::geom_label_repel(label.r = 0) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
    ) +
    xlab(target_label) +
    ylab(sprintf("Predicted %s", target_label)) +
    labs(
      title = sprintf("Predicted %s", target_label),
      subtitle = sprintf("CV: %s, R Squared = %.3f, RMSE_product = %.2f, No. variables = %.0f", 
                         tools::toTitleCase(resampling_type), R2, rmse, n_vars)
    )
}

# cv_predictions <- targets::tar_read(cv_predictions_nnet_sensory_t_texture_liking_liking_product)
get_category_probability_plot <- function(
  cv_predictions, 
  target_name,
  resampling_type
) {
  target_label <- target_name %>%
    str_remove("^t_") %>%
    label_to_title()
  
  target_levels <- levels(cv_predictions$target)
  
  pred_data <- cv_predictions %>%
    select(-.pred_class) %>%
    select(product, starts_with(".pred_")) %>%
    pivot_longer(starts_with(".pred_"),
                 names_to = "target", 
                 values_to = "pred", names_prefix = ".pred_") %>%
    group_by(product, target) %>%
    summarise(pred = mean(pred)) %>%
    inner_join(
      cv_predictions %>%
        group_by(product) %>%
        count(target) %>%
        mutate(real = n/sum(n))
    ) %>%
    mutate(target = factor(target, levels = target_levels))
  
  rmse <- rmse_vec(pred_data$real, pred_data$pred)
  rsq <- rsq_vec(pred_data$real, pred_data$pred)
  auc <- cv_predictions %>%
    select(-.pred_class) %>%
    roc_auc(target, starts_with(".pred")) %>%
    pull(.estimate)
  
  pred_data %>%
    rename(Predicted = pred, Real = real) %>%
    pivot_longer(c(Predicted, Real)) %>%
    ggplot(aes(name, value, fill = target)) +
    geom_col(width = 0.5) +
    scale_y_continuous(labels = scales::percent) +
    ggthemes::scale_fill_tableau() +
    xlab(NULL) + ylab(NULL) +
    labs(
      title = sprintf("Predicted %s", target_label),
      subtitle = sprintf("CV: %s; RMSE_product = %.2f, R2_product = %.2f, AUC_ind = %.2f",
                         tools::toTitleCase(resampling_type),
                         rmse, rsq, auc)
    ) +
    guides(fill = guide_legend(title = NULL)) +
    facet_wrap(~product) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 7)
    )
  
}


#' Inspect missing values
#'
#' Function to inspect missing values for each variable. The result is a graphical representation
#'
#' @param data data.frame contain selected features
#' @param main_variable target variables 

#' @return ggplot heat-map


inspect_missing_values <- function(data, main_variable, title = NULL) {
  data %>%
    mutate(across(-{{main_variable}}, is.na)) %>%
    pivot_longer(-{{main_variable}}) %>%
    group_by({{main_variable}}, name) %>%
    summarise(missing_pct = mean(value)) %>%
    ggplot(aes(name, product, fill = missing_pct)) +
    geom_tile() +
    scale_fill_distiller(palette = "Blues", direction = 1, labels = scales::percent) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}


#' save_plot
#'
#' Function to inspect missing values for each variable. The result is a graphical representation
#'
#' @param data data.frame contain selected feauters
#' @param main_variable target variables 

#' @return saved ggplot object in the indicated path

save_plot <- function(plot, output_dir, output_name = NULL, width = 10, height = 10) {
  if(is.null(output_name)) output_name <- tar_name()
  
  output_dir <- here::here(output_dir, "plots")
  output_file <- here::here(output_dir, sprintf("%s.png", output_name))
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  ggsave(output_file, plot, width = width, height = height)
  
  output_file
}

get_predictors <- function(x) {
  UseMethod("get_predictors")
}

get_predictors.workflow <- function(x) {
  x$pre$actions$recipe$recipe$var_info %>%
    filter(role == "predictor") %>%
    pull("variable")
}
get_predictors.product_model <- function(x) {
  get_predictors(x$model)
}

get_target <- function(x) {
  UseMethod("get_target")
}

get_target.workflow <- function(x) {
  x$pre$actions$recipe$recipe$var_info %>%
    filter(role == "outcome") %>%
    pull("variable")
}
get_target.product_model <- function(x) {
  get_target(x$model)
}

