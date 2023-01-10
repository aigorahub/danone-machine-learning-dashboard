#' Make input shiny
#'
#' Function .
#'
#' @param target target variable
#' @param all_product_data data.frame with product data
#' @param modelstudio_file model studio object
#' @param product_data data.frame with skin data
#' @param model object with fitted model
#' @param important_variables 
#' @param delta_tbl
#' @param optima
#' @param calibration_plot
#' @param data_type
#' @param skin_tone

#' @return shiny object?

make_shiny_input = function(
  target, all_product_data,
  modelstudio_file, product_data, model, 
  important_variables, delta_tbl, optima,
  calibration_plot, data_type, skin_tone
){
  target <- target %>%
    stringr::str_remove("^t_") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()
  tibble(
    target = target,
    all_product_data = list(all_product_data %>% rename(Target = target)),
    modelstudio_file = basename(modelstudio_file),
    product_data = list(product_data %>% rename(Target = target)),
    model = list(model),
    important_variables = list(important_variables),
    delta_tbl = list(delta_tbl),
    # optima = list(optima),
    calibration_plot = list(calibration_plot),
    products_type = str_to_title(data_type),
    skin_tone = skin_tone
  )
}