# target_types <- list(
#   liking = list(
#     mapping = c(`1` = "Dislike", `2` = "Dislike", `3` = "Dislike", `4` = "Dislike",
#                 `5` = "Neither",
#                 `6` = "Like", `7` = "Like", `8` = "Like", `9` = "Like"),
#     positive_val = "Like"
#   )
# )
# 
config <- list(

  # input data files
  data_file = "input/processed_data/20221227_danone_core_milk_consumer_data.csv",
#   sensory_data_file = "input/processed_data/final_sensory_data.rds",
#   rheology_data_file = "input/processed_data/final_rheology_data.rds",
#   whitening_data_file = "input/processed_data/final_whitening_data.rds",
#   # attributes_mapping_file = "input/match_files/Shorten_names.xlsx",
#   
#   # output directory
  model_output_dir = "output/models",
#   
#   # If TRUE, after model is fitted it will be refitted 
#   # using only predictors that had non-zero coefficients,
#   # This makes modelStudio object smaller and quicker to get 
  keep_only_impact_preds = TRUE,
#   
# 
#   
#   # Define data type and targets
# 
#   skin_tone_config = tibble::tibble(skin = list("all", "1", "2", "3")),
# 
  data_config = dplyr::inner_join(
    tidyr::expand_grid(
      include_cata = c(TRUE, FALSE),
      target_name = c("t_q_scl_empty_bottle_overall_liking"),
      country = c("1", "2", "all"),
      target_type = c("liking")
    ),
    tibble::tibble(
      resampling = c("product")
      # resampling = c("product", "product_consumer")
      # resampling = "consumer"
    ),
    by = character()),

  model_config = tibble::tibble(
    # Name of model(s) to be used, the models are configured in
    # targets/scripts/_models_config.R
    # models_config_name = "ordinal_reg_earth"
    models_config_name = c(
      "glmnet",
      "glmnet_earth"
      )
  )

)
# # config$data_config
# 

