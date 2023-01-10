library(targets)
library(tarchetypes)
library(tidyverse)

# library(future)
# library(future.callr)
# plan(callr)
`%>%` <- magrittr::`%>%`

source("targets/_targets_config.R")
sapply(list.files("targets/scripts/", full.names = TRUE), source)
source("targets/_config.R")


# End this file with a list of target objects.
prep <- list(
  
  #tar_target - single step of computation in pipeline
  
  # track changes in file
  tar_target(
    data_file,
    config$data_file,
    format = "file"
  ),
  tar_target(
    data,
    # preparing data for modeling
    prep_data(data_file)
  ),
  
  tar_target(
    # check missing values in main variables
    inspect_missing_values_plot,
    inspect_missing_values(data, product)
  ),
  # save plot with missing values in main variables
  tar_target(
    missing_values,
    save_plot(
      plot = inspect_missing_values_plot, # object with plot
      output_dir = config$model_output_dir, #output directory
      width = 24 # width of the plot
    ),
    format = "file"
  )
)

# tar_map - define multiple computations 
mapped <- tar_map(
  values = config$data_config,
  
  # get data for modeling
  tar_target(model_data, 
             prep_model_data(
               data = data, 
               include_cata = include_cata,
               country = country,
               target_column = target_name 
             )),
  # get product data
  # tar_target(product_data, 
  #            get_product_data(
  #              model_data
  #            )
  # ),
  # tar_target(
  #   delta_tbl,
  #   getRotationMatrix(product_data)
  # ),
  
  #Cross-validation 
  tar_target(resampling_cv,
             make_resampling_cv(
               model_data, 
               resampling
             )),

# tar_map - define multiple computations     
  tar_map(
    values = config$model_config,
    
    #fit the model with resampling data
    tar_target(
       
      model_fit,
      fit(
        models_config(models_config_name), # selected model definition defined in _config.R script
        resampling_cv,
        keep_only_impact_preds = config$keep_only_impact_preds
      ),
      
      #selected packages use by model_fit target use to by define in _targets_config.R 
      packages = modeling_packages
    ),
    
    # collect value of predictions
    tar_target(
      cv_predictions,
      get_cv_predictions(model_fit, resampling_cv),
      packages = modeling_packages
    )
    
    # make product model
  #   tar_target(
  #     product_model,
  #     make_product_model(model_fit, cv_predictions)
  #   ),
  #   # get model variables
  #   tar_target(model_vars,
  #              get_model_vars(product_model)),
  #   
  #   # check goodness of fitting model
  #   tar_target(
  #     gof_plot,
  #     get_goodness_of_fit(
  #       cv_predictions,
  #       target_name,
  #       resampling_type = resampling,
  #       model_vars
  #     ),
  #     packages = modeling_packages
  #   ),
  #   
  #   # save the plot with goodness of fitting model
  #   tar_target(
  #     calibration,
  #     save_plot(
  #       plot = gof_plot, # object with plot
  #       output_dir = config$model_output_dir # output directory
  #     ),
  #     format = "file"
  #   ),
  #   # here by skin tone
  #   
  # # define multiple computations     
  #   tar_map(
  #     values = config$skin_tone_config,
  #     
  #     tar_target(product_skin_data,
  #                {
  #                  if(skin != "all"){
  #                    data <- model_data %>%
  #                      filter(c_skin_tone %in% as.numeric(skin))
  #                  }
  #                  data <- model_data
  #                  data %>% 
  #                    get_product_data(
  #                    )
  #                }
  #     ),
  #     #calculate PCA rotation matrix
  #     tar_target(
  #       delta_tbl,
  #       getRotationMatrix(product_skin_data)
  #     ),
  #     #find impact of predictors
  #     tar_target(
  #       model_variables,
  #       {
  #         x <- product_model
  #         x$fit$fit$spec$mode <- x$model$fit$fit$spec$mode
  #         find_impact_preds(x, product_skin_data, category = "Like")
  #       },
  #       packages = modeling_packages
  #     ),
  #     
  #     # create model studio to explore the prediction model
  #     tar_target(
  #       modelstudio,
  #       make_modelstudio(
  #         model = product_model,
  #         data = product_skin_data,
  #         output_dir = config$model_output_dir,
  #         output_name = tar_name(),
  #         target_type = target_types[[target_type]]
  #       ),
  #       packages = modelstudio_packages,
  #       format = "file"
  #     ),
  #     # tar_target(
  #     #   modelstudio_dashboard,
  #     #   file.copy(modelstudio, file.path(config$dashboard_output_dir, "modelstudio"), recursive = TRUE)
  #     # ),
  #     
  #     
  #     # find optimal profile
  #     tar_target(
  #       optimal_profile,
  #       find_optimal_profile(
  #         model = product_model, 
  #         data = product_skin_data,
  #         model_variables = model_variables,
  #         use_pca = T
  #       ),
  #       packages = optimization_packages
  #     ),
  #     
  #     # create input for shiny app
  #     tar_target(
  #       input_to_shiny,
  #       make_shiny_input(
  #         target = target_name,
  #         all_product_data = product_data,
  #         modelstudio_file = modelstudio,
  #         product_data = product_skin_data,
  #         model = product_model,
  #         important_variables = model_variables,
  #         delta_tbl = delta_tbl,
  #         # optima = optimal_profile,
  #         calibration_plot = gof_plot,
  #         data_type = data_type,
  #         skin_tone = skin
  #       )
  #     )
  #   )
  #   
  # )
  )
)


# combined <- tar_combine(
#   combined_input_to_shiny,
#   # mapped[stringr::str_detect(names(mapped), "input_to_shiny_glmnet_earth_analytical")],
#   list(
#     mapped$input_to_shiny_1_glmnet_earth,
#     mapped$input_to_shiny_2_glmnet_earth,
#     mapped$input_to_shiny_3_glmnet_earth,
#     mapped$input_to_shiny_all_glmnet_earth,
#     mapped$input_to_shiny_1_glmnet,
#     mapped$input_to_shiny_2_glmnet,
#     mapped$input_to_shiny_3_glmnet,
#     mapped$input_to_shiny_all_glmnet),
#   command = dplyr::bind_rows(!!!.x) %>% 
#     filter(products_type %in% c("Analytical", "Sensory")) %>% 
#     filter(!(target == "Overall Liking Forearm&Face 10min After" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>%
#     filter(!(target == "Ad Is Lightweight Face 10min After" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>% 
#     filter(!(target == "Ad Leaves Skin Looking Glowing Face 10min After" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>% 
#     filter(!(target == "Ad Is Non-Greasy Face 10min After" & grepl("glmnet_earth",modelstudio_file))) %>% 
#     filter(!(target == "Ad Feels Dry To Touch Face 10min After" & grepl("glmnet_earth",modelstudio_file))) %>% 
#     filter(!(target == "Ad Feels Moisturized Face 10min After" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>% 
#     filter(!(target == "Ad Feels Smooth Face 10min After" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>% 
#     filter(!(target == "Ad Spreads Easily Forearm&Face Immed After" & products_type == "Sensory" & grepl("glmnet_earth",modelstudio_file))) %>% 
#     filter(!(target == "Ad Spreads Easily Forearm&Face Immed After" & products_type == "Analytical" & grepl("glmnet_a|glmnet_s",modelstudio_file))) %>% 
#     readr::write_rds("output/dashboard/models.rds", compress = "xz")
# )

# End this file with a list of target objects.
list(prep, mapped)
