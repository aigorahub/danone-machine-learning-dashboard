# Creating a workflow config class object and retrieving the appropriate model definition 

models_config <- function(model) {
  
  class(model) <- model
  config <- get_models_config(model)
  
  # Set class to workflow_config (see fit.workflow_config() ) unless already specified
  if(class(config) == "list")
    class(config) <- "workflow_config"
  
  return(config)
}

# add generic (schema S3 OOP)
get_models_config <- function(x) {
  UseMethod("get_models_config")
}

# default method  - get_config
get_models_config.default <- function(x) {
  available_methods <- methods(get_models_config) %>%
    as.character() %>%
    gsub(
      pattern = "get_models_config\\.",
      replacement = ""
    ) %>%
    setdiff("default") %>%
    paste(collapse = ", ")
  
  stop(sprintf(
    '\nUndefined config:  "%s"\nAvailable configs are: %s',
    class(x), available_methods
  ))
}

# get_m_co - creating a model object and recipe from the tidymodels package. Fit () is called on this object
  get_models_config.glmnet_earth_pca <- function(x) {
    
    model <- linear_reg(
      mixture = tune(),
      penalty = tune()
    ) %>%
      set_mode("regression") %>%
      set_engine("glmnet")
    
    model_grid <- model %>%
      parameters() %>%
      update(mixture = mixture(c(0, 1))) %>%
      grid_regular(levels = 10)
    
    rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
      if(is.null(predictors))
        predictors <- setdiff(colnames(data), c(target, adt_cols))
      
      formula <- as.formula(sprintf("%s ~ .", target))
      
      data %>% 
        select(all_of(c(target, predictors))) %>%
        recipe(formula = formula) %>%
        step_novel(all_nominal(), -all_outcomes()) %>%
        step_zv(all_predictors()) %>%
        step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
        step_impute_median(all_numeric(), -all_outcomes()) %>%
        step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
        step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
        step_nzv(all_predictors(), freq_cut = 5/1) %>%
        step_pca(starts_with("p_a_"), prefix = "PC_a",
                 options = list(center = TRUE, scale. = TRUE)) %>%
        step_pca(starts_with("p_s_"), prefix = "PC_s",
                 options = list(center = TRUE, scale. = TRUE)) %>%
        step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                   options = list(degree = 2))
    }
    
    # Model config to tibble
    config <- list(
      model = model,
      grid = model_grid,
      pre_rcp = rcp
    )
  }
get_models_config.glmnet_earth <- function(x) {

  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0.01, 1))) %>%
    grid_regular(levels = 20)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                 options = list(degree = 2))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.glmnet_earth_without_whitening <- function(x) {
  
  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0, 1))) %>%
    grid_regular(levels = 20)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols, "p_s_whitening_5_minutes", "p_s_whitening_1_minute", "p_s_whitening_immediate", "p_a_whitening", "p_s_whitening_5th_rotation"))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      # select(-c()) %>% 
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                 options = list(degree = 2))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.glmnet_earth_top_vars <- function(x) {
  
  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0, 1))) %>%
    grid_regular(levels = 20)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ p_a_whitening + p_a_r_viscosity_0_1 + p_a_r_storage_modulus_as_5_16 + p_a_r_min_strain_modulus_52 + p_a_r_loss_modulus_fs_0_32 + p_a_r_friction_work_150 +
                                  p_s_whitening_5_minutes + p_s_shine_1_minute + p_s_firmness + p_s_whitening_1_minute + p_s_wax_1_minute +
                                  c_gender + c_exact_age + c_skin_tone + c_skin_type + c_skin_sensitivity + c_race + c_ethnicity", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                 options = list(degree = 2))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.glmnet_pca <- function(x) {
  
  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0, 1))) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>% 
      step_pca(starts_with("p_a_"), prefix = "PC_a",
               options = list(center = TRUE, scale. = TRUE)) %>%
      step_pca(starts_with("p_s_"), prefix = "PC_s",
               options = list(center = TRUE, scale. = TRUE))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.glmnet_top_3 <- function(x) {
  
  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0, 1))) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ p_s_whitening_1_minute + p_s_surface_drag_5_minutes + p_a_qvalue_0_1", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.glmnet <- function(x) {

  model <- linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  model_grid <- model %>%
    parameters() %>%
    update(mixture = mixture(c(0, 1))) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.linear_reg <- function(x) {
  
  model <- linear_reg(
  ) %>%
    set_mode("regression") %>%
    set_engine("lm")
  
  model_grid <- model %>%
    parameters()
    # update(mixture = mixture(c(0, 1))) %>%
    # grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}
get_models_config.linear_reg_whitening <- function(x) {
  
  model <- linear_reg(
  ) %>%
    set_mode("regression") %>%
    set_engine("lm")
  
  model_grid <- model %>%
    parameters()
  # update(mixture = mixture(c(0, 1))) %>%
  # grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ p_a_whitening", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.ordinal_reg_earth <- function(x) {
  model <- ordinal_reg(
    mixture = tune(),
    penalty = tune()
    ) %>%
    set_engine("glmnetcr") %>%
    set_mode("classification") 

  raw_data_category <- raw_data %>%
    mutate(type_product = case_when(str_detect(Product, "vodka") ~ "vodka",
                                    str_detect(Product,"rum") ~ "rum",
                                    TRUE ~ "other"))
  
  
  model_grid <- model %>%
    parameters() %>%
    update(
      mixture = mixture(c(0, 1)),
      penalty = penalty(c(-7, -3), log2_trans())
    ) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                 options = list(degree = 2))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.ordinal_reg_pca_earth <- function(x) {
  model <- ordinal_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_engine("glmnetcr") %>%
    set_mode("classification") 

  
  model_grid <- model %>%
    parameters() %>%
    update(
      mixture = mixture(c(0, 1)),
      penalty = penalty(c(-7, -3), log2_trans())
    ) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_pca(starts_with("p_a_"), prefix = "PC_a",
               options = list(center = TRUE, scale. = TRUE)) %>%
      step_pca(starts_with("p_s_"), prefix = "PC_s",
               options = list(center = TRUE, scale. = TRUE)) %>%
      step_earth(all_numeric(), -all_outcomes(), outcome = target, drop = FALSE,
                 options = list(degree = 2))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.ordinal_reg_pca <- function(x) {
  model <- ordinal_reg(
    mixture = tune(),
    penalty = tune()
  ) %>%
    set_engine("glmnetcr") %>%
    set_mode("classification") 

  
  model_grid <- model %>%
    parameters() %>%
    update(
      mixture = mixture(c(0, 1)),
      penalty = penalty(c(-7, -3), log2_trans())
    ) %>%
    grid_regular(levels = 10)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1) %>%
      step_pca(starts_with("p_a_"), prefix = "PC_a",
               options = list(center = TRUE, scale. = TRUE)) %>%
      step_pca(starts_with("p_s_"), prefix = "PC_s",
               options = list(center = TRUE, scale. = TRUE))
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}


get_models_config.nnet <- function(x) {
  model <- mlp(
    hidden_units = 3,
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("classification") 
  
  model_grid <- model %>%
    parameters() %>%
    update(epochs = epochs(range = c(10L, 100L))) %>%
    grid_regular(levels = 4)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    if(is.null(predictors))
      predictors <- setdiff(colnames(data), c(target, adt_cols))
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}


get_models_config.nnet_sensory <- function(x) {
  model <- mlp(
    hidden_units = 3,
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("classification") 
  
  
  model_grid <- model %>%
    parameters() %>%
    update(epochs = epochs(range = c(10L, 100L))) %>%
    grid_regular(levels = 4)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    
    all_predictors <- c("p_type", str_subset(colnames(data), "^p_s_"))
    
    if(is.null(predictors)) {
      predictors <- all_predictors
    } else {
      predictors <- intersect(all_predictors, predictors)
    }
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}


get_models_config.nnet_rheo <- function(x) {
  model <- mlp(
    hidden_units = 3,
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("classification") 
  
  
  model_grid <- model %>%
    parameters() %>%
    update(epochs = epochs(range = c(10L, 100L))) %>%
    grid_regular(levels = 4)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    
    all_predictors <- c("p_type", "p_a_g001", "p_a_g10", "p_a_g50", "p_a_yield")
    
    if(is.null(predictors)) {
      predictors <- all_predictors
    } else {
      predictors <- intersect(all_predictors, predictors)
    }
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}

get_models_config.nnet_texture <- function(x) {
  model <- mlp(
    hidden_units = 3,
    penalty = tune(),
    epochs = tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("classification") 
  
  
  model_grid <- model %>%
    parameters() %>%
    update(epochs = epochs(range = c(10L, 100L))) %>%
    grid_regular(levels = 4)
  
  rcp <- function(data, target, predictors = NULL, adt_cols = character()) {
    
    all_predictors <- c("p_type", "p_a_work_shear", "p_a_fimn",
                        "p_a_consist", "p_a_cohe", "p_a_adhe_1min",
                        "p_a_work_adhe_1min", "p_a_adhe_5min", "p_a_work_adhe_5min")
    
    if(is.null(predictors)) {
      predictors <- all_predictors
    } else {
      predictors <- intersect(all_predictors, predictors)
    }
    
    formula <- as.formula(sprintf("%s ~ .", target))
    
    data %>% 
      select(all_of(c(target, predictors))) %>%
      recipe(formula = formula) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_zv(all_predictors()) %>%
      step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>%
      step_impute_median(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_mutate_at(all_numeric(), -all_outcomes(), fn = ~ifelse(is.na(.), 0, .)) %>% 
      step_nzv(all_predictors(), freq_cut = 5/1)
  }
  
  # Model config to tibble
  config <- list(
    model = model,
    grid = model_grid,
    pre_rcp = rcp
  )
}