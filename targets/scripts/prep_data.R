#' # library(tidyverse)
#' # sensory_data_file <- config$sensory_data_file
#' # consumer_data_file <- config$consumer_data_file 
#' # analytical_data_file <- config$analytical_data_file
#' # attributes_mapping_file <- config$attributes_mapping_file
#' 
#' 
#' #' Prepare data
#' #'
#' #' Function for transforming the raw data and create one data frame for modeling 
#' #'
#' #' @param sensory_data_file path to data.frame with sensory data
#' #' @param whitening_data_file path to data.frame with whitening data
#' #' @param consumer_data_file path to data.frame with consumer data
#' #' @param rheology_data_file path to data.frame with rheology data
#' 
#' #' @return data.frame contains sensory, whitening, consumer, rheology
#' 
prep_data <- function(data_file) {
  data <- read_csv(data_file) %>% 
    select(id, product, c_s_country, starts_with("c_q"), starts_with("c_s")) %>% 
    # select(-c_s_brand_reason) %>% 
    # rename(c_s_gender = t_s_gender) %>% 
    mutate(across(c(c_s_organic, c_s_brand_use, c_s_parent_type, c_s_child_gender, c_s_gender,
                  c_s_relationship, c_s_responsible_primary_care, c_s_responsible_purchase_product_child,
                  c_s_breastfed, c_s_breastmilk_in_bottle, c_s_breastfed_duration, c_s_format,
                  c_s_working_status, c_s_social), ~as.factor(as.character(.x)))) %>% 
    mutate(across(starts_with("c_q_cata"), ~ifelse(.x == 2,  0, .x)))
  
  # consumer_data <- read_rds("input/processed_data/final_consumer_data_model_ol.rds") %>%
  #   select(-race, -ethnicity) %>%
  #   dplyr::rename_with(~ paste0("c_", .), -c(id, product, `t_overall_liking_forearm&face_10min after`,
  #                                            "t_ad_is_lightweight_face_10min after", "t_ad_is_non-greasy_face_10min after", "t_ad_feels_dry_to_touch_face_10min after",
  #                                            "t_ad_feels_moisturized_face_10min after", "t_ad_feels_smooth_face_10min after",
  #                                            "t_ad_spreads_easily_forearm&face_immed after", "t_ad_leaves_skin_looking_glowing_face_10min after"))
  # whitening_data <- read_rds(whitening_data_file) %>% # Need to remove formulation info
  #   select(product, p_a_whitening)
  # rheology_data <- read_rds(rheology_data_file)
  # # Analytical (excluding formulation) + Sensory / Target = Overall Liking
  # 
  # data_model <- consumer_data %>%
  #   left_join(sensory_data) %>%
  #   left_join(whitening_data) %>%
  #   left_join(rheology_data) %>%
  #   relocate(`t_overall_liking_forearm&face_10min after`, .after = last_col()) %>%
  #   unnest() %>%
  #   mutate(p_a_r_qvalue_0.1 = ifelse(is.na(p_a_r_qvalue_0.1), mean(p_a_r_qvalue_0.1, na.rm = T), p_a_r_qvalue_0.1)) %>%
  #   select(-c(p_s_other_5_minutes, p_s_other_1_minute))
  # consumer_data %>%
  #   mutate(
  #     c_skin_type = coalesce(c_face_skin_type, c_body_skin_type),
  #     c_skin_tone = coalesce(c_face_skin_tone, c_body_skin_tone)
  #   ) %>%
  #   # mutate(t_overall_liking = factor(
  #   #   t_overall_liking, levels = 1:5,
  #   #   labels = c("Dislike", "Dislike", "Neither", "Like", "Like")
  #   # )) %>%
  #   # distinct(t_overall_liking) %>% pull(t_overall_liking)
  #   select(-c_age, -starts_with("c_face"),-starts_with("c_body")) %>%
  #   inner_join(sensory_data, by = c("sample")) %>%
  #   inner_join(analytical_data, by = c("sample", "type")) %>%
  #   rename_with(~if_else(
  #     . %in% attributes_mapping$original,
  #     attributes_mapping$new[which(attributes_mapping$original == .)],
  #     .
  #   ), .cols = c(starts_with("s_"), starts_with("a_"))) %>%
  #   rename_with(sprintf, fmt = "p_%s", .cols = c(starts_with("s_"), starts_with("a_"))) %>%
  #   rename(p_type = type) %>%
  #   select(product = sample, cons_id, starts_with("t_"), starts_with("c_"), starts_with("p_"))
}
#' 
#' # data <- tar_read(data)
#' # target_column <- config$data_config$target_name[[1]]
#' # target_type <- config$data_config$target_type[[1]]
#' 
#' 
#' 
#' #' Prepare model data
#' #'
#' #' Function for transforming the raw data and create data frame for modeling
#' #'
#' #' @param data data.frame for modeling
#' #' @param target_column  predictors
#' #' @param target_type dependent variable
#' #' @param data_type input data.frame (analytical - analytical data, sensory - sensory data, all - analytical and sensory data)
#' #'
#' #' @return data.frame contain always consumer data and selected data type
#' 
#' 
prep_model_data <- function(data, include_cata, country, target_column) {
  
  if(include_cata == FALSE){
    data <- data %>% 
      select(-starts_with("c_q_cata"))
  }
  
  model_data <- data %>%
    rename(target = !! target_column) %>%
    janitor::clean_names() %>%
    filter(!is.na(target))
  
  if(country == "all") return(model_data)
  if(country == "1") return(model_data %>% filter(c_s_country == 1))
  if(country == "2") return(model_data %>% filter(c_s_country == 2))

}
# data = tar_read(model_data_all_t_overall_liking_forearm.face_10min.after_liking_product)

#' Get product data
#'
#' Function for collecting products data
#'
#' @param data data.frame with product data


#' @return data.frame contains product related data


get_product_data <- function(data) {
  data %>%
    select(product, target, starts_with("p_")) %>%
    group_by(product) %>%
    mutate(target = mean(target)) %>%
    ungroup() %>%
    distinct()
}

#' Get consumer data
#'
#' Function for collecting consumer data
#'
#' @param data data.frame with consumer data
#'
#' @return data.frame contains consumer data

get_consumer_data <- function(data) {
  data %>%
    select(id, starts_with("c_")) %>%
    distinct()
}
