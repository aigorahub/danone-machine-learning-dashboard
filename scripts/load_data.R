source(file.path("scripts", "load_libraries.R"))

filepath <- file.path("input/20221208_Pref Map UK and DE - Aigora.xlsx")

data <- rio::import_list(filepath, setclass = "tbl")

consumer_raw_data <- data [[3]] 
sensory_milk_raw_data <- data[[8]]
sensory_powder_raw_data<- data [[9]]
sensory_prep_raw_data <- data [[10]]
analytical_raw_data <- data[[5]]
consumer_raw_datamap <- data[[4]]
product_names_codes <- data[[2]]

### Clean consumer data
product_codes <- unique(consumer_raw_data$QHIDPROD1)
nprod<-length(product_codes)
# remove unnecessary columns
consumer_data_wide<-consumer_raw_data %>%
  # rename_with(., ~ tolower(.x)) %>%
  select(-c(2:45,553:604)) 
# create list with one tibble per product
consumer_data_list<-list()
for (i in 1:nprod){
  consumer_data_list[[i]]<-consumer_data_wide %>%
    select(Serial, ends_with(as.character(product_codes[i]))) %>%
    rename_with(., ~ str_sub(., end = -4)) #remove product code index from question code
  names(consumer_data_list)[i] <- max(unique(consumer_data_list[[i]]$qhidimpproduct1))
}
# create tibble with one row per consumer*product pair
consumer_data <- map_dfr(consumer_data_list, bind_rows) %>%
  # rename("id" = ser) %>%
  # rename("product_code" = qhidimpproduct1) %>%
  select(-c(3)) %>%
  rename("id" = Ser) %>%
  rename('Consumer Code' = QHIDIMPPRODUCT1)
consumer_code <- product_names_codes[1:2]

consumer_data <- left_join(consumer_data, consumer_code, by='Consumer Code') %>%
  relocate('Suggested Name', .after = id) %>%
  rename("product" = 'Suggested Name') %>%
  select(-'Consumer Code')

# consumer demographics
consumer_demo <- consumer_raw_data %>%
  select(c(1,3:31)) %>%
  rename("id" = Serial)

# exit question
consumer_exit <- consumer_raw_data %>%
  select(c(1,553:604)) %>%
  rename("id" = Serial)

# select target 
consumer_target <- consumer_data

# select explanatory var
consumer_expl <- consumer_data


### datamap
consumer_datamap <- consumer_raw_datamap %>%
  slice(c(1:155, 351:568, 3185:3736)) %>%
  mutate(entry = if_else(str_starts(entry, "Q\\d"), str_sub(entry, end = -4), entry))


### Clean sensory data
sensory_milk_data <- sensory_milk_raw_data %>% 
  rename("product_name" = "...3", "product_code"="product or block") %>%
  filter(!row_number() %in% c(1)) %>%
  rename_at(6:ncol(.), ~ paste("p_s_m_", ., sep="")) %>%
  select("product_name", starts_with("p_s_m")) %>%
  group_by(product_name) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
sensory_milk_code <- product_names_codes[c(1,7)]%>%
  rename("product_name"='Sensory Milk Name')
sensory_milk_data <- left_join(sensory_milk_data, sensory_milk_code, by="product_name") %>%
  relocate('Suggested Name') %>%
  rename("product" = 'Suggested Name') %>%
  select(-product_name)

sensory_powder_data <- sensory_powder_raw_data %>% 
  rename("product_name" = "...3", "product_code"="product or block") %>%
  filter(!row_number() %in% c(1)) %>%
  rename_at(6:ncol(.), ~ paste("p_s_p_", .)) %>%
  select("product_name", starts_with("p_s_p")) %>%
  group_by(product_name) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
sensory_powder_code <- product_names_codes[c(1,9)]%>%
  rename("product_name"='Sensory Powder Name')
sensory_powder_data <- left_join(sensory_powder_data, sensory_powder_code, by="product_name") %>%
  relocate('Suggested Name') %>%
  rename("product" = 'Suggested Name') %>%
  select(-product_name)

sensory_prep_data <- sensory_prep_raw_data %>%
  rename_with(., ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  rename_with(., ~ gsub("#", "number_", .x, fixed = TRUE)) %>%
  rename_at(3:ncol(.), ~ paste("p_s_pr_", .)) %>%
  select("product_name", starts_with("p_s_pr")) %>%
  group_by(product_name) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
sensory_prep_code <- product_names_codes[c(1,10)]%>%
  rename("product_name"='Sensory Preparation Name')
sensory_prep_data <- left_join(sensory_prep_data, sensory_prep_code, by="product_name") %>%
  relocate('Suggested Name') %>%
  rename("product" = 'Suggested Name') %>%
  select(-product_name)

### Clean analytical data
analytical_data <- analytical_raw_data %>%
  row_to_names(row_number = 1) %>%
  rename_with(., ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  rename("product_name"="40degrees_only") %>%
  rename_at(3:ncol(.), ~ paste("p_a_", ., sep="")) %>%
  select("product_name", starts_with("p_a"))
analytical_code <- product_names_codes[c(1,4)]%>%
  rename("product_name"='Analytical Name')
analytical_data <- left_join(analytical_data, analytical_code, by="product_name") %>%
  relocate('Suggested Name') %>%
  rename("product" = 'Suggested Name') %>%
  select(-product_name)

### Combine datasets:
data_complete <- consumer_data %>%
  left_join(consumer_demo, by ="id") %>%
  left_join(consumer_exit, by ="id") %>%
  left_join(sensory_milk_data, by = "product") %>%
  left_join(sensory_powder_data, by = "product") %>%
  left_join(sensory_prep_data, by = "product") %>%
  left_join(analytical_data, by = "product")

  
  
