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
data_map <- data[[11]]

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
  select(-'Consumer Code') %>%
  select(!ends_with("OE"))

# consumer demographics
consumer_demo <- consumer_raw_data %>%
  select(c(1,3:31)) %>%
  rename("id" = Serial) %>%
  select(-c(3, 4, 5, 8, 12, 13, 14, 15, 21, 26, 27, 28))
  
# exit question
consumer_exit <- consumer_raw_data %>%
  select(c(1,553:604)) %>%
  rename("id" = Serial)


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
map<-data_map %>% 
  select(entry, code) %>% 
  filter(., !is.na(code)) %>% 
  filter(., entry!="REPBOOST") %>%
  filter(., entry!="QHIDPROD") %>%
  filter(., entry!="SEGMENT_Q8") %>%
  filter(., entry!="SEGMENT_Q13") %>%
  filter(., entry!="L") %>%
  filter(., entry!="SEXACTAGE_1") %>%
  filter(., entry!="SREGIONUK") %>%
  filter(., entry!="SREGIONDE") %>%
  filter(., entry!="S4") %>%
  filter(., entry!="S7") %>%
  filter(., entry!="S14") %>%
  filter(., entry!="Q1A_OE") %>%
  filter(., entry!="Q6A_OE") %>%
  filter(., entry!="Q10A_OE") %>%
  filter(., entry!="Q16_OE") %>%
  filter(., substr(entry,1,1) != "E") %>%
  filter(., substr(entry,1,3) != "S19") %>%
  filter(., entry!=("QHIDE15"))

data_consumer_complete <- consumer_data %>%
  left_join(consumer_demo, by ="id") %>%
  rename_(.dots = setNames(map$entry, map$code))%>%
  rename_at(35, ~ paste("t", ., sep = "_")) %>%
  rename_at(-c(1,2,35), ~ paste("c", ., sep = "_")) %>%
  filter(., product!="NA") %>%
  mutate(c_s_country=case_when(c_s_location==1 ~ 1, 
                               c_s_location==2 ~ 1, 
                               c_s_location==3 ~ 1, 
                               c_s_location==4 ~ 2, 
                               c_s_location==5 ~ 2,
                               c_s_location==6 ~ 2)) %>%
  select(-c_s_location)

write.csv(x=data_consumer_complete, file="input/processed_data/20221227_danone_core_milk_consumer_data.csv", row.names=FALSE)



data_complete <- data_consumer_complete %>%
  left_join(sensory_milk_data, by = "product") %>%
  left_join(sensory_powder_data, by = "product") %>%
  left_join(sensory_prep_data, by = "product") %>%
  left_join(analytical_data, by = "product")

