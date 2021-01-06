library(tidyverse)
library(tidyr)
library(haven)
library(readstata13)
library(foreign)
library(sjlabelled)
library(expss)
library(seplyr)
library(fastDummies)
setwd("~/Catalyst Off-Grid Advisors/Access Insights Platform")
#################Supporting Functions #########################################

haven_read <- function(file_name, read_factor = 1){
  if(read_factor == 1){
    df <- haven::as_factor(haven::read_dta(file_name))
  }else{
    df <- haven::read_dta(file_name)
    
  }
  
  names(df) <- tolower(names(df))
  return(df)
  
}

stata_read <- function(file_name){
  df <- readstata13::read.dta13(file_name)
  names(df) <- tolower(names(df))
  return(df)
  
}
source_use_freq <- function(test_str, var_name){
  #First: Tag the appropriate cols with the question text
  freqTemp <- 
    mtf2[,str_detect(question_labs, test_str)] %>%  
    rename(var_name = 1)
  
  #then bring in the geo information
  freqTemp <- cbind(mtf_info,freqTemp) 
  
  #Check what the categories look like
  freqTemp %>% 
    group_by(!! sym(var_name)) %>% 
    summarise(total = n())
  
  return(freqOut)
}

##### FILENAMES #######

hh_file_name = 'MTF_HH_Core_Survey_Final_Data_trimmed-2.dta'
weight_file_name = 'weight.dta'
mtf2 <- haven_read(hh_file_name)

# Kenya Tiers

kenya_tiers <- haven_read('Kenya_Tier calculated.dta')
kenya_mtf2 <- kenya_tiers[,c('parent_key','elc_aggr_tier')]

kenya_mtf2_joined <- 
  list(kenya_mtf2, mtf2, primary_cooking, secondary_cooking) %>%  
  reduce(inner_join, by='parent_key')


key_terms = c('Household Unique ID',
              'HH ID', 
              'is connected to the grid', 
              'Electricity consumption', 
              'back-up source of lighting',
              'is connected to a mini-grid', 
              'How many hours d', 
              "generator the household's main source", 
              "Solar devices",
              'Of all the electricity sources', 
              'Has solar been', 
              'electricity when it was not a solar', 
              'total quantity of candles',
              'kerosene did your household')

energy_access_db <- 
  mtf2[,str_detect(question_labs, paste(key_terms,collapse = "|"))] %>% 
  copy_labels(mtf2)


## Solar
solar_roster = haven_read('MTF_HH_Solar_Roster.dta')
solar_labs <- attr(stata_read('MTF_HH_Solar_Roster.dta'), "var.labels") 

solar_roster <- 
  solar_roster[,str_detect(solar_labs, 'payment system|Household Unique')] %>% 
  copy_labels(mtf2) 

solar_pmt_count <- 
  solar_roster %>% 
  group_by(parent_key, c_140_pmt_syst) %>% 
  count() %>% 
  spread(key = c_140_pmt_syst, value = n)

energy_access_db <- 
  energy_access_db %>% 
  merge(solar_pmt_count, by = merge_key, all.x = TRUE) %>% 
  copy_labels(mtf2)

## Cooking

cooking_roster <- haven_read('MTF_HH_Cooking_Data_Final.dta')
cooking_labs <- attr(stata_read('MTF_HH_Cooking_Data_Final.dta'), "var.labels") 

cooking_roster <- 
  cooking_roster[,str_detect(cooking_labs, 'i_position|What was cookstove|what is the fuel you used|Household Unique')] %>% 
  copy_labels(mtf2)

primary_cooking <- 
  cooking_roster %>% 
  group_by(parent_key) %>% 
  filter(i_position == 1) %>%  
  select(-i_position)

primary_cooking <- 
  primary_cooking %>% 
  rename_if(stringr::str_detect(names(.), "^i"), ~str_replace(.,'^.*?_[1-9]*_[a,b]?_?','Primary_'))

secondary_cooking <- 
  cooking_roster %>% 
  group_by(parent_key) %>% 
  filter(i_position == 2) %>%  
  select(-i_position)

secondary_cooking <- 
  secondary_cooking %>% 
  rename_if(stringr::str_detect(names(.), "^i"), ~str_replace(.,'^.*?_[1-9]*_[a,b]?_?','Secondary_'))


## Select mini dataset
kenya_mtf2_joined <- 
  list(kenya_mtf2, mtf2, primary_cooking, secondary_cooking) %>%  
  reduce(left_join, by='parent_key')

kenya_mtf2_mini <- 
  kenya_mtf2_joined[grepl("parent_key|locality_ur|elc_aggr_tier|hh_grid|c_c_159|
                          primary_1st_fuel|primary_2nd_fuel|primary_stove_type|
                          secondary_stove_type|c_c_27b|c_c_25bii|c_c_30|c_c_31|
                          c_c_126|c_c_123|solar|c_140_pmt_syst|c_c_149b_typicalmonth|
                          c_c_156|c_c_159|f_f_|g_g_|c_c_113|c_c_119", 
                          names(kenya_mtf2_joined))]

kenya_mtf2_mini$hh_grid<-ifelse(kenya_mtf2_mini$hh_grid == 1, "Yes", "No")
kenya_mtf2_mini$solar<-ifelse(kenya_mtf2_mini$solar == 1, "Yes", "No")
kenya_mtf2_mini$c_123_158_solar_rpt <- NULL

write_csv(kenya_mtf2_mini, 'kenya_mtf2_mini.csv')










