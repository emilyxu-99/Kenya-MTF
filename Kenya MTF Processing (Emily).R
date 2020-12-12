library(tidyverse)
library(tidyr)
library(RCMIP5)
library(haven)
library(readstata13)
library(foreign)
library(sjlabelled)
library(expss)
library(seplyr)
library(fastDummies)
setwd("\\Users\\Emily Xu\\Documents\\Catalyst Off-Grid Advisors\\Access Insights Platform")

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

# Kenya Tiers

kenya_tiers <- haven_read('Kenya_Tier calculated.dta')
kenya_mtf2 <- kenya_tiers[,c('parent_key','elc_aggr_tier')]
kenya_mtf2_joined <- inner_join(kenya_mtf2, mtf2, by='parent_key')

write_csv(kenya_mtf2_joined, 'kenya_mtf2_joined.csv')


kenya_mtf2_t <- 
  kenya_mtf2_joined[c('parent_key','elc_aggr_tier',
                      'hh_grid', 'c_c_159', 'c_c_27b',
                      'c_c_25bii', 'c_c_30', 'c_c_31',
                      'c_140_pmt_syst', '')]







######## Initial Cleaning and Reading ####################

mtf_general <- stata_read(hh_file_name)
mtf2 <- haven_read(hh_file_name)

mtf_weights <- stata_read(weight_file_name)
mtf_weights <- mtf_weights[,c(3,12)]
national_weight = sum(mtf_weights$pw_final)

## Data Basic Exploration and Cleaning ##

mtf2$prov <- tolower(mtf2$prov)

#How many NAs are there in each column?
colSums(is.na(mtf2)) #Note - some are entirely NA - not worth anything

mtf_stats <- 
  mtf2 %>% 
  group_by(prov,dist) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

sd(mtf_stats$count)
summary(mtf_stats$count)

merge_key = 'parent_key'


####### Create tools for later ################

mtf_info <- mtf2[,c(seq(2,15,1))] 
mtf_info <- merge(mtf_info, mtf_weights, by = merge_key)

question_labs <- attr(mtf_general, "var.labels") 
mtf_names <- names(mtf_general)
names_labels <- t(tibble(mtf_names,question_labs))


### Finding missing values. Extract weights

#mtf2, mtf_info, mtf_weights

joined_values <- left_join(mtf2, mtf_weights, by = c('parent_key'='parent_key'))
missing_subs <- subset(joined_values,is.na(pw_final))
select(missing_subs, 'prov', 'cty', 'locality', 'loc', 'subloc')

# Data set excluding non-weighted values
mtf2 <- subset(joined_values, !is.na(pw_final))

#dropping the weights column
mtf2$pw_final <- NULL



#ANALYSIS

############# Grid Connection #########################

#First: Tag the appropriate cols with the question text
grid_connected <- mtf2[,str_detect(question_labs, "connected to the national grid")] 

#then bring in the geo information
grid_connected <- cbind(mtf_info,grid_connected) 

mtf_viz <- grid_connected
#Check what the categories look like
grid_connected %>% 
  group_by(hh_grid) %>% 
  summarise(total = n())

no_grid_info <- 
  grid_connected %>% 
  filter(grid_connected$hh_grid == 0) %>% 
  group_by(dist) %>% copy_labels(mtf2) %>% 
  summarise(count = n()) 

#Percent Connected to Grid Nationally:
grid_connected %>% 
  filter(!is.na(hh_grid)) %>% 
  group_by(hh_grid) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(hh_grid == 1)

#Percent Connected  to Grid by district (NICK):
freqOut<- 
  freqTemp %>%  
  filter(!is.na(var_name)) %>% 
  group_by(province, district, var_name) %>% 
  summarise(n = sum(hh_weight)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(var_name == 'Yes')




############# Generator Use ##########################

#First: Tag the appropriate cols with the question text
grid_connected <- 
  mtf2[,str_detect(question_labs, "connected to the national grid")] %>%  
  rename(Grid_conn = 1)

gen_use <- mtf2[,str_detect(question_labs, "household use a generator")]

#then bring in the geo information
mtf_viz <- cbind(mtf_viz,gen_use)
gen_use <- cbind(mtf_info,gen_use) 

#Check what the categories look like
gen_use %>% 
  group_by(GenUse) %>% 
  summarise(total = n())

#Percent Using Generators Nationally:
gen_use %>%  
  filter(!is.na(GenUse)) %>% 
  group_by(GenUse) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(GenUse == 'Yes')

#Percent Connected  to Grid by district:
gen_freq<- 
  gen_use %>%  
  filter(!is.na(GenUse)) %>% 
  group_by(prov, dist, GenUse) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(GenUse == 'Yes')

gen_freq<- 
  gen_use %>%  
  filter(!is.na(GenUse)) %>% 
  group_by(prov, GenUse) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(GenUse == 'Yes')


############# Rechargeable Battery Use ################################

#First: Tag the appropriate cols with the question text
batt_use <- 
  mtf2[,str_detect(question_labs, "household use any rechargeable batteries")]

#then bring in the geo information
batt_use <- cbind(mtf_info,batt_use) 

#Check what the categories look like
batt_use %>% 
  group_by(Batt_Use) %>% 
  summarise(total = n())

#Percent Using Generators Nationally:
batt_use %>%  
  filter(!is.na(Batt_Use)) %>% 
  group_by(Batt_Use) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(Batt_Use == 'Yes')

#Percent Connected  to Grid by district:
batt_freq<- 
  batt_use %>%  
  filter(!is.na(Batt_Use)) %>% 
  group_by(prov, dist, Batt_Use) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(Batt_Use == 'Yes')


#########################################

## Solar Use - NOT COMPLETED ##

#First: Tag the appropriate cols with the question text
batt_use <- 
  mtf2[,str_detect(question_labs, "household use any rechargeable batteries")] %>% 
  rename(Batt_Use = 1)

#then bring in the geo information
batt_use <- cbind(mtf_info,batt_use) 

#Check what the categories look like
batt_use %>% 
  group_by(Batt_Use) %>% 
  summarise(total = n())

#Percent Using Generators Nationally:
batt_use %>%  
  filter(!is.na(Batt_Use)) %>% 
  group_by(Batt_Use) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(Batt_Use == 'Yes')

#Percent Connected  to Grid by district:
batt_freq<- 
  batt_use %>%  
  filter(!is.na(Batt_Use)) %>% 
  group_by(prov, dist, Batt_Use) %>% 
  summarise(n = sum(pw_final)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(Batt_Use == 'Yes')


#########################################
## Kerosene Usage ##
#First, need new datasets
#test_f14 <- readstata13::read.dta13('SECTION F KEROSEN_FUEL BASED_CANDLE LIGHTING F1-F14.dta')
mtf_f14 <- haven_read('SECTION F KEROSEN_FUEL BASED_CANDLE LIGHTING F1-F14.dta',0)
mtf_f15 <- haven_read('SECTION F KEROSEN_FUEL BASED_CANDLE LIGHTING F16-F21.dta')

kerosene_use <- 
  mtf_f14 %>% 
  select(c(1,2,3,4,8))

### METHOD FOR GETTING THE SINGLE HOUSEHOLD REPRESENTATION OF VALUES

kerosene_use <- 
  fastDummies::dummy_cols(kerosene_use,select_columns = 'f3') %>% 
  group_by(householdid) %>% 
  summarise(candle = sum(f3_1),
            open_wick = sum(f3_2),
            hurricane = sum(f3_3),
            pressure = sum(f3_4),
            other = sum(f3_555),
            none = sum(f3_5))

kerosene_use <- merge(mtf_info,kerosene_use)

#National Frequenci-
kerosene_use %>% 
  filter(!is.na(candle)) %>% 
  group_by(province, candle) %>% 
  summarise(n = sum(hh_weight)) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(candle==1)


################# Dry Cell Battery Usage ###
mtf_dry_cell <- haven::read_dta('SECTION G DRY CELL BATTERIES.dta')
dryCellLabelled <- haven::as_factor(mtf_dry_cell)

g2labels <- haven::print_labels(mtf_dry_cell$G2)

wide_dryCell <- 
  dryCellLabelled %>% 
  pivot_wider(id_cols = HouseholdID, names_from = G2, values_from = G3) %>%  
  copy_labels(dryCellLabelled)

wide_dryCell %>% 
  setNames(paste0('Dry Cell ', names(.)))

merge_test <- 
  merge(offgrid_select, wide_dryCell) %>% 
  copy_labels(offgrid_select)

solar_questions <- 
  mtf2[,str_detect(question_labs, "household|solar|generator")] %>% 
  copy_labels(mtf2)

bat_charge <- 
  summary(offgrid_select$C126)
sum(bat_charge[-1])



#### Main Source of Power

#### MODULE 1 DATA - Energy Access #########


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

## Incorporating Solar Devices
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

## Incorporating Cooking Information
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

rlang::last_error()

secondary_cooking <- 
  cooking_roster %>% 
  group_by(parent_key) %>% 
  filter(i_position == 2) %>%  
  select(-i_position)

secondary_cooking <- 
  secondary_cooking %>% 
  rename_if(stringr::str_detect(names(.), "^i"), ~str_replace(.,'^.*?_[1-9]*_[a,b]?_?','Secondary_'))

energy_access_db <- 
  energy_access_db %>% 
  merge(primary_cooking, by = merge_key) %>% 
  merge(secondary_cooking, by = merge_key) %>%
  copy_labels(mtf2)

solar_pmt <-
  solar_roster %>% 
  filter(c_137_acquisition == 'Bought, under installment') %>%  
  group_by(c_140_pmt_syst) %>% summarise(Total = n())

write_csv(solar_pmt, 'Solar Payments.csv')

###### MODULE 3 DATA - Financial Inclusion ######

key_terms = 
  c('Household Unique ID',
    'bank account', 
    'institution', 
    'loan/credit', 
    'mobile money')
paste(key_terms,collapse = "|")

fin_access <- 
  mtf2[,str_detect(question_labs, paste(key_terms,collapse = "|"))] %>% 
  copy_labels(mtf2)

fin_names <- 
  names_labels[2,str_detect(question_labs, paste(key_terms,collapse = "|"))]

names(fin_access) <- fin_names

fin_access <- 
  merge(mtf_info,fin_access, by.x = merge_key, by.y = 'Household Unique ID')

write_csv(fin_access, 'Kenya FinAccess Data.csv')

##### Write Outputs ############
write_csv(mtf2, 'Kenya MTF Data.csv')



#######################################################################
#This is an Appendix of code that isn't being used but may be useful going forward
offgrid <- 
  mtf2 %>% 
  filter(C2 == 'No') %>%  
  copy_labels(mtf2)

offgrid_select <- 
  offgrid %>% 
  select(HouseholdID, province, district, 
         TOWNSHIP_COMPOUND_VILLAGE, LOCALITY, 
         A25, A35, C83, C99,
         C119, C124, C125, C126, C126X, C127, C128, 
         C129, C130, C131, C136, C137, C138,C166, C166A, 
         C167,C168A, C168B, C169A, C169B, C170A, C170B, 
         C176, C177, C178,nC178X, C179, C180X, C181) 

#Data Exploration Graphs
mtf2 %>%  
  group_by(lof) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = lof, y = total)) + geom_bar(stat='identity')

mtf2 %>%  
  group_by(province,district) %>% 
  summarise(total = n()) 

mtf2 %>%  
  group_by(c2) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = c2, y = total)) + geom_bar(stat='identity')

test_search <- 
  mtf2[,str_detect(question_labs, "pay")] %>%  
  copy_labels(mtf2)

