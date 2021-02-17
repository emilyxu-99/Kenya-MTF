# Module 2
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

######## Initial Cleaning and Reading ####################

mtf_general <- stata_read(hh_file_name)
mtf2 <- haven_read(hh_file_name)

## Tools for later
question_labs <- attr(mtf_general, "var.labels")
question_labs2 <- attr(entire_mtf, "var.labels")

mtf_names <- names(mtf_general)
names_labels <- t(tibble(mtf_names,question_labs))

#mtf_entire_names <- names(entire_mtf)
entire_names_labels <- t(tibble(mtf_entire_names, question_labs2))

##### FILENAMES #######

hh_file_name = 'MTF_HH_Core_Survey_Final_Data_trimmed-2.dta'
weight_file_name = 'weight.dta'
mtf2 <- haven_read(hh_file_name)

# Remove extra data sets
rm(mtf_general)

# Entire Data set
entire_mtf <- haven_read('Kenya_Tier calculated.dta')

module2 <-
  entire_mtf[grepl(("parent_key|locality|elc_aggr_tier|locality_ur|d_3_wtp_cnx|d_4_wtp_3m|d_5_reason|d_6_wtp_6m|d_7_reason|d_8_wtp_12m|d_9_reason|d_10_no_fee|d_11_reason|d_12_elec_usage|e_e_2|e_e_3|e_e_4|e_e_5|e_e_6|e_e_7|e_e_8|f_f_|g_g_|h_h_|i_i_|k_k_|l_l_"), 
                   names(entire_mtf))]

# Pick one question
d3 <- entire_mtf[grepl(("d_3_wtp_cnx"), names(entire_mtf))]

# Test - replace 0 with No
e <- module2[,c('e_e_3','e_e_5','e_e_7')]
e$e_e_3 <- replace(e$e_e_3, e$e_e_3 == 0, 'No')
e$e_e_5 <- replace(e$e_e_5, e$e_e_5 == 0, 'No')
e$e_e_7 <- replace(e$e_e_7, e$e_e_7 == 0, 'No')

# Update the module data
module2$e_e_3 <- replace(module2$e_e_3, module2$e_e_3 == 0, 'No')
module2$e_e_5 <- replace(module2$e_e_5, module2$e_e_5 == 0, 'No')
module2$e_e_7 <- replace(module2$e_e_7, module2$e_e_7 == 0, 'No')

# 888 - unsure

module2$d_12_elec_usage <- replace(module2$d_12_elec_usage, module2$d_12_elec_usage == 888, NA)
module2$d_12_elec_usage <- replace(module2$d_12_elec_usage, module2$d_12_elec_usage == 'Unsure', NA)


#f_f_ detail

module2$f_f_2_candle<-ifelse(module2$f_f_2_candle == 1, "Yes", "No")
module2$f_f_2_openwick<-ifelse(module2$f_f_2_openwick == 1, "Yes", "No")
module2$f_f_2_hurr<-ifelse(module2$f_f_2_hurr == 1, "Yes", "No")
module2$f_f_2_press<-ifelse(module2$f_f_2_press == 1, "Yes", "No")
module2$f_f_2_other<-ifelse(module2$f_f_2_other == 1, "Yes", "No")
module2$f_f_2_none<-ifelse(module2$f_f_2_none == 1, "Yes", "No")




# Nested if/else statements: Categorize data accordingly for lamp detail.
# For example, if a household used candles, return "Candles".
# If a household used candles, return quantity of candles.
# If a household used candles, return the number of months.

# Returns Lamp Type based on survey response

module2$lamp_type <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         print("Candles"),
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                print("Open Wick Lamps"), 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       print("Hurricane Lamps"), 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              print("Pressurized Lamps"),
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     print("Other Lamps"),
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            print("Did not use candles or lamps"), 
                                            
                                    module2$f_f_otherlamp_12_other))))))

# Returns the number of lamps / candles a household uses based on survey response

module2$quantity_lamp_candles <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         module2$f_f_candle_10,
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                module2$f_f_openwick_5, 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       module2$f_f_hurricanelamp_5, 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              module2$f_f_pressurizedlamp_5,
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     module2$f_f_otherlamp_5,
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            
                                            NA, 
                                            
                                            module2$f_f_otherlamp_12_other))))))


# Returns the number of months a household used lamps/candles based on survey

module2$num_months <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         module2$f_f_candle_7,
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                module2$f_f_openwick_7, 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       module2$f_f_hurricanelamp_7, 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              module2$f_f_pressuirizedlamp_7,
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     module2$f_f_otherlamp_7,
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            
                                            NA, 
                                            
                                            NA))))))


# Returns the number of days a household used lamps/candles based on survey

module2$num_days <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         module2$f_f_candle_8,
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                module2$f_f_openwick_8, 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       module2$f_f_hurricanelamp_8, 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              module2$f_f_pressurizedlamp_8,
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     module2$f_f_otherlamp_8,
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            
                                            NA, 
                                            
                                            NA))))))

# Returns the number of hours a household used lamps/candles based on survey

module2$num_hours <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         module2$f_f_candle_9,
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                module2$f_f_openwick_9, 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       module2$f_f_hurricanelamp_9, 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              module2$f_f_pressurizedlamp_9,
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     module2$f_f_otherlamps_9,
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            
                                            NA, 
                                            
                                            NA))))))

# Returns the amount spent per week

module2$spent_weekly <- 
  
  ifelse(module2$f_f_2_candle == 'Yes',
         module2$f_f_candle_11,
         
         ifelse(module2$f_f_2_openwick == 'Yes', 
                module2$f_f_openwick_11, 
                
                ifelse(module2$f_f_2_hurr == 'Yes', 
                       module2$f_f_hurricanelamp_11, 
                       
                       ifelse(module2$f_f_2_press == 'Yes', 
                              module2$f_f_pressurizedlamp_11,
                              
                              ifelse(module2$f_f_2_other == 'Yes', 
                                     module2$f_f_otherlamp_11,
                                     
                                     ifelse(module2$f_f_2_none == 'Yes', 
                                            
                                            NA, 
                                            
                                            NA))))))


# g_g_ detail
module2$g_g_2_lant<-ifelse(module2$g_g_2_lant == 1, "Yes", "No")
module2$g_g_2_flash<-ifelse(module2$g_g_2_flash == 1, "Yes", "No")
module2$g_g_2_task<-ifelse(module2$g_g_2_task == 1, "Yes", "No")
module2$g_g_2_radio<-ifelse(module2$g_g_2_radio == 1, "Yes", "No")
module2$g_g_2_other<-ifelse(module2$g_g_2_other == 1, "Yes", "No")
module2$g_g_2_none<-ifelse(module2$g_g_2_none == 1, "Yes", "No")


# Nested if/else statements: Categorize data accordingly for dry-cell detail.
# For example, if a household used candles, return "Candles".
# If a household used candles, return quantity of candles.
# If a household used candles, return the number of months.

# Returns Lamp Type based on survey response

module2$dry_cell_type <- 
  
  ifelse(module2$g_g_2_lant == 'Yes',
         print("Lanterns"),
         
         ifelse(module2$g_g_2_flash == 'Yes', 
                print("Flashlights"), 
                
                ifelse(module2$g_g_2_task == 'Yes', 
                       print("Task lights"), 
                       
                       ifelse(module2$g_g_2_radio == 'Yes', 
                              print("Radio"),
                              
                              ifelse(module2$g_g_2_other == 'Yes', 
                                     print("Other light source"),
                                     
                                     ifelse(module2$g_g_2_none == 'Yes', 
                                            print("Did not use dry cell batteries"), 
                                            
                                            module2$f_f_otherlamp_12_other))))))

# Dry cell: returns the number of lanterns / lights powered by dry-cell batteries

module2$dry_cell_quantity <- 
  
  ifelse(module2$g_g_2_lant == 'Yes',
         module2$g_g_lantern_4,
         
         ifelse(module2$g_g_2_flash == 'Yes', 
                module2$g_g_flashlight_4, 
                
                ifelse(module2$g_g_2_task == 'Yes', 
                       module2$g_g_tasklight_4, 
                       
                       ifelse(module2$g_g_2_radio == 'Yes', 
                              module2$g_g_radio_4,
                              
                              ifelse(module2$g_g_2_other == 'Yes', 
                                     NA, # Note: linked to other survey
                                     
                                     ifelse(module2$g_g_2_none == 'Yes', 
                                            NA, 
                                            
                                            NA))))))


# Dry cell: Returns the number of months a household used lamps/candles based on survey

module2$dry_cell_months <- 
  
  ifelse(module2$g_g_2_lant == 'Yes',
         module2$g_g_lantern_6,
         
         ifelse(module2$g_g_2_flash == 'Yes', 
                module2$g_g_flashlight_6, 
                
                ifelse(module2$g_g_2_task == 'Yes', 
                       module2$g_g_tasklight_6, 
                       
                       ifelse(module2$g_g_2_radio == 'Yes', 
                              module2$g_g_radio_6,
                              
                              ifelse(module2$g_g_2_other == 'Yes', 
                                     NA, # Note: linked to other survey
                                     
                                     ifelse(module2$g_g_2_none == 'Yes', 
                                            NA, 
                                            
                                            NA))))))

# Dry cell: Returns the number of hours used in a typical day 

module2$dry_cell_hours <- 
  
  ifelse(module2$g_g_2_lant == 'Yes',
         module2$g_g_lantern_7,
         
         ifelse(module2$g_g_2_flash == 'Yes', 
                module2$g_g_flashlight_7, 
                
                ifelse(module2$g_g_2_task == 'Yes', 
                       module2$g_g_tasklight_7, 
                       
                       ifelse(module2$g_g_2_radio == 'Yes', 
                              module2$g_g_radio_7,
                              
                              ifelse(module2$g_g_2_other == 'Yes', 
                                     NA, # Note: linked to other survey
                                     
                                     ifelse(module2$g_g_2_none == 'Yes', 
                                            NA, 
                                            
                                            NA))))))

# write CSV
write_csv(module2, "module2.csv")














