# AIP Module Mock-up Calculations

# Remove extra data
rm(mtf_weights, mtf_info,mtf_general)
rm(missing_subs)

mtf2$hh_grid <- as.numeric(as.character(mtf2$hh_grid))
mtf2$hh_grid<-ifelse(mtf2$hh_grid == 1, "Yes", "No")

locality_ur <- mtf2[c('parent_key', 'locality_ur')]
grid_connected <- mtf2[c('parent_key', 'hh_grid', 'locality_ur')]

# Primary source of lighting

primary_joined_stats <-
  left_join(mtf2[1:4586,c("parent_key","c_c_159")], 
            grid_connected, by = c("parent_key" = "parent_key")) %>% 
  filter(!is.na(c_c_159)) %>% 
  group_by(hh_grid,c_c_159) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(freq = n / sum(n))

primary_joined<- 
  left_join(mtf2[1:4586,c("parent_key","c_c_159")], 
            grid_connected, by = c("parent_key" = "parent_key")) %>% 
  filter(!is.na(c_c_159)) %>% 
  group_by(hh_grid,c_c_159)

# Convert yes/no
primary_joined$hh_grid <- as.numeric(as.character(primary_joined$hh_grid))
primary_joined$hh_grid<-ifelse(primary_joined$hh_grid == 1, "Yes", "No")

write_csv(primary_joined, 'primary_joined.csv')

# c_c_159_other

primary_other <- 
  left_join(mtf2[1:4586,c("parent_key","c_c_159_other")], 
            grid_connected, by = c("parent_key" = "parent_key")) %>% 
  group_by(c_c_159_other) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(-(freq))

primary_other <- primary_other[2:75,]


## Primary source of cooking fuel 

primary_cooking_metric<-
  primary_cooking %>% 
  group_by(Primary_1st_fuel) %>% # Primary_1st_fuel
  dplyr::summarize(n = n()) %>% 
  filter(!is.na(Primary_1st_fuel)) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(-(freq))

primary_cooking <- 
  inner_join(primary_cooking, grid_connected, by = 'parent_key')

mtf2[,str_detect(question_labs, 'stove')]

write_csv(primary_cooking, 'primary_cooking.csv')

# HHs w.grid connection

write_csv(grid_connected, 'grid_connection.csv')

# HHs w/a solar system
solar_system <- mtf2[c('parent_key', 'locality_ur', 'c_c_126')]

write_csv(solar_system, 'hhs_w_solar_system.csv')

# HHs with solar system w/charging port -- 350/4586 = 7.63%
solar_port_hhs<- 
  mtf2[,c("parent_key","hh_grid", "locality_ur", "c_c_123")] %>% 
  filter(!is.na(c_c_123)) %>% 
  filter(c_c_123 > 0) %>% 
  group_by(locality_ur) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(freq = n / 4586) %>% 
  arrange(-(freq))

question_labs

# Average daily solar use
solar_metric <- 
  mtf2[c("solar", "hh_grid", "locality_ur", 
         "c_c_147b_typicalmonth", 
         "c_c_149b_typicalmonth")] %>% 
  copy_labels(mtf2) %>% 
  filter(solar > 0) %>% 
  filter(!is.na(c_c_149b_typicalmonth)) 

question_labs 
write_csv(solar_metric, '')
  
# Convert column to numeric, not individual values
solar_metric[,6:7] <- 
  lapply(solar_metric[6:7], function(x) as.numeric(as.character(x))) 

sapply(solar_metric, class)

# Aggregate by group: urban 6.41 hrs, rural 6.99 hrs

solar_avg <- data.frame(aggregate(solar_metric[,7], 
            by = list(solar_metric$locality_ur), 
            FUN = mean))

# HHs reliant on stopgap lighting (no electricity) -- 1105 / 4586 = 24.09%

stopgap_hhs <- left_join(mtf2[1:4586,c("parent_key","c_c_159")], 
                    grid_connected, by = c("parent_key" = "parent_key")) %>% 
  filter(!is.na(c_c_159) & c_c_159 == "No electricity") %>% 
  group_by(locality_ur) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

write_csv(stopgap_hhs, "stopgap_hhs.csv")

# Avg. daily stopgap lighting use (during black-outs?), 0.952 hrs

stopgap <- 
  mtf2[,"c_c_29b_hrs"] %>% 
  filter(!c_c_29b_hrs == "Do not know")
  
stopgap$c_c_29b_hrs <- as.numeric(as.character(stopgap$c_c_29b_hrs))
sapply(stopgap$c_c_29b_hrs, class)
sum(stopgap$c_c_29b_hrs) / (1839*7)


# Avg. consumption - kerosene primary, open wick / hurricane / pressurized / other

kerosene_table <- mtf2[c("f_f_3", "f_f_openwick_4", "f_f_openwick_5", "f_f_openwick_7",
                         "f_f_openwick_8", "f_f_openwick_9", "f_f_hurricanelamp_4",
                         "f_f_hurricanelamp_5", "f_f_hurricanelamp_6", "f_f_hurricanelamp_7",
                         "f_f_pressurizedlamp_4", "f_f_pressurizedlamp_5", "f_f_pressuriizedlamp_6",
                         "f_f_pressuirizedlamp_7", "f_f_pressurizedlamp_8","f_f_pressurizedlamp_9",
                         "f_f_pressurizedlamp_11", "f_f_otherlamp_4", "f_f_otherlamp_5",
                         "f_f_otherlamp_8", "f_f_otherlamps_9", "f_f_14", "h_h_12e", "h_h_13e")]

stopgap_table <- mtf2[grepl("f_f_", names(mtf2))]

openwick_kerosene <- mtf2[grepl("f_f_openwick",names(mtf2))]
hurricane_kerosene <- mtf2[grepl("f_f_hurricanelamp",names(mtf2))]
pressurized_kerosene <- mtf2[grepl("f_f_pressurized",names(mtf2))]
other_kerosene <- mtf2[grepl("f_f_otherlamp",names(mtf2))]

# Open wick: 62.75 liters/month

openwick_kerosene <- 
  kerosene_table[, c(1:5, 22:23)] %>% 
  filter(!is.na(f_f_openwick_4)) %>% 
  filter(!is.na(h_h_12e)) %>% 
  filter(h_h_12e < 888 & h_h_13e < 888) %>%        # Assuming 888 means "NA"
  mutate(monthly_kerosene_use = h_h_12e*4.28)

# h_h_12e refers to liters/week used 
# h_h_13e refers to liters/month purchased
openwick_avg<- data.frame(sapply(openwick_kerosene[6:8], FUN = mean)) 


# Hurricane 37.785 liters/month

hurricane_kerosene <-
  kerosene_table[, c(6:9, 22:23)] %>% 
  filter(!is.na(f_f_hurricanelamp_4)) %>% 
  filter(!is.na(h_h_12e)) %>% 
  filter(h_h_12e < 888 & h_h_13e < 888) %>% 
  mutate(monthly_kerosene_use = h_h_12e*4.28)

hurricane_avg<- data.frame(sapply(hurricane_kerosene[5:7], FUN = mean)) 

question_labs

# Pressurized: 10.70 liters/month

pressurized_kerosene <- 
  kerosene_table[, c(10:16, 22:23)] %>% 
  filter(!is.na(f_f_pressurizedlamp_4)) %>% 
  filter(!is.na(h_h_12e)) %>% 
  filter(h_h_12e < 888 & h_h_13e < 888) %>% 
  mutate(monthly_kerosene_use = h_h_12e*4.28)

pressurized_avg<- data.frame(sapply(pressurized_kerosene[8:10], FUN = mean))

# Other: 23.54 liters/month

other_lamps <-
  kerosene_table[, c(17:20, 22:23)] %>% 
  filter(!is.na(f_f_otherlamp_4)) %>% 
  filter(!is.na(h_h_12e)) %>% 
  filter(h_h_12e < 888 & h_h_13e < 888) %>% 
  mutate(monthly_kerosene_use = h_h_12e*4.28)

other_avg<- data.frame(sapply(other_lamps[5:7], FUN = mean))


# 33.69 liters/month for all kerosene lamps

total_kerosene <- (62.75 + 37.785 + 10.70 + 23.54) / 4

lamps <- c("openwick lamp", "hurricane lamp", "pressurized lamp", 
           "other lamp", "total average")

monthly_average <- c(62.75, 37.785, 10.70, 23.54, 33.69)

total_kerosene <- data.frame(lamps, monthly_average) 


# Take Kenya info









