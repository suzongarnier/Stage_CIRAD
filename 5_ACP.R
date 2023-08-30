# setwd("C:/Users/sgarnier/OneDrive - Cirad/Stage/R/analyse_data_Pretag")

rm(list=ls())

library(tidyverse)
library(Factoshiny)
load("Data/data.rdata")



#nombre d'application et de produit par fermier (sommer par application et moyenner pas cycle)

# caractiristiques plot/farmer
table_caracteristic = data %>% 
  select(id_farmer, district, plot_size, Total_size, nb_plot, nb_cycle, Experience,Age, Nb_household, Nb_active_labor) %>%  #province,
  unique() %>%
  mutate(plot_size = case_when(is.na(plot_size) ~ Total_size / nb_plot , 
                               !is.na(plot_size) ~ plot_size)) 
  

# # Extraction quantité par plot (fermier)
# table_qprod = data %>%
#   group_by(id_farmer, id_cycle) %>%
#   summarise(Sum_quantity_per_hectare = sum(Quantity_per_hectare))  %>%
#   group_by(id_farmer) %>%
#   summarise(Mean_quantity_per_cycle = mean(Sum_quantity_per_hectare))  %>%
#   ungroup()



# extraction Nombre produit par cycle
table_nprod = data %>% 
  group_by(id_farmer, cycl, id_application) %>%
  summarise(nb_prod_per_app = unique(How_many_product)) %>% 
  group_by(id_farmer, cycl) %>%
  summarise(nb_prod_per_cycle = mean(nb_prod_per_app)) %>%
  group_by(id_farmer) %>%
  summarise(n_prod_per_farm = mean(nb_prod_per_cycle)) %>%
  ungroup()

# extraction Nombre application par cycle
table_napplication<-data %>% 
  group_by(id_farmer, cycl) %>%
  summarise(nb_app = unique(n_per_cycle)) %>% 
  group_by(id_farmer) %>%
  summarise(n_app_per_farm = mean(nb_app)) %>%
  ungroup()

# extraction yeild par cycle
table_yield<-data %>%
  select(id_farmer, id_cycle, yield_hectar) %>%
  unique() %>%
  group_by(id_farmer) %>%
  summarise(yield_per_farm = mean(yield_hectar, na.rm = TRUE)) %>%
  ungroup()

# extraction quantité et nombre d'application fertiliseur par cycle
table_fert <-data %>%
  select(id_farmer, id_cycle, fertilizer_amount, n_apl_fert) %>%
  unique() %>%
  group_by(id_farmer) %>%
  summarise(fertilizer_amount_per_farm = mean(fertilizer_amount, na.rm = TRUE),
            n_apl_fert_per_farm = mean(n_apl_fert, na.rm = TRUE)) %>%
  ungroup()


# extraction kg seed par cycle
table_seed<-data %>%
  select(id_farmer, id_cycle, Kg_seed) %>%
  unique() %>%
  group_by(id_farmer) %>%
  summarise(Kg_seed_per_farmer = mean(Kg_seed, na.rm = TRUE)) %>%
  ungroup()

# merge
table_farm=table_nprod %>% 
  # full_join(table_qprod) %>% 
  full_join(table_napplication) %>% 
  full_join(table_yield) %>% 
  full_join(table_seed) %>% 
  full_join(table_fert)


table_fermier= table_caracteristic %>% 
  full_join(table_farm) %>%
  column_to_rownames("id_farmer")
view(table_fermier)


# ACP 
table_fermier$nb_cycle<-as.numeric(table_fermier$nb_cycle)
Factoshiny::Factoshiny(table_fermier)

#mean per district 

mean_plot_size <- aggregate(plot_size ~ district, data = table_fermier, FUN = mean)
mean_Total_size <- aggregate(Total_size ~ district, data = table_fermier, FUN = mean)
mean_nb_plot <- aggregate(nb_plot ~ district, data = table_fermier, FUN = mean)
mean_Experience <- aggregate(Experience ~ district, data = table_fermier, FUN = mean)
mean_Age <- aggregate(Age ~ district, data = table_fermier, FUN = mean)
mean_Nb_household <- aggregate(Nb_household ~ district, data = table_fermier, FUN = mean)
mean_Nb_active_labor <- aggregate(Nb_active_labor ~ district, data = table_fermier, FUN = mean)
mean_n_prod_per_farm <- aggregate(n_prod_per_farm ~ district, data = table_fermier, FUN = mean)
mean_n_app_per_farm <- aggregate(n_app_per_farm ~ district, data = table_fermier, FUN = mean)
mean_yield_per_farm <- aggregate(yield_per_farm ~ district, data = table_fermier, FUN = mean)
mean_Kg_seed_per_farmer <- aggregate(Kg_seed_per_farmer ~ district, data = table_fermier, FUN = mean)
mean_fertilizer_amount_per_farm <- aggregate(fertilizer_amount_per_farm ~ district, data = table_fermier, FUN = mean)
mean_n_apl_fert_per_farm <- aggregate(n_apl_fert_per_farm ~ district, data = table_fermier, FUN = mean)

mean_tables <- list(mean_plot_size, mean_Total_size, mean_nb_plot, mean_Experience, mean_Age, mean_Nb_household, mean_Nb_active_labor, mean_n_prod_per_farm, mean_n_app_per_farm, mean_yield_per_farm, mean_Kg_seed_per_farmer, mean_fertilizer_amount_per_farm, mean_n_apl_fert_per_farm)

final_table <- Reduce(function(x, y) merge(x, y, by = "district"), mean_tables)

#mean per class
Class <- c("3", "1", "1", "2", "1", "1", "1", "2", "2", "2", "2", "1", "2", "1", "1", "1", "1", "2", "2", "2", "1", "1", "1", "1", "1", "1", "3", "3", "1", "2", "2", "2", "2", "2", "2", "3", "3", "2", "2", "2", "2", "2", "2", "3", "2", "2", "3", "2", "3", "3", "1", "1", "1", "2", "1", "2")
id_farmer <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "29", "30", "T1", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27", "T28", "T29", "T3", "T4", "T5", "T6", "T7", "T8", "T9")
df <- data.frame(Class, id_farmer)
df

table_mean= table_caracteristic %>% 
  full_join(table_farm) %>%
  full_join(df) %>% 
  column_to_rownames("id_farmer")
view(table_mean)

mean_plot_size <- aggregate(plot_size ~ Class, data = table_mean, FUN = mean)
mean_Total_size <- aggregate(Total_size ~ Class, data = table_mean, FUN = mean)
mean_nb_plot <- aggregate(nb_plot ~ Class, data = table_mean, FUN = mean)
mean_Experience <- aggregate(Experience ~ Class, data = table_mean, FUN = mean)
mean_Age <- aggregate(Age ~ Class, data = table_mean, FUN = mean)
mean_Nb_household <- aggregate(Nb_household ~ Class, data = table_mean, FUN = mean)
mean_Nb_active_labor <- aggregate(Nb_active_labor ~ Class, data = table_mean, FUN = mean)
mean_n_prod_per_farm <- aggregate(n_prod_per_farm ~ Class, data = table_mean, FUN = mean)
mean_n_app_per_farm <- aggregate(n_app_per_farm ~ Class, data = table_mean, FUN = mean)
mean_yield_per_farm <- aggregate(yield_per_farm ~ Class, data = table_mean, FUN = mean)
mean_Kg_seed_per_farmer <- aggregate(Kg_seed_per_farmer ~ Class, data = table_mean, FUN = mean)
mean_fertilizer_amount_per_farm <- aggregate(fertilizer_amount_per_farm ~ Class, data = table_mean, FUN = mean)
mean_n_apl_fert_per_farm <- aggregate(n_apl_fert_per_farm ~ Class, data = table_mean, FUN = mean)
mean_n_cycle <- aggregate(as.numeric(nb_cycle) ~ Class, data = table_mean, FUN = mean)

mean_tables <- list(mean_n_cycle,mean_plot_size, mean_Total_size, mean_nb_plot, mean_Experience, mean_Age, mean_Nb_household, mean_Nb_active_labor, mean_n_prod_per_farm, mean_n_app_per_farm, mean_yield_per_farm, mean_Kg_seed_per_farmer, mean_fertilizer_amount_per_farm, mean_n_apl_fert_per_farm)

final_table <- Reduce(function(x, y) merge(x, y, by = "Class"), mean_tables)

