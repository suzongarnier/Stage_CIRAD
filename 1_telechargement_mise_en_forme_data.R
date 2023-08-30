
rm(list=ls())

## loading the necessary packages #############################################
#install.packages("tidyverse")
#install.packages("readxl")
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(googlesheets4)

##### data loading #############################################################

##TAKEO (out of kobo, in external excel file)

P_Takeo <- read_excel("Data/data_takeo.xlsx",sheet = "Pesticide")
GC_Takeo <- read_excel("Data/data_takeo.xlsx",sheet = "group_cyle")
GA_Takeo <- read_excel("Data/data_takeo.xlsx",sheet = "group_application")
GP_Takeo <- read_excel("Data/data_takeo.xlsx",sheet = "group_product")
 

## OTHER 

url_all <- "https://kobo.humanitarianresponse.info/api/v2/assets/a7XRdk75iDa2tPrtGsFqcg/export-settings/esG3pkoiRAFZmSmQFK8h8TS/data.xlsx"
 
download.file(url_all, destfile = "Data/data_all.xlsx", mode = "wb", overwrite = TRUE)  #download_file
# 
P_all <- read_excel("Data/data_all.xlsx",sheet = "PRETAG")
GC_all <- read_excel("Data/data_all.xlsx",sheet = "group_cyle")
GA_all <- read_excel("Data/data_all.xlsx",sheet = "group_application")
GP_all <- read_excel("Data/data_all.xlsx",sheet = "group_product")

## retraitement en vu de combiner les tables
P_all$`_index`<-as.character(P_all$`_index`)

GC_all$weeds<-as.character(GC_all$weeds)
GC_all$snails<-as.character(GC_all$snails)
GC_Takeo$yield<-as.numeric(GC_Takeo$yield)
GC_all$`_index`<-as.character(GC_all$`_index`)
GC_all$`_parent_index`<-as.character(GC_all$`_parent_index`)

GA_all$`_index`<-as.character(GA_all$`_index`)
GA_all$`_parent_index`<-as.character(GA_all$`_parent_index`)

GP_all$`_index`<-as.character(GP_all$`_index`)
GP_all$`_parent_index`<-as.character(GP_all$`_parent_index`)

#combinaison
P <- full_join(P_Takeo, P_all)
GC <- full_join(GC_Takeo, GC_all)
GA <- full_join(GA_Takeo, GA_all)
GP <- full_join(GP_Takeo, GP_all)


##### data formatting and layout ###############################################

##key formatting


P$id_farmer<-P$`_index`
GC$id_farmer<-GC$'_parent_index'
GC$id_cycle<-GC$`_index`
GA$id_cycle<-GA$'_parent_index'
GA$id_application<-GA$`_index`
GP$id_application<-GP$'_parent_index'
GP$id_produit<-GP$`_index`

##remove unnecessary columns

P<-subset(P, select = -`_index`)
GC<-subset(GC, select = -c(`_index`,`_parent_index`))
GA<-subset(GA, select = -c(`_index`,`_parent_index`))
GP<-subset(GP, select = -c(`_index`,`_parent_index`))


#ad which cycle
GC$cycl <- ave(GC$id_farmer, GC$id_farmer, FUN = seq_along)


## merge and reorganize column order
table_merged <- merge(merge(merge(P, GC, by = "id_farmer"), GA, by = "id_cycle"), GP, by = "id_application")


# relocate the "id" columns on the first position
data = table_merged  %>% dplyr::relocate(id_farmer, id_cycle, id_application, id_produit)


##remove unnecessary columns
data <- data %>% 
  select(-c(start, end, Informed_Consent, consent, Thanks, `_id`, `_uuid`, `_status` ))%>% 
  select(-contains(c("submi", "parent", "versions", "tags", "GPS"), ignore.case=FALSE)) %>%
  rename(c(gps_lat = xgps, gps_lon = ygps))


## conversion cost, income and yiels -->  per hectare 
data <- data %>% 
  mutate(Cost_service_provider = ifelse(!is.na(Cost_sp),
                                        ifelse(Cost_sp2 == "per_plot", Cost_sp /plot_size , Cost_sp),NA)) %>%
  mutate(Cost_service_provider_fp = ifelse(!is.na(Cost_sp_fp),
                                           ifelse(Cost_sp_fp2 == "per_plot", Cost_sp_fp /plot_size , Cost_sp_fp),NA)) %>%
  mutate(Cost_pesticide = ifelse(!is.na(Cost_of_pesticide),
                                 ifelse(Cost_of_pesticide2 == "per_plot", Cost_of_pesticide /plot_size , Cost_of_pesticide),NA)) %>%
  mutate(Cost_laborers = ifelse(!is.na(Cost_for_laborers),
                                ifelse(Cost_for_laborers2 == "per_plot", Cost_for_laborers /plot_size , Cost_for_laborers),NA)) %>%
  mutate(income_hectar = ifelse(!is.na(income),
                                ifelse(income2 == "per_plot", income /plot_size , income),NA)) %>%
  mutate(yield_hectar = ifelse(!is.na(yield),
                               ifelse(yield2 == "per_plot", yield /plot_size , yield),NA))

## yeild conversion per kilo (not per tone)
# if yield > 200, error in the data collection so /1000
data <- data %>%
  mutate(yield_hectar = case_when(yield_hectar >= 200 ~ yield_hectar / 1000 , 
                                  yield_hectar < 200 ~ yield_hectar))


## conversion quantity (liter, kilo, box) --> per hectare 
# we consider that 1 LITER of product is equivalent to 1 KG of product
# !!!!  we consider that 1 BOX of product is equivalent to 1 KG of product !!!!!
#plot(data$plot_size)

data <- data %>%
  rename(how_many_unit = how_many2) %>%
  mutate(Quantity_per_hectare = ifelse(!is.na(how_many),
                                       case_when(
                                         how_many_unit == "L/hec" ~ how_many,
                                         how_many_unit == "Kg/hec" ~ how_many,
                                         how_many_unit == "L/plot" ~ how_many * plot_size,
                                         how_many_unit == "Kg/plot" ~ how_many * plot_size,
                                         # how_many_unit == "Box/hec" ~ how_many,
                                         # how_many_unit == "Box/plot" ~ how_many * plot_size,
                                         TRUE ~ NA  ),
                                       NA))

data$fertilizer_amount <- data$fertilizer_amount/data$plot_size
#plot(data$fertilizer_amount)
# !!!!! There are some outlayer observations !!!!!!


# par(mfrow=c(2, 1))
# plot(data$Kg_seed)
# plot(data$plot_size)

data$Kg_seed <- data$Kg_seed/data$plot_size
# !!!!! There are some outlayer for small plot !!!!!!


## remove unnecessary columns
data <- subset(data, select = -c(Cost_sp, Cost_sp2, Cost_sp_fp, Cost_sp_fp2,Cost_of_pesticide, Cost_of_pesticide2,Cost_for_laborers, Cost_for_laborers2, 
                                 income, income2, yield, yield2,how_many, how_many_unit))

## damage severity conversion
data$insects = factor(data$insects)
data$diseases = factor(data$diseases)
data$weeds = factor(data$weeds)
data$rodents = factor(data$rodents)
data$snails = factor(data$snails)
data$Cow_buffalo = factor(data$Cow_buffalo)
data$Bird = factor(data$Bird)

levels(data$insects) = list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$diseases) = list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$weeds) = list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$rodents) = list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$snails) = list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$Cow_buffalo) =list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")
levels(data$Bird) =list("1"="1_No_damage", "2"="2", "3"="3_Moderate_damage", "4"="4", "5"="5_Serious_damage")

# var saison with Starting_date Harvest_date
data$saison_start<- ifelse(months(data$Starting_date) %in% c("mai", "juin", "juillet", "août", "septembre", "octobre", "novembre"), "rainy", "dry")
data$saison_harvest<- ifelse(months(data$Harvest_date) %in% c("mai", "juin", "juillet", "août", "septembre", "octobre", "novembre"), "rainy", "dry")

# ggplot(data, aes(x = Starting_date, y = id_farmer))+ #, color = saison_start)) +
#   geom_point() +
#   geom_point(aes(x = Harvest_date, y = id_farmer))+ #, color = saison_harvest))+
#   geom_segment(aes(x = Starting_date, y = id_farmer, xend =  Harvest_date, yend = id_farmer, color = cycl))


source("1_bis_retraitement_name_prod.R", local = TRUE)

#Code de retraitement des noms de produits 

#sort(unique(data$name_product2))

data<-data %>%
  select(-matches("/"))    ### attention ! fait le ici mais peut etre ailleurs a terme


data = data %>% group_by(id_farmer) %>% mutate(nb_cycle=factor(max(cycl)))



save(data, file="Data/data.Rdata")



