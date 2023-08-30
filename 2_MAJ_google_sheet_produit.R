
rm(list=ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(googlesheets4)

load("Data/data.Rdata")


###############################drive####################################

### pour ouvrir le Google sheet "product_pesticide"
sheet_url<-"https://docs.google.com/spreadsheets/d/1hjXlk5_ckY_LBcDL0mu2k2NKmm6s_-OJGu0d_5qQQQo/edit#gid=0"
Name_found<-read_sheet(sheet_url,sheet=1)   


## tout les product qu'on a pas encore identified, page 2 du drive, et pour les chercher  

unknown_prod <- data %>%
  group_by(name_product2) %>%
  filter(name_product2 %in% sort(setdiff(unique(data$name_product2), unique(Name_found$Name_r))))%>%
  summarise(kind_product = ifelse(any(kind_product == "Other"), NA, unique(na.omit(kind_product))))

write_sheet(unknown_prod, ss = sheet_url, sheet = "name_not_found")

## (pour eve) les noms comuns associer aux noms scientifiques et utilisations  page 3 du drive

Common_name <- data %>%
  group_by(Common_name, name_product2) %>%
summarise(kind_product = ifelse(any(kind_product == "Other"), NA, kind_product))%>%
  filter(!is.na(Common_name))

write_sheet(Common_name, ss = sheet_url, sheet = "Common_name")







