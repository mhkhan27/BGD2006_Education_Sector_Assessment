rm(list=ls())

library(dplyr)
library(sf)
# library(st)
library(rgdal)
library(stringr)
source("scripts/fix_coordinates_function.R")

# read_data ---------------------------------------------------------------

edu_5_w_data <- read.csv("inputs/EduSector_5W_July-2020_V2.csv",skip = 3,na.strings = c(""," ","N/A"),stringsAsFactors = F) 
camp_boundary <- st_read("D:\\mh1\\REACH\\Common_shape_files/190310_outline_rohingya_refugee_camp_a1/190310_Outline_Rohingya_Refugee_Camp_A1.shp")


# education_data_clean ----------------------------------------------------

edu_5_w_data$Latitude<- edu_5_w_data$Latitude %>% str_replace_all("'","") %>% trimws()
edu_5_w_data$Longitude<- edu_5_w_data$Longitude %>% str_replace_all("'","") %>% trimws()

edu_5_w_data_remove_na_in_coordinate <- edu_5_w_data %>% filter(!is.na(Longitude)) %>%  filter(!is.na(Latitude))
edu_5_w_remove_na_and_zero <- edu_5_w_data_remove_na_in_coordinate %>% filter(Latitude!="0"|Longitude!="0")

edu_5_w_remove_na_and_zero$Latitude1 <- edu_5_w_remove_na_and_zero$Latitude %>% as.numeric()
edu_5_w_remove_na_and_zero$Longitude1<-edu_5_w_remove_na_and_zero$Longitude %>% as.numeric()


edu_5_w_fix_coordinate_format <-edu_5_w_remove_na_and_zero %>% mutate(
  Latitude = if_else(is.na(Latitude1),as.numeric(substr(edu_5_w_remove_na_and_zero$Latitude,1,2)) + 
                         as.numeric(substr(edu_5_w_remove_na_and_zero$Latitude,4,5))/60 + 
                         as.numeric(sub('.*\\.','',Latitude))/3600,as.numeric(edu_5_w_remove_na_and_zero$Latitude)),
  Longitude = if_else(is.na(Longitude1),as.numeric(substr(edu_5_w_remove_na_and_zero$Longitude,1,3)) + 
                       as.numeric(substr(edu_5_w_remove_na_and_zero$Longitude,5,6))/60 + 
                       as.numeric(sub('.*\\.','',Longitude))/3600,as.numeric(edu_5_w_remove_na_and_zero$Longitude))
) %>% select(-Latitude1,-Longitude1)

edu_5_w_fix_flip_coordinate<- fix.coordinates(edu_5_w_fix_coordinate_format)

edu_5_w_cleaned_data <- edu_5_w_fix_flip_coordinate  %>% mutate(
     difference = new_lon-new_lat
  ) %>% filter(!difference <1) %>% mutate(
    latitude = new_lat,
    longitude =new_lon
  ) %>% select(-new_lat,-new_lon) 

edu_5_w_cleaned_data %>% names()
edu_5_w_cleaned_data <- edu_5_w_cleaned_data %>% mutate(
  lat_long = paste0(latitude,"_",longitude)
)
edu_5_w_cleaned_data2<- edu_5_w_cleaned_data %>% dplyr::add_count(lat_long) %>% filter(n>1)


# spatial_join ------------------------------------------------------------
edu_5_w_cleaned_data_sf <-  st_as_sf(edu_5_w_cleaned_data, coords = c("longitude","latitude"), crs = "WGS84")
edu_join_with_camp <- st_join(edu_5_w_cleaned_data_sf,camp_boundary) %>% mutate(
  location = if_else(is.na(New_Camp_N),"outside_the_camp","inside_the_camp"),
)

edu_join_with_camp$Targeted.Population <- tolower(edu_join_with_camp$Targeted.Population)
# writeshp ----------------------------------------------------------------

# st_write(edu_join_with_camp,"outputs/edu_with_camp2.gpkg",overwrite=TRUE)
edu_join_with_camp$Facility.ID %>% unique() %>% length()

edu_join_with_camp %>% group_by(location,Targeted.Population) %>% summarise(
  n = n()
)

edu_5_w_data %>% nrow()
