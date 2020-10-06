fix.coordinates<- function(data){
med_long<- median(data$Longitude,na.rm = T)
med_lat<- median(data$Latitude,na.rm = T)
mid_diff<- med_long-med_lat
x<- data %>% mutate(
  new_lat= if_else(Longitude<(med_long-(mid_diff-1)), data$Longitude, data$Latitude),
  new_lon= if_else(Latitude>(med_lat+(mid_diff-1)), data$Latitude, data$Longitude)
)
  new_data<- x %>% select(-Latitude,-Longitude)
  new_data<- new_data[!(is.na(new_data$new_lat) | new_data$new_lat=="")|!(is.na(new_data$new_lon) | new_data$new_lon=="") , ]

}



