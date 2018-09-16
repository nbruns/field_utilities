# Nicholas Bruns
# 9/16/2018
# 
# linking GPS and octo data

#todo: explicitly link to a projection, likely WGS84

# torun:
  # 1) copy filed to data dir
  # 2) set this date string:
date_string <- "2018_09_16_12_01_17"

library(tidyverse)
library(sf)
library(tmap)
library(measurements)

file_name_gps <- paste0("../data/CR1000_2_GPSdata_",date_string,".dat")
file_name_octo_data <- paste0("../data/CR1000_2_Data_8.28.2018_",date_string,".dat")

col_names <- read_csv(file_name_gps,skip = 1,n_max = 1) %>% names()
gps_table <- read_csv(file_name_gps,skip=4,col_names=col_names)
test_points <- gps_table[150:155,]
test_points

#convert to decimal degrees
gps_table <- gps_table %>% 
  mutate(lat_full=paste(latitude_a,latitude_b)) %>%
  mutate(lon_full=paste(longitude_a,longitude_b)) %>%
  mutate(lat_dec=as.numeric(conv_unit(lat_full,from="deg_dec_min",to="dec_deg"))) %>%
  mutate(lon_dec=as.numeric(conv_unit(lon_full,from="deg_dec_min",to="dec_deg"))) %>%
  mutate(altitude=as.numeric(altitude))

gps_table_sf <- gps_table %>%
  st_as_sf(coords=c("lon_dec","lat_dec"),na.fail=F)
  
tmap_mode("view")
tm_shape(gps_table_sf) + tm_dots(col="altitude")

##bring in data, left join with location by time stamp
octo_col_names <- read_csv(file_name_octo_data,skip = 1,n_max = 1) %>% names()
octo_data <- read_csv(file_name_octo_data,skip=4,col_names=octo_col_names)

octo_data_sf <- octo_data %>%
  left_join(gps_table_sf,by="TIMESTAMP") %>%
  st_as_sf()

#take a look at a fiew values
tm_shape(octo_data_sf) + tm_dots(col="Temp_C")
tm_shape(octo_data_sf) + tm_dots(col="Turb")
tm_shape(octo_data_sf) + tm_dots(col="Chl")

