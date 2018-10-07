# Nicholas Bruns
# 10/7/2017
#linking my station points to the gps. Why? Stations were noted by watch time, here I link them to the Octopus GPS.
# for first pass? 
  #not interpolating. Rather, just snapping to nearest GPS point (so, rounding station time points to nearest 5 seconds)

library(tidyverse)
library(lubridate)
library(tmap)
tmap_mode("view")

# date_string <- "2018_09_19_06_31_55" #day 1
date_string <- "2018_09_19_18_47_25" #day 2
date_string <- "2018_09_20_23_14_21" #day 3

#####step 1: read and format station data
station_data <- read_csv(paste0("../data/station_raw_times_",date_string,".csv"))
#format date, and set sample volume as factor
station_data <- station_data %>%
  mutate(TIMESTAMP_raw=as_datetime( paste(date_stamp,time_only_stamp))) %>% # append date and time, and caste into a datetime type
  mutate(TIMESTAMP=round_date(TIMESTAMP_raw,"5 seconds"))%>%
  mutate(sample_volume=as.factor(sample_volume))
  

#######step 2, read in gps data, then link station timestamp to station gps
file_name_gps <- paste0("../data/CR1000_2_GPSdata_",date_string,".dat")

col_names <- read_csv(file_name_gps,skip = 1,n_max = 1) %>% names()
gps_table <- read_csv(file_name_gps,skip=4,col_names=col_names)

#convert to decimal degrees
gps_table <- gps_table %>% 
  mutate(lat_full=paste(latitude_a,latitude_b)) %>%
  mutate(lon_full=paste(longitude_a,longitude_b)) %>%
  mutate(lat_dec=as.numeric(conv_unit(lat_full,from="deg_dec_min",to="dec_deg"))) %>%
  mutate(lon_dec=as.numeric(conv_unit(lon_full,from="deg_dec_min",to="dec_deg"))) %>%
  mutate(altitude=as.numeric(altitude))

gps_table_sf <- gps_table %>%
  st_as_sf(coords=c("lon_dec","lat_dec"),na.fail=F,remove=F)


station_data_sf <- station_data %>%
  left_join(select(gps_table_sf,TIMESTAMP,lat_dec,lon_dec,altitude,geometry) ) %>%
  st_as_sf()



station_data_sf_500 <- station_data_sf %>% filter(sample_volume==500)
station_data_sf_125 <- station_data_sf %>% filter(sample_volume==125) 

#sanity check plot-- check both sizes for transcription erros
tm_shape(station_data_sf_500 ) + tm_dots(col="sample_volume") + tm_text("sample_id",just="left") +
tm_shape(station_data_sf_125) + tm_dots(col="sample_volume") + tm_text("sample_id",just="left")

#how to save--
st_write(station_data_sf,
         dsn=paste0("../data/station_linked_coordinates_",date_string,".gpkg"))

#
write_csv(station_data_sf,
         paste0("../data/station_linked_coordinates_",date_string,".csv"))


tm_shape(octo_data_sf) + tm_dots(col="red",size=.001) + 
tm_shape(station_data_sf_500 ) + tm_dots(col="sample_volume") + tm_text("sample_id",just="left") +
  tm_shape(station_data_sf_125) + tm_dots(col="sample_volume") + tm_text("sample_id",just="left")
