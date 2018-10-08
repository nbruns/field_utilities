# Nicholas Bruns
# 9/16/2018
# 
# linking GPS and octo data

#todo: explicitly link to a projection, likely WGS84

# torun:
  # 1) copy filed to data dir
  # 2) set this date string:
# date_string <- "2018_09_16_12_01_17" #driving test of GPS
# date_string <- "2018_09_19_06_31_55" #day 1
# date_string <- "2018_09_19_18_47_25" #day 2
date_string <- "2018_09_20_23_14_21" #day 3

library(tidyverse)
library(sf)
library(tmap)
library(measurements)
library(viridis)

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
  st_as_sf(coords=c("lon_dec","lat_dec"),na.fail=F,remove=F)
  
tmap_mode("view")
tm_shape(gps_table_sf) + tm_dots(col="altitude")

##bring in data, left join with location by time stamp
octo_col_names <- read_csv(file_name_octo_data,skip = 1,n_max = 1) %>% names()
octo_data <- read_csv(file_name_octo_data,skip=4,col_names=octo_col_names)

octo_data_sf <- octo_data %>%
  left_join(gps_table_sf,by="TIMESTAMP") %>%
  st_as_sf()

#take a look at a fiew values
tm_shape(octo_data_sf) + tm_dots(col="RDO_mg",style="cont",palette="viridis",
  popup.vars=c("Temp_C","Chl","Turb","RDO","RDO_mg"))

tm_shape(octo_data_sf) + tm_dots(col="Temp_C",style="cont",palette="viridis")
tm_shape(octo_data_sf) + tm_dots(col="Turb")
tm_shape(octo_data_sf) + tm_dots(col="Chl")


POKE_AROUND_MORE <- FALSE
# to save an 
if(POKE_AROUND_MORE){
  diag_plot <- tm_shape(octo_data_sf) + tm_dots(col="Chl",style="cont",palette="viridis",
                                   popup.vars=c("Temp_C","Chl","Turb","RDO_mg")) + tm_basemap("Esri.WorldTopoMap")
  
  tmap_save(diag_plot,"test_gps_interactive_map.html")
  
  ggplot(octo_data) + geom_line(aes(x=TIMESTAMP,y=Chl))
  ggplot(octo_data) + geom_point(aes(x=TIMESTAMP,y=FDOM))
  ggplot(octo_data) + geom_point(aes(x=TIMESTAMP,y=RDO_mg))
  ggplot(octo_data) + geom_point(aes(x=TIMESTAMP,y=Temp_C))
  ggplot(octo_data) + geom_point(aes(x=TIMESTAMP,y=Turb))
  ggplot(octo_data_sf) + geom_line(aes(x=TIMESTAMP,y=RDO_mg))
  ggplot(octo_data_sf) + geom_point(aes(x=TIMESTAMP,y=RDO_mg))
  
  ggplot(octo_data) + geom_point(aes(x=FDOM,y=Chl))
  
  glimpse(octo_data_sf)
  ggplot(octo_data_sf) + geom_point(aes(x=lat_full,y=Chl,col=TIMESTAMP)) + scale_color_viridis()
  ggplot(octo_data_sf) + geom_point(aes(x=lat_full,y=Chl,col=TIMESTAMP)) + scale_color_viridis()
  
  
  ggplot(octo_data_sf) + geom_point(aes(x=Chl,y=Turb))
}