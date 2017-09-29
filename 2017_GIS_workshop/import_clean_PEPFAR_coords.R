library(tidyverse)
library(sf)
library(geocenter)
library(ggplot2)

# import old version of data which contains some errors -------------------

df = read_sf('~/Documents/2017_Workshop_SouthAfrica/input data/RSA_Excel_Data.shp')


# sample ~ 500 / year -----------------------------------------------------
set.seed(seed = 20170929)

# row numbers of facilities 
to_keep = c(1184, 933, 1920, 648, 1878, 1613, 325, 1900) + 1 # Arc Map 0-indexed

specials = df %>% slice(to_keep) %>% mutate(bad_coords = 1)

df_sampled = df %>% group_by(SNU1) %>% sample_frac(0.25) %>% mutate(bad_coords = 0) %>% filter(Longitude != 0)

df_sampled = df_sampled %>% rbind(specials) %>% mutate(facility_id = row_number())

ggplot(df_sampled, aes(colour = SNU1)) + geom_sf()


# Remove excess cols
df_sampled = df_sampled %>% select(facility_id, Facility, SNU1, SNU2, 
                                   Latitude, Longitude, bad_coords, contains('Test'), contains('Pos'))

# convert back to a data.frame
st_geometry(df_sampled) = NULL

ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = SNU1)) + geom_point() + coord_equal() + theme_void()


# screw up 3 random points
manual_bad = sample(1:nrow(df_sampled), 3)

# label as being bad
df_sampled = df_sampled %>% mutate(bad_coords = ifelse(facility_id %in% manual_bad, 1, bad_coords))

# flip lat/long
new_lat = df_sampled[manual_bad[1], 'Longitude']
new_lon = df_sampled[manual_bad[1], 'Latitude']

df_sampled[manual_bad[1], 'Latitude'] = new_lat
df_sampled[manual_bad[1], 'Longitude'] = new_lon



# set long to 0

df_sampled[manual_bad[2], 'Longitude'] = 0
df_sampled[manual_bad[3], 'Longitude'] = 0

ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = SNU1)) + geom_point() + coord_equal() + theme_void()
ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = bad_coords)) + geom_point() + coord_equal() + theme_void()

# reshape to long ---------------------------------------------------------
df_pos = df_sampled %>% 
  select(-contains('Test')) %>% 
  gather(qtr_year, pos, contains('Pos')) 

df_tot = df_sampled %>% 
  gather(qtr_year, pos, contains('Pos'))

# create new id
%>% mutate(id = row_number())


setwd('~/Documents/2017_Workshop_SouthAfrica/output data/')
st_write(df_sampled, 'RSA_Excel_Data_sampled.shp', delete_layer = TRUE)
