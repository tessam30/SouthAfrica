library(tidyverse)
library(sf)
library(geocenter)
library(ggplot2)
library(stringr)
library(lubridate)

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
# # positive people
df_pos = df_sampled %>% 
  select(-contains('Test')) %>% 
  gather(qtr_year, pos, contains('Pos')) %>% 
  mutate(qtr_year = str_replace_all(qtr_year, 'Pos', ''))

# total sampled
df_tot = df_sampled %>% 
  select(-contains('Pos')) %>% 
  gather(qtr_year, tested, contains('Test')) %>% 
  mutate(qtr_year = str_replace_all(qtr_year, 'Test', ''))


# join together positive and sampled
df_long =  full_join(df_pos, df_tot, 
                     by = c("facility_id", "Facility", "SNU1", "SNU2", "Latitude", "Longitude", "bad_coords", "qtr_year")) %>% 
  mutate(
  # create new id
  id = row_number(),
  # calculate rate
  rate = pos/tested,
  rate_flag = rate > 1,
  quarter = as.numeric(str_sub(qtr_year, start = 2, end = 2)),
  year = as.numeric(str_sub(qtr_year, start = 3, end = 6)),
  # !!! FIX
  date = ymd(paste(year, quarter, '1', sep = '-')))



setwd('~/Documents/2017_Workshop_SouthAfrica/output data/')
st_write(df_sampled, 'RSA_Excel_Data_sampled.shp', delete_layer = TRUE)
