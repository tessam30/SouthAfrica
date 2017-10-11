
# Merge together SNU2 averages with their corresponding shapefile ---------


# setup -------------------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(stringr)

adm2_dir = '~/Documents/USAID/GISWkshp2017/'

# Import shapefile from gadm ----------------------------------------------
adm2 = read_sf(paste0(adm2_dir, 'ZAF_adm2.shp'))


# Import pre-pivoted SNU2 averages ----------------------------------------
# from source('import_clean_PEPFAR_coords.R')

snu2 = read_csv('~/Documents/USAID/GISWkshp2017/RSA_pepfarsample_SNU2.csv') %>% 
  # initially just took avg loc of the facilities for testing purposes.
  select(-lat, -lon) %>% 
  # create a cleaner version of SNU1, SNU2 name
  mutate(admin1 = str_replace(str_sub(SNU1, 4), ' Province', ''),
         admin2 = str_replace(str_sub(SNU2, 4), ' Municipality', ''),
         admin2 = str_replace(str_replace(admin2, ' Metropolitan', ''),
                              ' District', ''))


# Figure out what to merge ------------------------------------------------
snu2_unique = distinct(snu2, admin1, admin2) %>% mutate(rollup = 'rollup')

# test merge
test = left_join(snu2_unique, adm2, by = c('admin1' = "NAME_1", "admin2" = "NAME_2"))

test %>% filter(is.na(rollup) | is.na(NAME_0)) %>% select(admin1, admin2)
# Mismatches:
# admin1            admin2
# <chr>             <chr>
#   1 KwaZulu-Natal       Harry Gwala
# 2 KwaZulu-Natal     uMgungundlovu
# 3    North West Bojanala Platinum


# Fix merges --------------------------------------------------------------


# Manual fixes based on previous test merges
snu2 = snu2 %>% mutate(
  admin2 = ifelse(admin2 == 'uMgungundlovu', 'Umgungundlovu', admin2),
  # Based on https://en.wikipedia.org/wiki/KwaZulu-Natal
  admin2 = ifelse(admin2 == 'Harry Gwala' , 'Sisonke', admin2),
  admin2 = ifelse(admin2 == 'Bojanala Platinum', 'Bojanala', admin2)
)


# test merge
test2 = left_join(snu2, adm2, by = c('admin1' = "NAME_1", "admin2" = "NAME_2"))

test2 %>% filter(is.na(quarter) | is.na(NAME_0)) %>% select(admin1, admin2)


snu2_map = full_join(snu2, adm2, by = c('admin1' = "NAME_1", "admin2" = "NAME_2"))
# plot --------------------------------------------------------------------
# make sure it's an SF object for saving
snu2_map = st_as_sf(snu2_map)

centroids = snu2_map %>% st_centroid()

ggplot(centroids) +
  geom_sf(data = adm2, fill = llamar::grey10K, 
          size = 0.25,
          colour = llamar::grey40K) +
  geom_sf(aes(size = pos, colour = rate)) +
  scale_colour_gradientn(colours = llamar::RdPu) +
  facet_wrap(~quarter) +
  theme_void()


# Write shapefile ----------------------------------------------------------
sf::write_sf(snu2_map, 'RSA_pepfar_SNU2.shp')

