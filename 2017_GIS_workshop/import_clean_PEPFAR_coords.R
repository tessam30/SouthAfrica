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

specials = df %>% slice(to_keep) %>% mutate(coord_flag = 1)

df_sampled = df %>% group_by(SNU1) %>% sample_frac(0.25) %>% mutate(coord_flag = 0) %>% filter(Longitude != 0)

df_sampled = df_sampled %>% rbind(specials) %>% mutate(facility_id = row_number())

ggplot(df_sampled, aes(colour = SNU1)) + geom_sf()


# Remove excess cols
df_sampled = df_sampled %>% select(facility_id, Facility, SNU1, SNU2, 
                                   Latitude, Longitude, coord_flag, contains('Test'), contains('Pos'))

# convert back to a data.frame
st_geometry(df_sampled) = NULL

ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = SNU1)) + geom_point() + coord_equal() + theme_void()


# screw up 3 random points
manual_bad = sample(1:nrow(df_sampled), 3)

# label as being bad
df_sampled = df_sampled %>% mutate(coord_flag = ifelse(facility_id %in% manual_bad, 1, coord_flag))

# flip lat/long
new_lat = df_sampled[manual_bad[1], 'Longitude']
new_lon = df_sampled[manual_bad[1], 'Latitude']

df_sampled[manual_bad[1], 'Latitude'] = new_lat
df_sampled[manual_bad[1], 'Longitude'] = new_lon



# set long to 0

df_sampled[manual_bad[2], 'Longitude'] = 0
df_sampled[manual_bad[3], 'Longitude'] = 0

ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = SNU1)) + geom_point() + coord_equal() + theme_void()
ggplot(df_sampled, aes(x = Longitude, y = Latitude, colour = coord_flag)) + geom_point() + coord_equal() + theme_void()

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
                     by = c("facility_id", "Facility", "SNU1", "SNU2", "Latitude", "Longitude", "coord_flag", "qtr_year")) %>% 
  mutate(
  # create new id
  id = row_number(),
  # calculate rate
  rate = pos/tested,
  rate_flag = as.numeric(rate > 1),
  quarter = as.numeric(str_sub(qtr_year, start = 2, end = 2)),
  year = as.numeric(str_sub(qtr_year, start = 3, end = 6)),
  # !!! FIX
  date = ymd(paste(year, quarter, '1', sep = '-'))) %>% 
  select(-qtr_year)


# Test graphs (EDA) -------------------------------------------------------
ggplot(df_long, aes(x = rate)) + geom_histogram() + scale_x_continuous(labels = scales::percent)

# collapse for mapping, viz. ----------------------------------------------
snu2 = df_long %>% 
  # filter out bad values
  filter(rate_flag == 0, coord_flag == 0) %>% 
  group_by(SNU1, SNU2, date, year, quarter) %>% 
  summarise(pos = sum(pos),
            tested = sum(tested),
            num_fac = n()) %>% 
  mutate(rate = pos/tested)

snu1 = df_long %>% 
  # filter out bad values
  filter(rate_flag == 0, coord_flag == 0) %>% 
  group_by(SNU1, date, year, quarter) %>% 
  summarise(pos = sum(pos),
            tested = sum(tested),
            num_fac = n()) %>% 
  mutate(rate = pos/tested)


# test graphs (summary) -------------------------------------------------------------
ggplot(snu2, aes(x = quarter, y = rate)) + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~SNU2)

ggplot(snu1, aes(x = quarter, y = rate)) + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~SNU1)


ggplot(snu1 %>% filter(quarter == 1), aes(y = num_fac, x = SNU1, fill = SNU1)) + 
  coord_flip() +
  geom_bar(stat = 'identity')

# write datasets ----------------------------------------------------------

setwd('~/Documents/2017_Workshop_SouthAfrica/output data/')

write_csv(snu2, '')
