# Reshape HIV Data from South Africa Mission -----------------------------------------
#
# 02_reshape_HIVdata_v2.R
#
# Script to ingest shapefiles and reshape into a long table
# 
# Data are from the Mashudo Mosamba, USAID South Africa
#
# Tim Essam, tessam@usaid.gov; Modified by Laura Hughes, lhughes@usaid.gov
#
# Copyright 2017 by Tim Essam via MIT License
#
# -------------------------------------------------------------------------


# Overview ----------------------------------------------------------------
# 1. Import prevalence data and swing to long format to isolate quarter/year data
# 2. Swing back to wide format to have variable value by each separate indicator, 
# e.g. HIV negative tests, HIV positive tests, positivity rate, etc.
# 3. ID weird vars, priority facilities, mobile clinics
# 4. Fix facilities w/ known geographic coordinate errors
# 5. Check that numbers make sense
# 6. Calculate avgs at the SNU1 and SN2 level
# 7. Export filtered and non-filtered data for use in ArcMap.


# setup -------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(foreign)
library(stringr)
library(readxl)
library(data.table)
library(RColorBrewer)
library(forcats)
library(knitr)
library(lubridate)

# where data is located
# where csvs are located
base_dir = "~/Documents/GitHub/SouthAfrica/"
# where prevalence data is located
geo_dir = "~/Documents/GitHub/SouthAfrica/shp/"

# start month for each quarter's data reporting
# Q12017 is actually 10-10-2016 to 12-31-2016, for instance. 
# Based off of fiscal year, not calendar; need data frame to map the values to
quarter2months = data.frame(quarter = c('Q1', 'Q2', 'Q3', 'Q4'), month = c(10, 1, 4, 7))

# name of the variable that encodes 'prevalence'.  In past, has been "Prev" or "PosRat"
prev_var = "PosRat"
pos_var = "HTSPos" # number tested positive
neg_var = "HTSNeg" # number tested negative
tested_var = "HTSTst" # total number tested

# function to calculate positivity rate
calc_pos = function(pos_var, tested_var) {
  sum(pos_var) / sum(tested_var)
}

# calculate positivity within a mutate_ or summarise_ function which uses a stringed variable name
# e.g. df %>% mutate_(calc_pos_('pos', 'tested'))
calc_pos_ = function (pos_var, tested_var) {
  paste0('calc_pos(', pos_var, ',', tested_var, ')')
}

# import data -------------------------------------------------------------


# 2016 and 2017 data merged together, unlike in previous cuts of data
df <- read.dbf(paste0(geo_dir, "HTS_2016_2017.dbf"))
df_latlon <- read.csv(paste0(base_dir, "Facility_corrected_latlon.csv")) %>% 
  na.omit()

# Data frame to filter out excess clinics that are not priority ones
df_muni <- read_excel(paste0(base_dir, "Priority_districts_filter.xlsx")) %>% 
  rename(SNU2 = `Organisation unit`)


# Insert a stub before the first Q (indicating to fiscal quarter) allow splitting in reshape
colnames(df) <- gsub('Q', '_Q', colnames(df))

# Reshaping to get a long data frame for ArcMap Geodatabase table
df_PEPFAR = df %>% 
  gather(key = indicator, value = value, -OU, -contains('SNU'), -contains('Facility'), -Latitude, -Longitude) %>% 
  # split indicator and quarter/year data
  separate(indicator, into = c('indicator', 'quarter_year'), sep = '_') %>% 
  # split time into quarter/year (after 2nd digit)
  # Create two new variables to capture time dimension (FY Quarter and Year)
  separate(quarter_year, into = c('quarter', 'year'), sep = 2) %>% 
  
  mutate(
    # convert quarter to month
    month = plyr::mapvalues(quarter, from = quarter2months$quarter, to = quarter2months$month),
    
    # make sure month, year are numbers
    month = as.numeric(month),
    year = as.numeric(year),
    
    # create time var = PosiX object of year - month - 01
    # NOTE: if quarter == Q1, shift the year to accomodate fiscal year
    timeVar = ifelse(quarter == 'Q1', (paste(year - 1, month, '01', sep = '-')),
                     (paste(year, month, '01', sep = '-'))),
    timeVar = ymd(timeVar)) %>% 
  
  # spread indicator wide now that quarter/year has been isolated
  spread(indicator, value) %>% 
  
  # flag wonky vars
  mutate_(prevFlag = paste0('ifelse(', prev_var, ' > 100 | ', prev_var, '< 0, 1, 0)'),
          testedFlag = paste0('ifelse(', pos_var, '+', neg_var, '!=', tested_var, ', 1, 0)')) %>% 
  mutate(
    priorityFlag = ifelse(SNU2 %in% df_muni$SNU2, 1, 0),
    mobileFac = ifelse(Facility %like% "Mobile", 1, 0), 
    locationFlag = ifelse(Longitude == 0, 1, 0)) %>% 
  
  # calculate one quarter lag
  group_by(Facility) %>% 
  arrange(timeVar) %>% 
  mutate_(prevQ_pos = paste0('lag(', prev_var, ')'),
          prevQ_diff = paste0(prev_var, '- prevQ_pos'),
          prevQ_chg = 'prevQ_diff / prevQ_pos')
    
  

# Fix facility locations --------------------------------------------------


# Fix facility locations that have improper latitude and longitude coordinates
df_latlon_sub <- select(df_latlon, Facility, Latitude, Longitude) %>%
  rename(Facility_y = Facility,
         Latitude_y = Latitude,
         Longitude_y = Longitude)

df_PEPFAR = df_PEPFAR %>%
  left_join(df_latlon_sub, c("Facility" = "Facility_y")) %>%
  mutate(Latitude = ifelse(is.na(Latitude_y), Latitude, Latitude_y),
         Longitude = ifelse(is.na(Longitude_y), Longitude, Longitude_y)) 


# checks ------------------------------------------------------------------

# Verify that the priority facility flag is accurate
df_PEPFAR %>% 
  filter(priorityFlag == 1) %>%
  group_by(SNU1, SNU2) %>% 
  summarise(n = n())

# Facility roster --> assuming that the Facility name is distinct
df_PEPFAR %>% group_by(timeVar) %>% 
  count(Facility) %>% 
  filter(n >1)

facilities = df_PEPFAR %>% 
  filter(prevFlag == 0, priorityFlag == 1) %>% 
  distinct(Facility, SNU1, Latitude, Longitude)

# Check that values make sense
summary(df_PEPFAR)

# Tally those records that are over 100
df_PEPFAR %>% 
  filter(prevFlag == 1) %>% 
  select_('Facility', 'SNU1', 'SNU2', 'timeVar', prev_var) 

# subset only the valid observations

df_subset = df_PEPFAR %>%
  filter(prevFlag == 0, priorityFlag == 1, locationFlag == 0, mobileFac == 0, testedFlag == 0)

# PLOTS -------------------------------------------------------------------

# Look at SNU2 averages by quarter over time
df_snu2 = df_subset %>% 
  group_by(SNU1, timeVar) %>% 
  mutate_(tot_SNU1_qtr = calc_pos_(pos_var, tested_var), 
         facCountSNU1 = 'n()') %>% 
  ungroup() %>% 
  mutate(sortVarSNU1 = fct_reorder(SNU1, -tot_SNU1_qtr)) %>% 
  group_by(SNU2) %>% 
  mutate_(tot_tot_SNU2 = calc_pos_(pos_var, tested_var)) %>%
  ungroup() %>% 
  mutate(sortVarSNU2 = fct_reorder(SNU2, -tot_tot_SNU2)) %>% 
  group_by(SNU2, timeVar) %>% 
  mutate_(tot_SNU2_qtr = calc_pos_(pos_var, tested_var),
         facCountSNU2 = 'n()')


# Check how the number of facilities varies aross time
df_snu2 %>% 
  group_by(SNU1) %>% 
  mutate(facCount = n()) %>% 
  ungroup() %>% 
  group_by(SNU1, timeVar, facCount) %>% 
  summarize_(n = 'n()', totsize = paste0('sum(', tested_var,')')) %>% 
  ungroup() %>% 
  mutate(sortOrd = fct_reorder(factor(SNU1), -facCount)) %>% 
  ggplot(., aes(x = timeVar, y = n, colour = sortOrd)) +
  geom_line() +
  geom_point(aes(size = totsize)) +
  facet_wrap(~sortOrd, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none")


# Show differences from SNU1 average prevalence by SNU2, sort the values as well  
ggplot(df_snu2, aes(x = timeVar, y = tot_SNU2_qtr, colour = factor(SNU1))) +
  geom_line() + scale_color_brewer(palette = "Set2")+
  geom_line(aes(x = timeVar, y = tot_SNU1_qtr), colour = "gray", size = 1.25, alpha = 0.5) +
  geom_point() +
  facet_wrap(sortVarSNU1 ~ sortVarSNU2, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 7, hjust = 0),
        axis.text.x = element_text(size = 6)) +
  labs(title = "Gauteng Province has some of the highest HIV Prevalence rates across PEPFAR locations", 
       subtitle = "Gray line indicates province average by quarter", 
       y = "", x = "") +
  scale_y_continuous(labels = scales::percent)

ggsave('SA_SNU2avgs_byquarter.pdf', width = 15, height = 8)

# District (SNU1) average along w/ fitted lines

ggplot(df_snu2, aes(x = timeVar, y = tot_SNU1_qtr, colour = factor(SNU1))) +
  scale_color_brewer(palette = "Set2") +
  geom_line(size = 1.25, alpha = 0.5) +
  geom_smooth(method='lm',formula=y~x, linetype = 2, size = 0.75) +
  geom_point() + 
  theme_minimal() +
  facet_wrap(~sortVarSNU1, nrow = 2) +
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 7, hjust = 0),
        axis.text.x = element_text(size = 6)) +
  labs(title = "Gauteng Province has seen little decrease in positivity, while Mpumalanga and KZN have decreased", 
       subtitle = "Dotted line indicates linear fitted line to the trend", 
       y = "", x = "") +
  scale_y_continuous(labels = scales::percent)

ggsave('SA_SNU1avgs_byquarter.pdf', width = 9, height = 6)

# Is there still a drastic drop in prevalence rates?
# Plotting mean positivity at SNU1 and SNU2
df_PEPFAR %>%
  filter(priorityFlag == 1, prevFlag == 0, mobileFac == 0) %>%
  group_by(SNU1) %>%
  mutate_(meanSNU1 = calc_pos_(pos_var, tested_var)) %>%
  ungroup() %>%
  mutate(SNU1ord = fct_reorder(SNU1, -meanSNU1)) %>%
  group_by(timeVar, SNU2, SNU1, SNU3, SNU1ord) %>%
  summarise_(mean = paste0('mean(', prev_var, ')'),
            n = 'n()')%>%
  ggplot(., aes(x = timeVar, y = mean, colour = SNU3)) +
  geom_line(alpha = 0.25)+
  geom_smooth(colour = "gray70", alpha = 0.25, size = 1) +
  facet_wrap(~SNU1ord, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "HIV Test counts averaged by SNU3",
       ylab = "HIV Test Counts", x = "")


# Create a slippy map of the facilities
# Question: What to do about the mobile facilities?
library(htmlwidgets)
library(htmltools)
library(leaflet)
facilities %>% 
  filter(Longitude!= 0) %>%  
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~Longitude, ~Latitude, 
             clusterOptions = markerClusterOptions(), 
             popup = ~htmlEscape(Facility)) %>%
  addMiniMap(
    tiles = providers$CartoDB.Positron,
    toggleDisplay = TRUE) 


# check outliers in maps --------------------------------------------------
pal = colorFactor('Pastel1', domain = unique(df_PEPFAR$SNU1))

facilities %>% 
  filter(Longitude!= 0) %>%  
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~Longitude, ~Latitude, 
                   fillColor = ~pal(SNU1),
                   stroke = FALSE, radius = 4, fillOpacity = 0.8,
             popup = ~htmlEscape(SNU1)) %>%
  addMiniMap(
    tiles = providers$CartoDB.Positron,
    toggleDisplay = TRUE) 


# Export ------------------------------------------------------------------

# Save for importing into Geodatabase table
write.csv(df_PEPFAR, "HIV_Prevalence_long_2017-07-18.csv")


write.csv(df_subset, "HIV_Prevalence_long_filtered_2017-07-18.csv")

# Compare the numbers for each cut of data to see how many facilities are filitered out
table(df_subset$SNU1, df_subset$timeVar)
table(df_PEPFAR$SNU1, df_PEPFAR$timeVar)


# outliers ----------------------------------------------------------------

write.csv(df_PEPFAR %>% filter(prevFlag == 1 | testedFlag == 1) %>% 
            select_('Facility', 'SNU1', 'SNU2', 'SNU3', 'quarter', 'year', 'timeVar', 
                   pos_var, neg_var, tested_var, prev_var), 
          "HIV_Prevalence_outliers_2017-07-18.csv")

