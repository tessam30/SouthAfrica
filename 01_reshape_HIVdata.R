# Reshape HIV Data from South Africa Mission -----------------------------------------
#
# 01_reshape_HIVdata.R
#
# Script to ingest shapefiles and reshape into a long table
# 
# Data are from the Mashudo Mosamba, USAID South Africa
#
# Tim Essam, tessam@usaid.gov 
#
# Copyright 2017 by Tim Essam via MIT License
#
# -------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(foreign)
library(stringi)
library(readxl)
library(data.table)
library(RColorBrewer)
library(forcats)
library(knitr)

setwd("~/SouthAfrica/GIS/Shapefile")

df_fc16 <- read.dbf("Facility_Clinical_Cascade_2016.dbf")
df_fc17 <- read.dbf("Facility_Clinical_Cascade_2017.dbf")
df_latlon <- read.csv("Facility_corrected_latlon.csv") %>% 
  na.omit()

# Data frame to filter out excess clinics that are not priority ones
df_muni <- read_excel("Priority_districts_filter.xlsx") %>% 
  rename(SNU2 = `Organisation unit`)

# Check for differences in columns between dataframes
names(df_fc16)
names(df_fc17)
# names(df[,order(colnames(df))])


#Rename to make reshaping easier
df1 = df_fc16 %>%  
  rename(TXCurQ22016 = TX_CurQ216,
         TXCurQ42016 = TX_CurQ416,
         TXNewQ12016 = TX_NewQ116,
         TXNewQ22016 = TX_NewQ216,
         TXNewQ32016 = TX_NewQ316,
         TXNewQ42016 = TX_NewQ416)

# Insert a stub before the first Q (indicating to fiscal quarter) allow splitting in reshape
colnames(df1) <- gsub('Q', '_Q', colnames(df1))

# Reshaping to get a long data frame for ArcMap Geodatabase table
df1_long = df1 %>% 
  gather(Test_Q12016:Link_Q42016, key = indicator, value = value) %>% 
  extract(indicator, into = c("indic", "time"), regex = "([[:alnum:]]+)_([[:alnum:]]+)") %>% 
  #na.omit() %>% 
  
  # Create two new variables to capture time dimension (FY Quarter and Year)
  mutate(year = stri_sub(time, -4, -1),
         quarter = stri_sub(time, 1, 2)) %>%
  spread(indic, value)


# --- FY 2017 Data --- 

# Similar routine on the fy2017 data, dropping redudancies from fY2016 data
df2 = df_fc17 %>% 
  rename( TXNewQ12017 = TX_NewQ117,
          TXNewQ22017 = TX_NewQ217,
          TXCurQ22017 = TX_CurQ217,
          PrevQ12017  = PrevQ12016,
          PrevQ22017  = PrevQ22016)

colnames(df2) <- gsub('Q', '_Q', colnames(df2))

# Reshape per above 
df2_long = df2 %>% 
  gather(Test_Q12017:Link_Q22017, key = indicator, value = value) %>% 
  extract(indicator, into = c("indic", "time"), regex = "([[:alnum:]]+)_([[:alnum:]]+)") %>%
  #na.omit() %>% 
  
  mutate(year = stri_sub(time, -4, -1), quarter = stri_sub(time, 1, 2)) %>% 
  spread(indic, value)

# Check the two dataframes for variable overlap
setdiff(names(df1_long), names(df2_long))
intersect(names(df1_long), names(df2_long))

# Join the two dataframes together in a row bind operation
df_PEPFAR = bind_rows(df1_long, df2_long)

df_PEPFAR = df_PEPFAR %>% 
  mutate(timeVar = ifelse(time == "Q12016", "10/01/2015", 
                          ifelse(time == "Q22016", "01/01/2016", 
                                 ifelse(time == "Q32016", "04/01/2016",
                                        ifelse(time == "Q42016", "07/01/2016",
                                               ifelse(time == "Q12017", "10/01/2016", "01/01/2017"))))),
         prevFlag = ifelse(Prev > 100, 1, 0),
         priorityFlag = ifelse(SNU2 %in% df_muni$SNU2, 1, 0),
         mobileFac = ifelse(Facility %like% "Mobile", 1, 0), 
         locationFlag = ifelse(Longitude == 0, 1, 0))

# Convert timeVar to a date,  type so ESRI reads it correctly
df_PEPFAR$timeVar <- as.Date(df_PEPFAR$timeVar, format="%m/%d/%Y")
  
  #Fix facility locations that have improper latitude and longitude coordinates
  df_latlon_sub <- select(df_latlon, Facility, Latitude, Longitude) %>%
    rename(Facility_y = Facility,
           Latitude_y = Latitude,
           Longitude_y = Longitude)

  df_PEPFAR = df_PEPFAR %>%
      left_join(df_latlon_sub, c("Facility" = "Facility_y")) %>%
      mutate(Latitude = ifelse(is.na(Latitude_y), Latitude, Latitude_y),
             Longitude = ifelse(is.na(Longitude_y), Longitude, Longitude_y)) 

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
    distinct(Facility, Latitude, Longitude)
  
  # Check that values make sense
  summary(df_PEPFAR)
  
  # Tally those records that are over 100
  df_PEPFAR %>% 
    filter(SNU1 == "wc Western Cape Province") %>% 
    group_by(prevFlag, timeVar, SNU2, SNU1) %>%
    summarise(mean = mean(Prev),
              #min = min(Prev),
              #max = max(Prev),
              count = n())
  
  
  # Look at SNU2 averages by quarter over time
  df_subset = df_PEPFAR %>% 
    filter(priorityFlag == 1, locationFlag ==0, prevFlag == 0, mobileFac == 0) %>% 
    group_by(SNU1, timeVar) %>% 
    mutate(prev_SNU1_qtr = sum(Pos) / sum(Test), 
           facCountSNU1 = n()) %>% 
    ungroup() %>% 
    mutate(sortVarSNU1 = fct_reorder(SNU1, -prev_SNU1_qtr)) %>% 
    group_by(SNU2) %>% 
    mutate(tot_prev_SNU2 = sum(Pos) / sum(Test)) %>%
    ungroup() %>% 
    mutate(sortVarSNU2 = fct_reorder(SNU2, -tot_prev_SNU2)) %>% 
    group_by(SNU2, timeVar) %>% 
    mutate(prev_SNU2_qtr = sum(Pos) / sum(Test),
           facCountSNU2 = n())
  
  # Check how the number of facilities varies aross time
  df_subset %>% 
    group_by(SNU1) %>% 
    mutate(facCount = n()) %>% 
    ungroup() %>% 
    group_by(SNU1, timeVar, facCount) %>% 
    summarize(n = n(), totsize = sum(Test)) %>% 
    ungroup() %>% 
    mutate(sortOrd = fct_reorder(factor(SNU1), -facCount))%>% 
    ggplot(., aes(x = timeVar, y = n, colour = sortOrd)) +
    geom_line() +
    geom_point(aes(size = totsize)) +
    facet_wrap(~sortOrd, nrow = 2) +
    theme_minimal() +
    theme(legend.position = "none")
    
  
  # Show differences from SNU1 average prevalence by SNU2, sort the values as well  
  ggplot(df_subset, aes(x = timeVar, y = prev_SNU2_qtr, colour = factor(SNU1))) +
      geom_line() + scale_color_brewer(palette = "Set2")+
      geom_line(aes(x = timeVar, y = prev_SNU1_qtr), colour = "gray", size = 1.25, alpha = 0.5) +
      geom_point(aes(fill = SNU1)) +
      facet_wrap(sortVarSNU1 ~ sortVarSNU2, nrow = 3) +
      theme_minimal() +
      theme(legend.position = "none", 
            strip.text.x = element_text(size = 7),
            axis.text.x = element_text(size = 6)) +
      labs(title = "Gauteng Province has some of the highest HIV Prevalence rates across PEPFAR locations", 
           subtitle = "Gray line indicates district average by quarter", 
           y = "", x = "") +
    scale_y_continuous(labels = scales::percent)
    
    # spread(timeVar, mean_qp) %>% 
    # select(-mean_tp, everything()) %>% 
    # arrange(-mean_tp) %>% 
    # kable(format.args = list(big.mark = ","), digits = 3)
 
  # What explains the drastic drop in prevalence rates?
  # df_PEPFAR %>% 
  #   filter(priorityFlag == 1, prevFlag == 0, mobileFac == 0) %>% 
  #   group_by(SNU1) %>% 
  #   mutate(meanSNU1 = sum(Pos)/sum(Test)) %>% 
  #   ungroup() %>% 
  #   mutate(SNU1ord = fct_reorder(SNU1, -meanSNU1)) %>% 
  #   group_by(timeVar, SNU2, SNU1, SNU3, SNU1ord) %>% 
  #   summarise(mean = mean(Prev),
  #             n = n())%>% 
  #   ggplot(., aes(x = timeVar, y = mean, colour = SNU3)) +
  #   geom_line(alpha = 0.25)+
  #   geom_smooth(colour = "gray70", alpha = 0.25, size = 1) +
  #   facet_wrap(~SNU1ord, ncol = 4) +
  #   theme_minimal() +
  #   theme(legend.position = "none") +
  #   labs(title = "HIV Test counts averaged by SNU3", 
  #        ylab = "HIV Test Counts", x = "")


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
  
  
  
# Save for importing into Geodatabase table
    write.csv(df_PEPFAR, "HIV_Prevalence_long.csv")
    
    # df_subset = df_PEPFAR %>% 
    #   filter(prevFlag == 0, priorityFlag == 1, locationFlag == 0, mobileFac == 0)
    write.csv(df_subset, "HIV_Prevalence_long_filtered.csv")
    
    # Compare the numbers for each cut of data to see how many facilities are filitered out
    table(df_subset$SNU1, df_subset$timeVar)
    table(df_PEPFAR$SNU1, df_PEPFAR$timeVar)



# 
# 
# # Read in dbf portion of shapefile
#   df <- read.dbf("HIV_Prevalence_2016.dbf")
#   df2 = df %>% rename(`Y2016_1` = PrevQ12016, 
#                       `Y2016_2` = PrevQ22016,
#                       `Y2016_3` = PrevQ32016,
#                       `Y2016_4` = PrevQ42016)
#   write.csv(df2, "HIV_Prevalence_2016_rename.csv")
# 
# # Reshape the data long using the prevalence columns as the reshape point
#   df_esri = df %>% rename(`10/01/2015` = PrevQ12016, 
#                           `01/01/2016` = PrevQ22016,
#                           `04/01/2016` = PrevQ32016,
#                           `07/01/2016` = PrevQ42016) %>% 
#     select(OU:Facility, `01/01/2016`:`04/01/2016`) %>% 
#     gather(`01/01/2016`:`04/01/2016`,
#            key = quarter,
#            value = incidence)
# 
#   df_esri$quarter <- as.Date(df_esri$quarter, format="%m/%d/%Y")
# 
# # Create a group id time variable for each quarter
#   i <- as.data.frame(group_indices(df_esri, quarter))
#   i = i %>% rename(time_id = `group_indices(df_esri, quarter)`)
#   df_esri <- bind_cols(df_esri, i)
# 
# # Save the data as a .dbf -- you have to change all the other shapefile names as well  
#   write.dbf(df_esri, "HIV_Prevalence_2016_long.dbf")
#   write.csv(df_esri, "HIV_Prevalence_2016_ts.csv")

