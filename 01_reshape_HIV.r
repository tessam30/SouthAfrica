
library(tidyverse)
library(foreign)

setwd("~/SouthAfrica/GIS/Shapefile")

# Read in dbf portion of shapefile
  df <- read.dbf("HIV_Prevalence_2016.dbf")
  df = df %>% rename(`Y2016_1` = PrevQ12016, 
                     `Y2016_2` = PrevQ22016,
                     `Y2016_3` = PrevQ32016,
                     `Y2016_4` = PrevQ42016)
  write.csv(df, "HIV_Prevalence_2016_rename.csv")
  
  # Reshape the data long using the prevalence columns as the reshape point
  df = df %>% 
    select(OU:Facility, PrevQ12016:PrevQ42016) %>% 
    gather(PrevQ12016:PrevQ42016,
           key = time,
           value = incidence)

  # Create a group id time variable for each quarter
  i <- as.data.frame(group_indices(df, time))
  i = i %>% rename(time_id = `group_indices(df, time)`)
  df <- bind_cols(df, i)

# Save the data as a .dbf -- you have to change all the other shapefile names as well  
write.dbf(df, "HIV_Prevalence_2016_long.dbf")
write.csv(df, "HIV_Prevalence_2016_long.csv")

