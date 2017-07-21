# Calculate raster statistics for two types of interpolations -------------
# Goal: create a raster interpolation of HIV positivity rates across South Africa
# However, what's the best way to do that?
# 
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter, 21 July 2017
# 
# Standard way to calculate an interpolated raster in ArcMap is to interpolate
# the positivity rates across each facility using Empirical Bayesian Kriging.
# However, this treats each facility as being equal, when they can wildly differ 
# in terms of the number of people tested at each facility, biasing the raster 
# towards small facilities.
#
# To test if there's a better way, instead of kriging the positivity rate,
# I separately interpolated the number of people testing positive and the total number tested.
# Then calculated the ratio using raster division to get the rate at each pixel.


# ArcMap Processing summary -----------------------------------------------
# 1. Project data to AEA Albers Africa
# 2. Ran EBK with params here: https://docs.google.com/document/d/1HtItuVG3bbvAB0sRUoKTCeNzysaHGdoglSDfHuI7xUI/edit
    # a. Interpolating PosRat, the positivity rate for each facility
    # b. Interpolating HTSPos, the number of HIV+ patients at each facility
    # c. Interpolating HTSTst, the total number of people tested at each facility.
# 3. Calculated a rate for the new procedure by raster dividing the Pos/Tst interpolations    
# 4. Clipped interpolation to a 40 km buffer around facilities
# 5. For each clipped raster, divided into the SNU1 regions using Raster Split, to get individual rasters per SNU1


# What this script does ---------------------------------------------------
# 1. Loads in each raster piece
# 2. Calculates the average posivity rate for each SNU1
# 3. Compares to the SNU1 average, based on the facility-level data


# Get the real values -----------------------------------------------------
source('~/Documents/GitHub/SouthAfrica/02_reshape_HIVdata_v2.R')

snu1_avg = df_PEPFAR %>% 
  filter(quarter == 'Q2', year == 17) %>% 
  group_by(SNU1) %>% 
  summarise_(avg_snu1 = calc_pos_(pos_var, tested_var)) %>% 
  mutate(PR_NAME = str_replace_all(str_sub(start = 4, SNU1), " Province", ""))

# libraries ---------------------------------------------------------------

library(raster)
library(dplyr)
library(sf)


# Pull out the lookups for the Provinces ----------------------------------
snu1 = read_sf('shp/PR_SA_2011_proj.shp') %>% 
  bind_cols(data.frame(raster_num = 0:8))

# calc raster avg ---------------------------------------------------------
rasterStats = function (raster_base, raster_num) {
  
  # read in the raster info
  r = raster(paste0(raster_base, raster_num, '.TIF'))
  
  # plot the raster
  plot(r)
  title(raster_num)
  
  avg = cellStats(r, 'mean')
  min = cellStats(r, 'min')
  max = cellStats(r, 'max')
  std = cellStats(r, 'sd')
  
  df = data.frame(raster_num = raster_num, raster_type = raster_base, 
                    avg = avg, min = min, max = max, std = std)
  
  df %>% left_join(snu1, by = c('raster_num'))
}


# run over each of the SNU1 -----------------------------------------------

calcd = lapply(0:8, function(x) rasterStats('rasterdivision_Q22017/q217rstrdvclp', x)) %>% 
  bind_rows() %>% mutate(method  = 'new')

interp = lapply(0:8, function(x) rasterStats('rateinterp_Q22017/q217posrat', x)) %>% 
  bind_rows() %>% 
  mutate(method = 'std',
         # fix avgs to be in percent
         avg = avg/100)

raster_avg = bind_rows(calcd, interp)


# compare to calc’d avg ---------------------------------------------------

raster_avg = raster_avg %>% 
  left_join(snu1_avg, by = 'PR_NAME') %>% 
  filter(PR_NAME != 'Northern Cape') %>% 
  mutate(diff = avg - avg_snu1)


raster_avg %>% 
  dplyr::select(PR_NAME, method, diff) %>% 
  spread(method, diff)

ggplot(raster_avg, aes(x = avg_snu1, y = avg, colour = method)) + geom_point() + theme_minimal() + coord_equal() + geom_abline(slope = 1, intercept = 0)

ggplot(raster_avg, aes(x = avg, y = forcats::fct_reorder(PR_NAME, avg_snu1), colour = method)) +
  geom_point(aes(x = avg_snu1), size = 4, shape = 15, colour = 'black') +
  geom_point(size = 4) +
  theme_minimal() +
  ylab(' ') +
  scale_x_continuous(labels = scales::percent)
