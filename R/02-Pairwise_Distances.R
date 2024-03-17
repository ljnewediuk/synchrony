
### Calculate all pairwise distances and differences in nesting dates 
### for purple martin 2009-2019

library(tidyverse)

# Load function to get nest distance metrics
source('functions/GetNestMetrics.R')

# 1 - Load data ====

# Nest data from Nestwatch
nestdat <- read.csv('input/nestwatch_download_aug3_2023.csv')

# Urban Extents from VIIRS and MODIS for the Continental U.S. Using Machine 
# Learning Methods, v1 (2015) from SEDAC (Liu et al. 2019 
# https://doi.org/10.3390/rs11101247)
spatdat <- read_stars('input/urban.tif')

# 2 - Extract distance data for 2010-2019 ====

# Purple Martin
nest_yrs_pm <- data.frame()

for(yr_i in 2010:2019) {
  
  nest_yr_pm <- nest_mets(nestdat = nestdat, 
                          spatdat = spatdat, 
                          yr = yr_i, 
                          maxdist = Inf,
                          species = 'Purple Martin') %>%
    mutate(spatVar = factor(spatVar, levels = c(1, 0), 
                            labels = c('urban', 'rural')))
  
  nest_yrs_pm <- rbind(nest_yrs_pm, nest_yr_pm)
  
}

# 3 - Finish cleaning up data ====

# Remove NAs
nest_yrs_pm <- na.omit(nest_yrs_pm) %>%
  # Add log distance var
  mutate(log_xyDist_m = log(xyDist_m + 0.0001),
         # Factor year
         Year = factor(Year))

# 4 - Save for models ====

saveRDS(nest_yrs_pm, 'data/purple_martin2009_2019.rds')
