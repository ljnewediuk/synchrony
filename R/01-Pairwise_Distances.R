
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

# 2 - Extract distance data for purple martin 2009-2019 ====

nest_yrs <- data.frame()

for(yr_i in 2009:2019) {
  
  nest_yr <- nest_mets(nestdat = nestdat, 
                       spatdat = spatdat, 
                       yr = yr_i, 
                       maxdist = Inf,
                       species = 'Purple Martin') %>%
    mutate(spatVar = factor(spatVar, levels = c(1, 0), 
                            labels = c('urban', 'rural')))
  
  nest_yrs <- rbind(nest_yrs, nest_yr)
  
}

# 3 - Finish cleaning up data ====

# Remove NAs
nest_yrs <- na.omit(nest_yrs) %>%
  # Add log distance var
  mutate(log_xyDist_m = log(xyDist_m + 0.0001),
         # Factor year
         Year = factor(Year))

# 4 - Save for models ====

saveRDS(nest_yrs, 'data/purple_martin2009_2019.rds')
