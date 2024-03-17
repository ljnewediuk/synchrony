
library(tidyverse)

# Load function to get nest distance metrics
source('functions/GetNestMeans.R')

# 1 - Load data ====

# Nest data from Nestwatch
nestdat <- read.csv('input/nestwatch_download_aug3_2023.csv')

# Urban Extents from VIIRS and MODIS for the Continental U.S. Using Machine 
# Learning Methods, v1 (2015) from SEDAC (Liu et al. 2019 
# https://doi.org/10.3390/rs11101247)
spatdat <- read_stars('input/urban.tif')

# 2 - Extract mean difference data for 2010-2019 ====

# Species list
species_list <- c('Purple Martin', 'Barn Swallow', 
                  'Black-capped Chickadee', 'European Starling')

nest_diff_dat <- data.frame()

for(yr_i in 2015:2019) {
  
  for(spp in species_list) { 
    
    nest_diff <- nest_means(nestdat = nestdat,
                          spatdat = spatdat,
                          species = spp,
                          yr = yr_i,
                          dist = 500000)
    
    nest_diff_dat <- rbind(nest_diff_dat, nest_diff)
    }
  
}

# 3 - Finish cleaning up data ====

# Remove NAs
nest_diff_clean <- na.omit(nest_diff_dat) %>%
  # Factor year
  mutate(Year = factor(Year),
         a_LayDate_z = abs(LayDate_z)) %>%
  # Remove extreme outliers (probably miscount/error - these species lay up to
  # ~ 7 eggs and 13 for black-capped chickadee; some fledged values are > 30)
  filter(! Fledglings > 13)

# 4 - Save for models ====

saveRDS(nest_diff_clean, 'data/urban_birds_2015_2019.rds')
