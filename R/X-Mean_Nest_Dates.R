
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

# Purple Martin
nest_diff_pm <- data.frame()

for(yr_i in 2010:2019) {
  
  nest_pm <- nest_means(nestdat = nestdat,
             spatdat = spatdat,
             species = 'Purple Martin',
             yr = yr_i,
             dist = 500000)
  
  nest_diff_pm <- rbind(nest_diff_pm, nest_pm)
  
}

# 3 - Finish cleaning up data ====

# Remove NAs
nest_diff_pm_clean <- na.omit(nest_diff_pm) %>%
  # Factor year
  mutate(Year = factor(Year),
         a_LayDate_z = abs(LayDate_z)) %>%
  # Remove extreme outliers (probably miscount - PM lay avg 1-6 eggs; some
  # fledged values are > 30)
  filter(! Fledglings > 10)

# 4 - Save for models ====

saveRDS(nest_diff_pm_clean, 'data/purple_martin2009_2019.rds')
