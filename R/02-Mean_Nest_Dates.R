
library(tidyverse)

# Load function to get nest distance metrics
source('functions/GetNestMeans.R')

# 1 - Load data ====

# Nest data from Nestwatch
nestdat <- read.csv('input/nestwatch_download_aug3_2023.csv')

# Urban Extents from VIIRS and MODIS for the Continental U.S. Using Machine 
# Learning Methods, v1 (2015) from SEDAC (Liu et al. 2019 )
# https://doi.org/10.3390/rs11101247)
spatdat <- read_stars('input/urban.tif')

# 2 - Extract mean difference data for synchronous species ====

# Species list with distance and year attributes
species_list <- readRDS('output/data_attributes.rds')

# Initiate data frame
nest_diff_dat <- data.frame()

# Pull data for all elements in list
for(spp in 1:length(species_list)) { 
  
  # Pull new set of data for each year
  for(yr_i in 1:length(species_list[[spp]]$yrs)) {  
    
    nest_diff <- nest_means(nestdat = nestdat,
                            spatdat = spatdat,
                            species = species_list[[spp]]$name,
                            yr = species_list[[spp]]$yrs[yr_i],
                            dist = species_list[[spp]]$dist)
    
    # Bind together
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
  filter(! Fledglings > 13) %>%
  # Add species code before NestID to distinguish
  mutate(NestID = paste(gsub("[^A-Z]", "", Species), NestID, sep = '_'))

# 4 - Save for models ====

saveRDS(nest_diff_clean, 'data/urban_birds_data.rds')
