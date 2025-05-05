
# 01 - Extract NestWatch data

# This script clips the raw NestWatch data to polygons of urban centres and 
# surrounding rural areas in the US. The result is a list of data frames with
# data points marked as 'rural' or 'urban'.

library(tidyverse)
library(sf)

# Load function to get nest distance metrics
source('functions/SubsetNests.R')

# 1 - Load data ====

# Define the root directory where data are stored
root_dir <- '../../../OneDrive - University of Manitoba/Documents/'

# ESRI USA census urban areas (encompassing at least 2,000 housing units or 
# having a population of at least 5,000)
urbandat <- st_read(paste0(root_dir, 'Spatial Data/USA Census Urban Areas/tl_2020_us_uac10/'))

# Nest data from Nestwatch
nestdat <- read.csv(paste0(root_dir, 'Other Data/NestWatch Data/nestwatch_download_aug3_2023.csv')) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(urbandat))

### FUNCTION 1 - SUBSET ALL NESTS WITHIN CITY AND 10-km BUFFER OUTSIDE CITY ----

# Vector of city names
cities <- unique(urbandat$NAME10)

# If list is already started, load it and make a list of completed cities
if(file.exists('temp/temp_nest_list.rds')) {
  # Completed nest data
  nest_list <- readRDS('temp/temp_nest_list.rds')
  # Make vector of completed cities
  cities_done <- names(nest_list)
  # Remove the completed cities from the list of cities
  cities <- cities[! cities %in% cities_done]
} else {
  nest_list <- list()
}

# Run function (this might be better as a loop so stopping points are possible)
for(i in length(nest_list)+1:length(cities)) {
  city <- cities[i]
  cat(i, city, '\n\n')
  city_nests <- subNests(city, urbandat, nestdat)
  nest_list[[i]] <- city_nests
  names(nest_list)[[i]] <- city
  # Save temporary list
  saveRDS(nest_list, 'temp/temp_nest_list.rds')
}

# Load temp file
nest_list <- readRDS('temp/temp_nest_list.rds')

# Remove nulls, if any (cities with no data)
nest_save <- Filter(function(x) nrow(x) > 0, nest_list)

# Save as final file
saveRDS(nest_save, 'output/nest_data.rds')
  