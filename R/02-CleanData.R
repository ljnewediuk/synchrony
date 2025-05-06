
# 02 - Clean the extracted NestWatch data

# This script calculates variation in nest laying dates (coefficients of
# variation in urban/rural surrounding areas) into a new data frame. Another
# dataset from the extracted data includes reproductive success (fledgling 
# numbers) for individual nests in each city. The cities are given individual
# Site IDs in both datasets so they can be joined later.

library(tidyverse)
library(sf)

# Load function to calculate coefficients of variation
source('functions/CleanNests.R')

# 1 - Load data ====

nest_list <- readRDS('output/nest_data.rds')

# Apply cleaning function to raw nest data to get CV of lay dates
lay_dates_list <- lapply(nest_list, function(x) cleanNests(x))

# Collapse into data frame and clear infinite values
lay_dates_df <- bind_rows(lay_dates_list) %>%
  filter(! is.infinite(CV))

# Save the cleaned data
saveRDS(lay_dates_df, 'output/laying_data.rds')

# 2 - Function for cleaning reproductive success data ====

getReproSuccess <- function(nest_i) {
  
  # Format data
  repro_i <- nest_i %>%
    st_drop_geometry() %>%
    mutate(Site.ID = paste(
      gsub("[-/&'()., ]+",'', City),
      gsub("[-/&'()., ]+",'', Species.Name),
      Year,
      sep = '_')) %>%
    select(Site.ID, Young.Fledged, Clutch.Size)
  
  # Return the data frame
  return(repro_i)
  
}

# Clean the reproductive success data (fledglings and clutch sizes) and collapse
# into data frame
fledge_df <- lapply(nest_list, function(x) getReproSuccess(x)) %>%
  bind_rows()

# Save the reproductive success data
saveRDS(fledge_df, 'output/fledgling_data.rds')
