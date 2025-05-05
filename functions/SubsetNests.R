
# Function to subset the data for urban and rural nests

# Finds all nest points within the urban area and rural nests within a 10-km
# buffer beginning 5-km outside the city limits. Rural nests are excluded if
# they overlap with an urban area that is outside the city proper but still
# considered an urban area.

# Load libraries
library(tidyverse)
library(sf)

subNests <- function(city_name, urban_dat, nest_dat) {
  
  # Get polygon for city
  city <- urbandat %>%
    filter(NAME10 == city_name)
  
  # Buffer 5 km around the city, then 15, and take difference
  city_buff <- suppressWarnings(st_difference(
    st_buffer(city, dist = 15000), 
    st_buffer(city, dist = 5000)
  ))
  
  # Filter nests within the city
  nests_within_city <- nestdat %>% 
    st_filter(city) %>%
    # Add columns for location type, city, and lay date
    mutate(Lay.Date = yday(as.Date(First.Lay.Date)),
           Land.Use = 'Urban',
           City = city_name) %>%
    # Select necessary columns
    select(City, Year, Land.Use, Species.Name, Attempt.ID, Location.ID,
           Lay.Date, Young.Fledged, Clutch.Size)
  
  # Filter nests within buffer outside the city
  nests_outside_city <- nestdat %>%
    st_filter(city_buff) %>%
    filter(! Location.ID %in% unique(nests_within_city$Location.ID)) %>%
    mutate(Lay.Date = yday(as.Date(First.Lay.Date)),
           Land.Use = 'Rural',
           City = city_name) %>%
    # Select necessary columns
    select(City, Year, Land.Use, Species.Name, Attempt.ID, Location.ID,
           Lay.Date, Young.Fledged, Clutch.Size)
  
  # Filter out nests that are within the rural buffer, but still overlap with an
  # urbanized area
  nests_outside_city <- nests_outside_city[lengths(st_intersects(nests_outside_city, urbandat)) == 0,]
  
  # Combine the nests within and outside city as a single data frame and filter
  # any nests without a lay date
  nests_total <- bind_rows(nests_within_city, nests_outside_city) %>%
    filter(! is.na(Lay.Date))
  
  # Return nests
  return(nests_total)
  
}

