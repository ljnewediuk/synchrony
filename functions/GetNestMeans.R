
library(sf)
library(tidyverse)
library(stars)

nest_means <- function(nestdat, spatdat, species, yr, dist) {
  
  # Get lay dates in specific year and species
  nests <- nestdat %>% 
    arrange(Latitude) %>%
    filter(Year %in% yr & 
             Species.Name %in% species) %>%
    mutate(LayDate = as_date(First.Lay.Date)) %>%
    mutate(LayDate_j = yday(LayDate),
           Fledglings = Young.Fledged) %>%
    select(Location.ID, Year, Latitude, Longitude, LayDate_j, Fledglings) %>%
    na.omit(LayDate_j)
  
  # Make into sf
  nest_sf <- nests %>%
    st_as_sf(coords = c('Longitude', 'Latitude'),
             crs = 4326,
             remove = F)
  # Transform nesting data to projection of spatial data for extraction
  nest_sf_trans <- nest_sf %>%
    st_transform(crs = st_crs(spatdat))
  # Extract spatial data
  nestspat <- st_extract(spatdat, nest_sf_trans) %>%
    st_transform(crs = 4326) %>%
    st_drop_geometry()
  # Add spatial data to nest data
  nest_sf$spat <- nestspat[, 1]
  
  # Compute pairwise geo distances between each ID and other nests
  mean_diff_lay <- data.frame()
  
  for(i in unique(nest_sf$Location.ID)) {
    
    nest_id <- nest_sf %>%
      filter(Location.ID == i)
    
    nest_comp <- nest_sf %>%
      filter(! Location.ID == i)
    
    if(nrow(nest_comp) < 5) next
    
    xyDist <- as.numeric(st_distance(x = nest_id, y = nest_comp, by_element = F)[1,])
    
    lays_within_dist <- nest_comp %>%
      mutate(Distance = xyDist) %>%
      filter(! Distance > dist)
    
    if(nrow(lays_within_dist) < 5) next
    
    mean_diff <- data.frame(Species = species,
                            Year = yr,
                            Urban = factor(nest_id$spat),
                            NestID = nest_id$Location.ID,
                            Fledglings = nest_id$Fledglings,
                            Fledglings_mean = mean(nest_comp$Fledglings),
                            Fledglings_sd = sd(nest_comp$Fledglings),
                            LayDate_j = nest_id$LayDate_j,
                            LayDate_mean = mean(nest_comp$LayDate_j),
                            LayDate_sd = sd(nest_comp$LayDate_j)) %>%
      mutate(LayDate_z = (LayDate_j - LayDate_mean)/LayDate_sd,
             Fledglings_z = (Fledglings - Fledglings_mean)/Fledglings_sd) %>%
      select(! c(Fledglings_mean, Fledglings_sd, LayDate_mean, LayDate_sd))
    
    mean_diff_lay <- rbind(mean_diff_lay, mean_diff)
    
  }
  
 return(mean_diff_lay)
  
}
