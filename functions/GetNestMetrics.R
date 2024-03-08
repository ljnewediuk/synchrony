
library(tidyverse)
library(sf)
library(stars)

nest_mets <- function(nestdat, spatdat, yr, species, maxdist = NULL, predguard = F) {

  # Get lay dates in specific year and species
  
  # If using predator guard data, remove samples with no predator guard info
  # and factor by predator guard
  if(predguard == T) {
    
    nests <- nestdat %>% 
      arrange(Latitude) %>%
      filter(Year %in% yr & 
               Species.Name %in% species &
               ! Predator.Guard == '') %>%
      mutate(PredatorGuard = ifelse(Predator.Guard == 'no', 0, 1)) %>%
      mutate(LayDate = as_date(First.Lay.Date)) %>%
      mutate(LayDate_j = yday(LayDate),
             Fledglings = Young.Fledged) %>%
      select(Location.ID, Year, Latitude, Longitude, PredatorGuard, LayDate_j,
             Fledglings) %>%
      na.omit(LayDate_j)
    
  } else {
    
    nests <- nestdat %>% 
      arrange(Latitude) %>%
      filter(Year %in% yr & 
               Species.Name %in% species) %>%
      mutate(LayDate = as_date(First.Lay.Date)) %>%
      mutate(LayDate_j = yday(LayDate),
             Fledglings = Young.Fledged) %>%
      select(Location.ID, Year, Latitude, Longitude, LayDate_j, Fledglings) %>%
      na.omit(LayDate_j)
    
  }
  
  # Combine spatial data with nest data
  
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
  
  # Get pairwise distances
  
  # Dist of pairwise geo distances between points
  xyDist <- as.dist(st_distance(nest_sf, by_element = F))
  # Dist of pairwise diffs between nest dates
  nestDist <- dist(nest_sf$LayDate_j)
  # Spatial data of first point in pair
  spatVar <- as.dist(outer(nest_sf$spat, 
                             rep(0, times = length(nest_sf$spat)), "+"))
  # Whether first point in pair has a predator guard
  guardVar <- as.dist(outer(nest_sf$PredatorGuard, 
                              rep(0, times = length(nest_sf$PredatorGuard)), "+"))
  # Fledgling success
  fledgVar <- as.dist(outer(nest_sf$Fledglings, 
                            rep(0, times = length(nest_sf$Fledglings)), "+"))
  # Get nest ID
  # Add temp numeric ID variable
  nest_ids <- nest_sf %>%
    group_by(Location.ID) %>%
    mutate(ID = cur_group_id())
  # Get unique nest IDs for each first nest in comparison
  NestID <- as.dist(outer(nest_ids$ID, rep(0, times = length(nest_ids$ID)), '+'))
  
  # Make data frame
  nest_dat <- data.frame(NestID = paste0('N', NestID),
                         Year = yr,
                         Species = species,
                         xyDist_m = as.numeric(xyDist),
                         nestDist_d = as.numeric(nestDist),
                         Fledg = as.numeric(fledgVar),
                         xyDist_sc = scale(as.numeric(xyDist)),
                         nestDist_sc = scale(as.numeric(nestDist)),
                         Fledg_sc = scale(as.numeric(fledgVar)),
                         spatVar = (as.numeric(spatVar)))
  
  # Add nest guard variable if predguard = T
  if(predguard == T) {
    
    nest_dat <- bind_cols(nest_dat, guardVar)
    
  }
  
  # If setting a maximum distance for xy (!is.null(xyDist)), filter rows larger
  # than that distance
  if(! is.null(maxdist)) {
    
    nest_dat <- nest_dat %>%
      filter(xyDist_m < maxdist)
    
  }
  
  return(nest_dat)
  
}


