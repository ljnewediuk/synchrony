
### FUNCTION 2 - FOR EACH CITY-YEAR-SPECIES COMBO, CALCULATE CVS ----

nest_list <- readRDS('output/nest_data.rds')

source('functions/CleanNests.R')

####**** NEED TO TURN THIS INTO A FUNCTION AND MAKE SURE IT'S CALCULATING 
####**** CV BY INDIVIDUAL YEAR ****#####

# Apply cleaning function to raw nest data to get CV of lay dates
lay_dates_list <- lapply(nest_list, function(x) cleanNests(x))

# Collapse into data frame and clear infinite values
lay_dates_df <- bind_rows(lay_dates_list) %>%
  filter(! is.infinite(CV))

# Save the cleaned data
saveRDS(lay_dates_df, 'output/laying_data.rds')

#### FUNCTION 3 - EXTRACT FLEDGLINGS/CLUTCH SIZES BY INDIVIDUAL ----

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

# Save the data
saveRDS(fledge_df, 'output/fledgling_data.rds')