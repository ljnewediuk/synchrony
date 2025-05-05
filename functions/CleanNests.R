
# Function to clean the nest data

# Takes the list of all cities, finds the species-year combinations with ≥ 10
# nests, and then calculates the coefficient of variation for the nests. If
# there is an imbalance in the number of rural/urban nests for a species-year
# combination, the one with more nests is subsampled so the sample sizes are
# equal.

cleanNests <- function(nest_i) {
  
  # First, check if there are at least 10 nests both inside and outside the city
  if(nrow(nest_i[nest_i$Land.Use == 'Urban', ]) < 10 |
     nrow(nest_i[nest_i$Land.Use == 'Rural', ]) < 10) return(NULL)
  
  # Separate into rural and urban data
  rural_dat <- nest_i[nest_i$Land.Use == 'Rural', ]
  urban_dat <- nest_i[nest_i$Land.Use == 'Urban', ]
  
  # Make list of species common to both urban and rural sites
  spp_list <- intersect(unique(rural_dat$Species.Name), 
                        unique(urban_dat$Species.Name))
  
  # Make list of years
  yrs_list <- unique(nest_i$Year)
  
  # Make expanded dataframe of unique species and years
  spp_yrs <- expand.grid(spp = spp_list, yr = yrs_list)
  
  # NOTE: coefficients of variation are probably unstable for n < 10, so will
  # likely need to filter any cities/species/years where n < 10 for either the 
  # within- or outside-urban area. Sokal & Rohlf (1995) Biometry: The Principles 
  # and Practice of Statistics in Biological Research is likely a good ref. for 
  # this.
  
  # Calculate coefficient of variations with correction for small sample (Sokal & 
  # Rohlf 1995; Vangel 1996)
  # Determines spread around the mean: values closer to 1 indicate larger spread
  # around the mean, which means more inter-individual variation in laying dates.
  
  # Function for calculating adjusted coefficient of variation
  calc_cv_adj <- function(dat, col_name) {
    # Get column
    col <- dat %>% 
      st_drop_geometry() %>%
      select(all_of(col_name))
    # Calculate adjusted CV
    adj_cv <- (sd(pull(col), na.rm = T) / 
                 mean(pull(col), na.rm = T)) * (1 + 1 / (4 * nrow(na.omit(dat))))
    # Return
    return(adj_cv)
  }
  
  # Function to calculate CV for each species and year combination
  cv_by_spp_yr <- function(spp, yr) {
    
    # Subset the data for specific species and year
    rural_i <- rural_dat[rural_dat$Species.Name == spp & rural_dat$Year == yr, ]
    urban_i <- urban_dat[urban_dat$Species.Name == spp & urban_dat$Year == yr, ]
    
    # Next if either urban or rural nests for the species are < 10
    if(nrow(rural_i) < 10 | nrow(urban_i) < 10) {
      return(NULL)
    }
    
    # If more nests either inside or outside the city, sub-sample the nests so that
    # the same number of nests are sampled in and outside of the city
    
    # If more outside than inside
    if(nrow(urban_i) > nrow(rural_i)) {
      urban_i <- urban_i[sample(nrow(urban_i), nrow(rural_i)), ]
    }
    
    # If more inside than outside
    if(nrow(urban_i) < nrow(rural_i)) {
      rural_i <- rural_i[sample(nrow(rural_i), nrow(urban_i)), ]
    }
    
    # Calculate adjusted coefficients of variation
    cv_urban <- calc_cv_adj(urban_i, 'Lay.Date')
    cv_rural <- calc_cv_adj(rural_i, 'Lay.Date')
    
    # Make unique site ID for the year, species, city combination
    id <- paste(
      gsub("[-/&'()., ]+",'', unique(urban_i$City)),
      gsub("[-/&'()., ]+",'', spp),
      yr,
      sep = '_'
    )
    
    # Compile into table
    cv_table <- data.frame(Site.ID = id,
                           City = unique(urban_i$City),
                           Year = yr,
                           Species.Name = spp,
                           Land.Use = c('Rural', 'Urban'),
                           CV = c(cv_rural, cv_urban))
    
    # Return table
    return(cv_table)
    
  }
  
  # Apply functions to calculate coefficient of variation for each species and 
  # year
  city_list <- mapply(function(spp, yr) cv_by_spp_yr(spp = spp, yr = yr),
                      spp = spp_yrs$spp, yr = spp_yrs$yr)
  
  # Collapse into data frame
  city_data <- bind_rows(city_list)
  
  # Return the data frame
  return(city_list)
  
}
