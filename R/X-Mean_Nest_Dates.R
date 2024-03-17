
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
foo <- na.omit(nest_diff_pm) %>%
  # Factor year
  mutate(Year = factor(Year),
         a_LayDate_z = abs(LayDate_z)) %>%
  # Remove extreme outliers (probably miscount - PM lay avg 1-6 eggs; some
  # fledged values are > 30)
  filter(! Fledglings > 10)

# pp_check - Maybe try tighter prior sd but looks pretty good
nest_urban_mod <-  brm(a_LayDate_z ~ Urban + (Urban | NestID),
                       data = foo,
                       family = lognormal(link = "identity"),
                       iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                       prior = c(prior(normal(0,1), class = b)),
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       backend = 'cmdstanr')

foo_sample <- foo %>%
  group_by(Urban) %>%
  slice_sample(n = 50)
  
fledg_mod2 <-  brm(Fledglings_z ~ a_LayDate_z*Urban + (a_LayDate_z*Urban | NestID), 
                  data = foo, 
                  family = skew_normal(),
                  iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                  prior = c(prior(normal(0,1), class = b)),
                  control = list(adapt_delta = 0.99, max_treedepth = 20),
                  backend = 'cmdstanr')


# Quick scatterplot
ggplot(foo, aes(x = a_LayDate_z, y = Fledglings_z, 
                      colour = Urban, fill = Urban)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = 'lm') 

ggplot(foo, aes(x = Urban, y = a_LayDate_diff)) + 
  geom_boxplot()

# 4 - Save for models ====

# saveRDS(nest_yrs_pm, 'data/purple_martin2009_2019.rds')
