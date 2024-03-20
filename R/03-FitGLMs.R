
### Bayesian GLMs to test hypotheses:
###   1 - Being in urban environment disrupts synchrony
###   2 - Lay date synchrony influences reproductive success
###   3 - Lay date synchrony affects reproductive success differently for
###       urban and rural birdss

library(tidyverse)
library(brms)
library(tidybayes)

# 1 - Load data ====
nest_yrs <- readRDS('data/urban_birds_data2.rds')

# Subsample data for smaller test model
test_dat <- nest_yrs %>% 
  group_by(Species, Year, Urban) %>%
  summarize(n())
  slice_sample(n = 20)

# Fit test model with uncorrelated random intercepts and slopes for species
test_mod <-  brm(a_LayDate_z ~ Urban + (Urban || Species),
                 data = test_dat,
                 family = lognormal(link = "identity"),
                 iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                 prior = c(prior(normal(0,1), class = b)),
                 control = list(adapt_delta = 0.99, max_treedepth = 20),
                 backend = 'cmdstanr')

# Species list
species_list <- unique(nest_yrs$Species)

### REPEAT FOR EACH SPECIES
for(i in species_list) {
  
  # Filter out focal species
  dat <- nest_yrs %>%
    filter(Species == i)
  
  # If too much data, group by year and environment and sample
  if(nrow(dat) > 5000) {
    
    set.seed(5)
    dat <- dat %>%
      group_by(Year, Urban) %>%
      slice_sample(n = 400)
    
  }
  
  # 2 - Causal model: urban vs. rural (total effect location on synchrony) ====
  
  nest_urban_mod <-  brm(a_LayDate_z ~ Urban + (Urban | NestID),
                         data = dat,
                         family = exponential(link = 'log'),
                         iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                         prior = c(prior(normal(0,1), class = b)),
                         control = list(adapt_delta = 0.99, max_treedepth = 20),
                         backend = 'cmdstanr')
  
  
  # 4 - Causal model: Total effect of synchrony/location on fledglings ====
  
  fledg_mod <-  brm(Fledglings_z ~ a_LayDate_z*Urban + (a_LayDate_z*Urban | NestID),
                    data = dat,
                    family = skew_normal(),
                    iter = 5000, warmup = 2500, chains = 4, cores = 4,
                    prior = c(prior(normal(0,1), class = b)),
                    control = list(adapt_delta = 0.99, max_treedepth = 20),
                    backend = 'cmdstanr')
  
  # 5 - Save models ====
  
  saveRDS(nest_urban_mod, paste0('output/models/', gsub(' ', '_', i), '_urban_mod.rds'))
  saveRDS(fledg_mod, paste0('output/models/', gsub(' ', '_', i), '_fledg_mod.rds'))
  
}



ord_fit_mean <- ordbetareg(formula = Fledglings_z ~ a_LayDate_z*Urban + 
                             (a_LayDate_z*Urban | NestID), 
                           data=dat,
                           control=list(adapt_delta=0.95),
                           cores=1,chains=1,iter=500,
                           refresh=0)
