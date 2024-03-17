
### Bayesian GLMs to test hypotheses:
###   1 - Being in urban environment disrupts synchrony
###   2 - Lay date synchrony influences reproductive success
###   3 - Lay date synchrony affects reproductive success differently for
###       urban and rural birdss

library(tidyverse)
library(brms)
library(tidybayes)

# 1 - Load data ====
nest_yrs <- readRDS('data/purple_martin2009_2019.rds')

### REPEAT FOR EACH SPECIES

# 2 - Causal model: urban vs. rural (total effect location on synchrony) ====

nest_urban_mod <-  brm(a_LayDate_z ~ Urban + (Urban | NestID),
                       data = nest_yrs,
                       family = lognormal(link = "identity"),
                       iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                       prior = c(prior(normal(0,1), class = b)),
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       backend = 'cmdstanr')


# 4 - Causal model: Total effect of synchrony/location on fledglings ====

fledg_mod <-  brm(Fledglings_z ~ a_LayDate_z*Urban + (a_LayDate_z*Urban | NestID), 
                  data = nest_yrs, 
                  family = skew_normal(),
                  iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                  prior = c(prior(normal(0,1), class = b)),
                  control = list(adapt_delta = 0.99, max_treedepth = 20),
                  backend = 'cmdstanr')

# Quick scatterplot
ggplot(fledg_yrs, aes(x = a_LayDate_z, y = Fledglings_z, 
                      colour = Urban, fill = Urban)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = 'lm') 

# 5 - Save models ====

saveRDS(nest_urban_mod, 'output/nest_urban_mod.rds')
saveRDS(nest_intercept_mod, 'output/nest_intercept_mod.rds')
saveRDS(fledg_mod, 'output/fledg_mod.rds')
