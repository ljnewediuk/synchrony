
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

# Subset to 500 km radius, range where lay dates are positively autocorrelated
nest_yrs_corr <- nest_yrs %>%
  filter(xyDist_m < 500000)

# Save for running on server
# saveRDS(nest_yrs_corr, 'data/purple_martin_500km.rds')

# ...and sample some urban and rural individuals to speed the model
# (runs extremely slowly with all points)

# Get nest IDs with at least 5 years of data
# nest_yrs_all <- nest_yrs %>% 
#   group_by(NestID, Year) %>%
#   summarize(N = n()) %>%
#   group_by(NestID) %>%
#   summarize(N_yrs = n()) %>%
#   filter(! N_yrs < 5) %>%
#   pull(NestID)
# # Sample urban and rural birds with at least 5 years of data
# urban_birds <- nest_yrs %>% 
#   filter(spatVar == 'urban' & NestID %in% nest_yrs_all) %>%
#   pull(NestID) %>%
#   unique() %>%
#   sample(15)
# rural_birds <- nest_yrs %>% 
#   filter(spatVar == 'rural' & NestID %in% nest_yrs_all) %>%
#   pull(NestID) %>%
#   unique() %>%
#   sample(15)
# # Filter out sample
# nest_yrs_sample <- nest_yrs %>%
#   filter(NestID %in% c(urban_birds, rural_birds)) %>%
#   group_by(NestID, Year) %>%
#   slice_sample(n = 20)

##***************####
## **** For lay date diff model (absolute difference from mean lay date within
## 500 km), use the following family:

# family = lognormal(link = "identity")
#***************####

# 2 - Causal model: urban vs. rural (total effect location on synchrony) ====

nest_urban_mod <-  brm(nestDist_d ~ spatVar + (1 | NestID/Year),
                       data = nest_yrs_corr,
                       family = negbinomial(link = 'log', link_shape = 'log'),
                       iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                       prior = c(prior(normal(0,1), class = b),
                                 prior(student_t(3,0,1), class = 'sd')),
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       backend = 'cmdstanr')

nest_urban_mod <-  brm(nestDist_sc ~ spatVar + (spatVar | NestID/Year),
                       data = nest_yrs_sample,
                       family = gaussian,
                       iter = 10000, warmup = 5000, chains = 4, cores = 4, 
                       prior = c(prior(normal(0,1), class = b),
                                 prior(student_t(3,0,1), class = 'sd'),
                                 prior(lkj(2), class = 'cor')),
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       backend = 'cmdstanr')

# 3 - Model: Intercept only (FOR EXTRACTING INDIVIDUAL RESIDS) ====

nest_intercept_mod <-  brm(nestDist_d ~ 1 + (1 | NestID/Year), 
                       data = nest_yrs_corr, 
                       family = negbinomial(link = 'log', link_shape = 'log'), 
                       iter = 10000, warmup = 5000, chains = 4, cores = 4, 
                       prior = prior(normal(0,1), class = 'Intercept'),
                       control = list(adapt_delta = 0.99, max_treedepth = 20),
                       backend = 'cmdstanr')

# Get random intercepts from model
nest_intercept_ranefs <- nest_intercept_mod %>%
  # Conditional draws
  spread_draws(b_Intercept, `r_NestID:Year`[Intercept]) %>%
  mutate(idIntercept = b_Intercept + `r_NestID:Year`) %>%
  median_qi(idIntercept) %>%
  # Rename NestID and year columns and select
  mutate(NestID = str_split(Intercept, '_', simplify = TRUE)[,1],
         Year = str_split(Intercept, '_', simplify = TRUE)[,2]) %>%
  select(NestID, Year, idIntercept, .lower, .upper)

# Join random effects with fledgling data
fledg_yrs <- nest_yrs %>%
  select(NestID, Year, Fledg, Fledg_sc, spatVar) %>%
  distinct() %>%
  right_join(nest_intercept_ranefs) %>%
  mutate(Year = factor(Year),
         idIntercept_sc = scale(idIntercept)[,1])

# 3 - Causal model: Asynchrony in urban and rural birds ====

# 4 - Causal model: Total effect of synchrony/location on fledglings ====

fledg_mod <-  brm(Fledg ~ idIntercept_sc*spatVar + Year + 
                    (idIntercept_sc*spatVar | NestID), 
                  data = fledg_yrs, 
                  family = poisson(link = 'log'),
                  iter = 5000, warmup = 2500, chains = 4, cores = 4, 
                  prior = c(prior(normal(0,1), class = b)),
                  control = list(adapt_delta = 0.99, max_treedepth = 20),
                  backend = 'cmdstanr')

# Quick scatterplot
ggplot(fledg_yrs, aes(x = idIntercept, y = Fledg, 
                      colour = spatVar, fill = spatVar)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(method = 'lm') 

# 5 - Save models ====

saveRDS(nest_urban_mod, 'output/nest_urban_mod.rds')
saveRDS(nest_intercept_mod, 'output/nest_intercept_mod.rds')
saveRDS(fledg_mod, 'output/fledg_mod.rds')
