
### Initial quick lmer models to check causal associations before final
### analysis

library(tidyverse)

# 1 - Load data ====
nest_yrs <- readRDS('data/purple_martin2009_2019.rds')

# Subset to 500 km radius, range where lay dates are positively autocorrelated
nest_yrs_corr <- nest_yrs %>%
  filter(xyDist_m < 500000)

# 2 - Causal model: urban vs. rural (total effect location on synchrony) ====

# Association paths are:
# Urban --> Distance
# Urban --> Lay date diff
# Distance --> Lay date diff

# Only need urban vs. rural to get total effect on nest diff, within 50 km where
# there shouldn't be a relationship between nest date differences and distance

# Model
nest_urban_lm <- lme4::lmer(nestDist_sc ~ spatVar + 
                              (1 | NestID/Year), data = nest_yrs_corr)

# extract coefficients
nest_urban_coefs <- data.frame(coef(summary(nest_urban_lm)))
# use normal distribution to approximate p-value
nest_urban_coefs$p.z <- 2 * (1 - pnorm(abs(nest_urban_coefs$t.value)))
# Result: Rural birds significantly more synchronous than urban birds
nest_urban_coefs

# Quick boxplots
ggplot(nest_yrs_corr, aes(x = spatVar, y = nestDist_d)) +
  geom_boxplot()

# 3 - Model: Intercept only (FOR EXTRACTING INDIVIDUAL RESIDS) ====

# Model
nest_lm_intercept <- lme4::lmer(nestDist_sc ~ 1 + 
                                  (1 | NestID/Year), data = nest_yrs_corr)

# Get random effects from intercept model for fledgling success model
nest_intercept_ranefs <- coef(nest_lm_intercept)$`Year:NestID` %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  # Get columns for nest ID and year
  mutate(NestID = paste0('N', str_extract(rowname, '(?<=N)(\\d+)')),
         Year = str_sub(rowname, 1, 4)) %>%
  # Add column for model slopes
  rename('Ranef' = `(Intercept)`) %>%
  select(NestID, Year, Ranef)

# Join random effects with fledgling data
fledg_yrs <- nest_yrs %>%
  select(NestID, Year, Fledg, Fledg_sc, spatVar) %>%
  distinct() %>%
  right_join(nest_intercept_ranefs) %>%
  mutate(Year = factor(Year))

# 5 - Causal model: distance (total effect distance on synchrony) ====

# As per DAG, need to condition on urban vs. rural because this affects distance
nest_dist_lm <- lme4::lmer(nestDist_sc ~ log_xyDist_m + spatVar + 
                             (log_xyDist_m | NestID/Year) + 
                             (log_xyDist_m | NestID/Year), data = nest_yrs)

# extract coefficients
nest_dist_coefs <- data.frame(coef(summary(nest_dist_lm)))
# use normal distribution to approximate p-value
nest_dist_coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

nest_dist_coefs

# Quick scatterplot
# First sample points for plotting
nest_yrs_samp <- nest_yrs %>% slice_sample(n = 10000, weight_by = nestDist_d)

ggplot() +
  geom_jitter(data = nest_yrs200, alpha = 0.2, width = 0.5,
              aes(x = log_xyDist_m, y = nestDist_d, 
                  colour = spatVar, fill = spatVar)) +
  geom_smooth(data = nest_yrs200, 
              aes(x = log_xyDist_m, y = nestDist_d, 
                  colour = spatVar, fill = spatVar),
              method = 'lm')

# 6 - Get random effects from distance model for fledgling success model ====

# Get random effects
nest_dist_ranefs <- coef(nest_dist_lm)$`Year:NestID` %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  # Get columns for nest ID and year
  mutate(NestID = paste0('N', str_extract(rowname, '(?<=N)(\\d+)')),
         Year = str_sub(rowname, 1, 4)) %>%
  # Add column for model slopes
  rename('syncSlope' = log_xyDist_m) %>%
  select(NestID, Year, syncSlope)

# Join random effects with data
fledg_yrs <- nest_yrs %>%
  select(NestID, Year, Fledg, Fledg_sc, spatVar) %>%
  distinct() %>%
  right_join(nest_dist_ranefs) %>%
  mutate(Year = factor(Year))

# 5 - Causal model: Total effect of synchrony/location on fledglings ====

# Association paths are:
# Urban --> Distance
# Urban --> Synchrony
# Distance --> Synchrony
# Year --> Synchrony
# Synchrony --> Fledglings
# Year --> Fledglings

# Need to condition on year
fledg_sync_lm <- lme4::lmer(Fledg_sc ~ Ranef*spatVar + Year + (1 | NestID), data = fledg_yrs)

# extract coefficients
fledg_sync_coefs <- data.frame(coef(summary(fledg_sync_lm)))
# use normal distribution to approximate p-value
fledg_sync_coefs$p.z <- 2 * (1 - pnorm(abs(fledg_sync_coefs$t.value)))

fledg_sync_coefs

# Quick scatterplot
ggplot(fledg_yrs, aes(x = Ranef, y = Fledg, 
                      colour = spatVar, fill = spatVar)) +
  geom_jitter(alpha = 0.4) +
  geom_smooth(method = 'lm') +
  ylim(0, 10)
