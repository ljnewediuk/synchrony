# Statistical analysis

# Test if lay dates are more variable within vs. outside cities

# Response: Beta distribution CV lay dates (continuous between 0 and 1)
# Predictor: Category (urban, rural)
# Link function: Logit
# Random intercepts for:
#   Pair ID (city, species, year)
#   Species
#   Year

# Model:
# 
# model <- glmmTMB(
#   layDateCV ~ urban + (1 | pair_id) + (1 | species) + (1 | yr) + (1 | city),
#   family = beta_family(link = "logit"),
#   data = dat
# )

# Load the data
lay_data <- readRDS('output/laying_data.rds')

# Fit model (beta regression)
m <- glmmTMB::glmmTMB(
  CV ~ Land.Use + (Land.Use | City) + (Land.Use | Year) + (Land.Use | Species.Name),
  family = glmmTMB::beta_family(link = "logit"),
  data = lay_data
)

# Summarize
summary(m)

# Species coefficients
coef(m)$cond$Species.Name

# Test if variable lay dates reduce fledgling success

# Response: Negative binomial distribution fledgling success
# Predictor: CV lay dates
# Link function: Log
# Random intercepts for:
#   Pair ID (city, species, year)
#   Species
#   Year

# Load the reproductive success data and bind to laying data
fledg_data <- readRDS('output/fledgling_data.rds') %>%
  na.omit() %>%
  right_join(lay_data, relationship = 'many-to-many')

# Fit model (zero-inflated poisson)
m2 <- glmmTMB::glmmTMB(
  Young.Fledged ~ CV + (CV | City) + (CV | Year) + (CV | Species.Name),
  ziformula = ~1,
  family = poisson,
  data = fledg_data
)

# Predict by species
nd_m2_spp <- expand_grid(
  CV = seq(from = 0, to = 0.5, by = 0.01),
  City = NA, 
  Year = NA, 
  Species.Name = levels(m2$frame$Species.Name)
)

pred_m2_spp <- predict(m2, nd_m2_spp, type = 'response')

# Make data frame
pred_m2_df_spp <- nd_m2_spp %>% 
  mutate(Fledglings = pred_m2_spp)

# Predict main effects
nd_m2_fixef <- expand_grid(
  CV = seq(from = 0, to = 0.5, by = 0.01),
  City = NA, 
  Year = NA, 
  Species.Name = NA
)

pred_m2_fixef <- predict(m2, nd_m2_fixef, se.fit = T, type = 'response')

# Make data frame
pred_m2_df_fixef <- nd_m2_fixef %>% 
  mutate(Fledglings = pred_m2_fixef$fit, 
         CI = pred_m2_fixef$se.fit)


ggplot() +
  scale_colour_viridis_d() +
  geom_ribbon(data = pred_m2_df_fixef,
              aes(x = CV, ymin = Fledglings - CI, ymax = Fledglings + CI), 
              alpha = 0.5, colour = NA) +
  geom_line(data = pred_m2_df_fixef, aes(x = CV, y = Fledglings)) +
  geom_line(data = pred_m2_df_spp, 
            aes(x = CV, y = Fledglings, colour = Species.Name))

