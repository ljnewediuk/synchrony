
# 03 - Fit GLM models

# This script fits two models to test the hypotheses:
#   1 - Inconsistent cues in urban areas results in more variation in laying 
#       dates among nests in urban areas (P: CV is higher in urban than rural
#       areas).
#   2 - Since synchronizing reproduction with food availability, etc. is
#       adaptive, nests with less variation in laying dates will be more 
#       successful (P: fledgling success is lower in urban/rural areas with
#       higher nest CV)

library(glmmTMB)
library(tidyverse)

# 1 - Load data ====

# Load the laying dates data
lay_data <- readRDS('output/laying_data.rds')

# Load the reproductive success data and bind to laying data
fledg_data <- readRDS('output/fledgling_data.rds') %>%
  na.omit() %>%
  right_join(lay_data, relationship = 'many-to-many')

# 2 - Fit H1 model ====

# Test if lay dates are more variable within vs. outside cities

# Response: Beta distribution CV lay dates (continuous between 0 and 1)
# Predictor: Land.Use (category: urban, rural)
# Link function: Logit
# Random slopes for:
#   City
#   Species
#   Year

# Fit model (beta regression)
m_H1 <- glmmTMB::glmmTMB(
  CV ~ Land.Use + (Land.Use | City) + (Land.Use | Year) + (Land.Use | Species.Name),
  family = glmmTMB::beta_family(link = "logit"),
  data = lay_data
)

# Summarize
summary(m_H1)

# Quickly check species coefficients
coef(m_H1)$cond$Species.Name

# Save model
saveRDS(m_H1, 'output/laying_var_glm.rds')

# 3 - Fit H2 model ====

# Test if variable lay dates reduce fledgling success

# Response: Zero-inflated Poisson distribution fledgling success
# Predictor: CV lay dates
# Link function: Log
# Random slopes for:
#   City
#   Species
#   Year

# Fit model (zero-inflated poisson)
m_H2 <- glmmTMB::glmmTMB(
  Young.Fledged ~ CV + (CV | City) + (CV | Year) + (CV | Species.Name),
  ziformula = ~1,
  family = poisson,
  data = fledg_data
)

# Summarize
summary(m_H2)

# Quickly check species coefficients
coef(m_H2)$cond$Species.Name

# Save model
saveRDS(m_H2, 'output/fledg_success_glm.rds')
