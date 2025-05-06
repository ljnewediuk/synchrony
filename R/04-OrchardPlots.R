
# 03 - Make orchard plots

library(tidyverse)
library(cowplot)

# This script pulls the fixed and random effects from the glm models to make
# a 2-panel orchard plot:

# PANEL A - Main effect for synchrony, showing no global relationship between 
#           site location (urban/rural) and laying date CV. Also add random 
#           effects points for species, showing some species with obvious 
#           positive/negative responses.
# PANEL B - Main effect for number of fledglings, showing fewer fledged
#           when laying date CV is higher. Points showing this is a nearly
#           universal effect for all species; the variation in the relationship 
#           is all between cities and years. This suggests that if species are 
#           responding differently to cues in urban/rural environments, this 
#           could affect their reproductive success; some species that become 
#           more synchronous in cities may have an advantage, while those
#           becoming less synchronous will have a disadvantage. Add random 
#           effects points for species, showing all species have the same 
#           negative response.

# 1 - Load models ====

m_H1 <- readRDS('output/laying_var_glm.rds')
m_H2 <- readRDS('output/fledg_success_glm.rds')

# Extract random slopes for species
r_slopes_H1 <- coef(m_H1)$cond$Species.Name %>%
  rownames_to_column('Species.Name') %>%
  select(! `(Intercept)`) %>%
  mutate(term = 'Land.UseUrban') %>%
  rename(estimate = Land.UseUrban)

# Extract random slopes for species
r_slopes_H2 <- coef(m_H2)$cond$Species.Name %>%
  rownames_to_column('Species.Name') %>%
  select(! `(Intercept)`) %>%
  mutate(term = 'CV') %>%
  rename(estimate = CV)

# Bind together
r_slopes <- bind_rows(r_slopes_H1, r_slopes_H2) %>%
  mutate(term_name = ifelse(term == 'Land.UseUrban', 'Laying date \n variation', 'Fledging \n success'))

# Extract the slope from the model with CIs
slope_H1 <- broom.mixed::tidy(m_H1, conf.int = T) %>%
  filter(term == 'Land.UseUrban') %>%
  select(term, estimate, conf.low, conf.high)

# Extract the slope from the model with CIs
slope_H2 <- broom.mixed::tidy(m_H2, conf.int = T) %>%
  filter(term == 'CV') %>%
  select(term, estimate, conf.low, conf.high)

# Bind together
slopes <- bind_rows(slope_H1, slope_H2) %>%
  mutate(term_name = ifelse(term == 'Land.UseUrban', 'Laying date \n variation', 'Fledging \n success'))

# Orchard plot panel
ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_jitter(data = r_slopes, aes(x = term_name, y = estimate), 
              size = 4, alpha = 0.7, colour = 'black') + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  geom_linerange(data = slopes, 
                 aes(x = term_name, ymin = conf.low, ymax = conf.high),
                 lwd = 5, colour = '#ff4040') +
  geom_pointrange(data = slopes,
                  aes(x = term_name, y = estimate, ymin = conf.low, ymax = conf.high),
                  lwd = 1, shape = 21, fill = 'white', colour = '#ff4040', stroke = 7) +
  coord_flip() + ylab('slope') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -3),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylim(-1, 1) +
  ylab('Slope estimate') 

