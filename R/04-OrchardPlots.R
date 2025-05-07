
# 03 - Make orchard plots

library(tidyverse)
library(cowplot)
library(ggdraw)

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

# 1 - Load images ====

# Western Bluebird
wstblu_img <- image_read2('images/western_bluebird.svg', cut_empty_space = T)
# Mountain Bluebird
mtnblu_img <- image_read2('images/mountain_bluebird.svg', cut_empty_space = T)
# Protonothary Warbler
prwarb_img <- image_read2('images/prothonotary_warbler.svg', cut_empty_space = T)
# Purple Martin
prpmrtn_img <- image_read2('images/purple_martin.svg', cut_empty_space = T)

# 2 - Load models ====

m_H1 <- readRDS('output/laying_var_glm.rds')
m_H2 <- readRDS('output/fledg_success_glm.rds')

# Extract random slopes for species
r_slopes_H1 <- coef(m_H1)$cond$Species.Name %>%
  rownames_to_column('Species.Name') %>%
  select(! `(Intercept)`) %>%
  # Add new column for coloured points
  mutate(col_pts = case_when(Species.Name == 'Prothonotary Warbler' ~ 'yellow',
                             Species.Name == 'Purple Martin' ~ 'purple',
                             Species.Name == 'Mountain Bluebird' ~ 'blue',
                             Species.Name == 'Western Bluebird' ~ 'orange'),
         # Add column for term
         term = 'Land.UseUrban') %>%
  rename(estimate = Land.UseUrban)

# Extract random slopes for species
r_slopes_H2 <- coef(m_H2)$cond$Species.Name %>%
  rownames_to_column('Species.Name') %>%
  select(! `(Intercept)`) %>%
  mutate(term = 'CV') %>%
  rename(estimate = CV)

# Bind together
r_slopes <- bind_rows(r_slopes_H1, r_slopes_H2) %>%
  # Add new column for plot labels
  mutate(term_name = ifelse(term == 'Land.UseUrban', 'Laying date \n variation', 
                            'Fledging \n success'),
         # Fix point colors
         col_pts = ifelse(is.na(col_pts), 'none', col_pts))

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
orch_plot <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_jitter(data = r_slopes, aes(x = term_name, y = estimate, col = col_pts, 
                                   alpha = col_pts), size = 4) + 
  scale_alpha_manual(values = c(1, 0.5, 1, 1, 1)) +
  scale_colour_manual(values = c('#6593F1', 'black', '#B47762', '#F6E44C', '#403F66')) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  geom_linerange(data = slopes, 
                 aes(x = term_name, ymin = conf.low, ymax = conf.high),
                 lwd = 5, colour = 'black') +
  geom_pointrange(data = slopes,
                  aes(x = term_name, y = estimate, ymin = conf.low, ymax = conf.high),
                  lwd = 1, shape = 21, fill = 'white', colour = 'black', stroke = 7) +
  coord_flip() + ylab('slope') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.y = element_text(size = 13, colour = 'black'), 
        axis.text.x = element_text(size = 13, colour = 'black'),
        axis.title.x = element_text(size = 13, vjust = -3),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 13, colour = 'black')) +
  ylim(-1, 1) +
  ylab('Slope estimate') 

ggdraw() +
  draw_plot(orch_plot, scale = 0.9, vjust = 0.05) +
  draw_image(mtnblu_img, scale = 0.15, hjust = -.3, vjust = -.28) +
  draw_image(wstblu_img, scale = 0.15, hjust = -.3, vjust = -.12) +
  draw_image(prwarb_img, scale = 0.15, hjust = -.3, vjust = 0.04) +
  draw_image(prpmrtn_img, scale = 0.15, hjust = -.3, vjust = 0.2)

# Save as tiff
ggsave('figures/orchard.tiff', plot = last_plot(), bg = 'white',
       device = 'tiff', dpi = 300, height = 12, width = 18, units = 'cm')

# Save as svg
ggsave('figures/orchard.svg', plot = last_plot(), bg = 'white',
       device = 'svg', dpi = 300, height = 12, width = 18, units = 'cm')
