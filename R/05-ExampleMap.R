
# 05 - Make example map/graphical abstract

# Graphical abstract showing how I clipped the nest locations within the urban 
# area and within the 10-km buffer surrounding the city. This is combined with a 
# larger context map of the continental US, and the predictions are shown in 
# the other panels

library(tidyverse)
library(sf)
library(cowplot)
library(rnaturalearth)

# Load function to get nest distance metrics
source('functions/SubsetNests.R')

# 1 - Load data ====

# Define the root directory where data are stored
root_dir <- '../../../OneDrive - University of Manitoba/Documents/'

# ESRI USA census urban areas (encompassing at least 2,000 housing units or 
# having a population of at least 5,000)
urbandat <- st_read(paste0(root_dir, 'Spatial Data/USA Census Urban Areas/tl_2020_us_uac10/'))

# Nest data from Nestwatch
nestdat <- read.csv(paste0(root_dir, 'Other Data/NestWatch Data/nestwatch_download_aug3_2023.csv')) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(urbandat))

# 2 - Load images ====

# Image of focal species (tree swallow)
trswal_img <- image_read2('images/tree_swallow.svg', cut_empty_space = T)

# 2 - Subset a single city as an example and plot ====

# Get polygon for city
city <- urbandat %>%
  filter(NAME10 == 'Kalamazoo, MI')

# Buffer 5 km around the city, then 15, and take difference
city_buff <- suppressWarnings(st_difference(
  st_buffer(city, dist = 15000), 
  st_buffer(city, dist = 5000)
))

# Bounding box for the city buffer (to add to inset map)
city_bbox <- st_bbox(city_buff) %>%
  st_as_sfc() %>%
  st_buffer(dist = 50000)

# Filter nests within the city
nests_within_city <- nestdat %>% 
  st_filter(city) %>%
  filter(Species.Name == 'Tree Swallow' & Year == 2012)

# Filter nests within buffer outside the city
nests_outside_city <- nestdat %>%
  st_filter(city_buff) %>%
  filter(Species.Name == 'Tree Swallow' & Year == 2012)

# Plot
city_map <- ggplot() + 
  geom_sf(data = city, fill = 'darkgrey', colour = 'black') + 
  geom_sf(data = city_buff, fill = 'darkgrey', colour = 'black') +
  geom_sf(data = nests_within_city, colour = '#ffd700', fill = '#ffd70020',
          pch = 21, size = 3, linewidth = 1) +
  geom_sf(data = nests_outside_city, colour = '#008080', fill = '#00808020',
          pch = 21, size = 3, linewidth = 1) +
  coord_sf(crs = usa_proj) +
  theme_void() +
  theme(plot.margin = unit(c(2, 2, 0, 0), 'cm'))

# 3 - Load continental-US map and plot inset ====

# Set projection
usa_proj <- "+proj=laea +lat_0=10 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "

# Map of the continental USA
cont_usa <- ne_states(country = 'United States of America') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

# Inset map
inset_usa <- ggplot() + 
  geom_sf(data = cont_usa, fill = '#EEEEEE', colour = '#CCCCCC') + 
  geom_sf(data = city_bbox, fill = 'black', colour = 'black') +
  coord_sf(crs = usa_proj) +
  theme_void()

# Combine into inset
map_panel <- ggdraw() +
  draw_image(trswal_img, scale = 0.3, hjust = -.3, vjust = -.3) +
  draw_plot(city_map, scale = 0.9, vjust = 0.05) +
  draw_plot(inset_usa, x = 0.02, y = 0.65, 
            width = 0.35, height = 0.35, scale = 1.1, vjust = 0.1)

# 4 - Plot hypothetical laying date distributions ====

# Make data frame for hypothetical laying dates
hyp_lay_dat <- data.frame(Lay.Date = c(rnorm(400, 150, 10), rnorm(400, 160, 25)),
                          Land.Use = c(rep('rural', 400), rep('urban', 400)))

# Plot panel
dens_panel <- hyp_lay_dat %>%
  ggplot(aes(x = Lay.Date, colour = Land.Use, fill = Land.Use)) +
  scale_colour_manual(values = c('#008080', '#ffd700')) +
  scale_fill_manual(values = c('#00808050', '#ffd70050')) +
  xlab('Laying date') +
  geom_density(linewidth = 1) +
  theme(panel.background = element_rect(fill = 'white', 
                                        colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 1), 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(size = 15, colour = 'white'),
        axis.title.x = element_text(size = 15, colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, colour = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(0.8,0.8)
  )

# 5 - Plot hypothetical relationship between CV and fledglings ====

# Make data frame for hypothetical laying dates
hyp_fledg_dat <- data.frame(CV = seq(0, 1, length.out = 100)) %>%
  mutate(Fledglings = 5 * exp(-CV * 2))

# Plot panel
fledg_panel <- hyp_fledg_dat %>%
  ggplot(aes(x = CV, y = Fledglings)) +
  xlab('Variation in laying date') + ylab('Number of fledglings') +
  geom_line(linewidth = 2, colour = '#808080') +
  theme(panel.background = element_rect(fill = 'white', 
                                        colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 1), 'cm'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(size = 15, colour = 'black'),
        axis.title.x = element_text(size = 15, colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, colour = 'black'),
        legend.position = 'inside',
        legend.position.inside = c(0.8,0.8)
  )
  
# 6 - Plot panels into final plot and save ====

# 1-column panel for the data plots
data_plots <- plot_grid(dens_panel, fledg_panel, 
                        ncol = 1, labels = c('B', 'C'))

# Combine the map-image panel with the data plots
plot_grid(map_panel, data_plots, 
          labels = c('A', ''), rel_widths = c(1, 0.6))

# Save as svg
ggsave('figures/graph_abstract.svg', plot = last_plot(), bg = 'white',
       device = 'svg', dpi = 300, height = 15, width = 25, units = 'cm')

# Save as tiff
ggsave('figures/graph_abstract.tiff', plot = last_plot(), bg = 'white',
       device = 'tiff', dpi = 300, height = 15, width = 25, units = 'cm')
