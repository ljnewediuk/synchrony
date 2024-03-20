
library(tidyverse)
library(tidybayes)
library(ggdist)
library(modelr)
library(cowplot)

# 1 - Load data ====
nest_yrs <- readRDS('data/urban_birds_data2.rds')

# 2 - Load models ====

for(mod_type in c('fledg', 'urban')) {
  
  # List of models to load
  mods <- list.files('output/models', pattern = mod_type)
  
  # Load models and assign names
  for(n in 1:length(mods)) {
    mod_n <- readRDS(paste0('output/models/', mods[n]))
    assign(paste(gsub("[^A-Z]", "", mods[n]), mod_type, 'mod', sep = '_'), mod_n)
  }
  
}

# 3 - Make plots for each species ====

# Get species list
species_list <- unique(nest_yrs$Species)

# Start making plots in lists
urban_plots <- list()
fledg_plots <- list()
for(i in 1:length(species_list)) {
  
  spp <- species_list[i]
  
  # 3a - Make new data with only species of interest ----
  nd <- nest_yrs %>%
    filter(Species == spp)
  
  # Get models specific to species
  fledg_mod <- get(paste0(gsub("[^A-Z]", "", spp), '_fledg_mod'))
  urban_mod <- get(paste0(gsub("[^A-Z]", "", spp), '_urban_mod'))
  
  # 3b - Take draws from posterior ----

  # Fledglings model
  fledg_draws <- nd |>
    data_grid(a_LayDate_z = seq(from = 0, to = 2, length.out = 100),
              Urban = c(1, 0), NestID = NA)  |>
    add_epred_draws(object = fledg_mod, ndraws = 10000) %>%
    group_by(a_LayDate_z, Urban) %>%
    median_qi(.epred, .width = c(0.5, 0.8, 0.95))
  # Urban birds fledgling model
  fledg_draws_urban <- fledg_draws %>%
    filter(Urban == 1) %>%
    group_by(.width) %>%
    group_split()
  # Rural birds fledgling model
  fledg_draws_rural <- fledg_draws %>%
    filter(Urban == 0) %>%
    group_by(.width) %>%
    group_split()

  # Urban model
  urban_draws <- nd |>
    data_grid(Urban = c(1, 0), NestID = NA)  |>
    add_epred_draws(object = urban_mod, ndraws = 1000) %>%
    mutate(Urban = factor(Urban, levels = c(0, 1), labels = c('Rural', 'Urban')))
  
  # 3c - Plot urban model ----
  urban_plot <- ggplot() +
    scale_colour_manual(values = c('black', 'red')) +
    scale_fill_manual(values = c('gray', 'red')) +
    geom_violin(data = urban_draws, alpha = 0.5,
                aes(x = Urban, y = .epred,
                    colour = Urban, fill = Urban)) +
    labs(title = spp) +
    theme(panel.background = element_rect(colour = 'black',
                                          fill = 'white', linewidth = 1.25),
          axis.text = element_text(size = 13, colour = 'black'),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 13, colour = 'black', hjust = 0.5),
          legend.position = 'none',
          plot.margin = unit(c(0.25, 0.25, 0.75, 0.75), 'cm'),
          panel.grid = element_blank())
  
  # 3d - Plot fledglings model ----
  fledg_plot <- ggplot() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_lineribbon(data = fledg_draws_rural[[3]],
                    aes(x = a_LayDate_z, y = .epred, ymin = .lower, ymax = .upper),
                    fill = 'black', alpha = 0.2) +
    geom_lineribbon(data = fledg_draws_urban[[3]],
                    aes(x = a_LayDate_z, y = .epred, ymin = .lower, ymax = .upper),
                    fill = 'red', alpha = 0.2) +
    geom_smooth(data = fledg_draws_rural[[1]], colour = 'black', se = F,
                aes(x = a_LayDate_z, y = .epred)) +
    geom_smooth(data = fledg_draws_urban[[1]], colour = 'red', se = F,
                aes(x = a_LayDate_z, y = .epred)) +
    theme(panel.background = element_rect(colour = 'black',
                                          fill = 'white', linewidth = 1.25),
          axis.text = element_text(size = 13, colour = 'black'),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = 'none',
          plot.margin = unit(c(0.25, 0.25, 0.75, 0.75), 'cm'),
          panel.grid = element_blank()) +
    ylab('Z-score N fledglings') +
    xlab('|Z-score laying date|') +
    ylim(-1.5, 1.5)
  
  # 3e - Add plots to list ----
  urban_plots[[spp]] <- urban_plot
  fledg_plots[[spp]] <- fledg_plot
  
}

# 4 - Construct panel plot ====

# 4a - Make axis labels ----
# Make y axis labels
Ylab_urban <- ggplot() + geom_text(aes(x = 0, y = 0), 
                                   label = '| Z-score laying date |', 
                                   size = 5, angle = 90) + theme_void()
Ylab_fledg <- ggplot() + geom_text(aes(x = 0, y = 0), 
                                   label = 'Z-score N fledglings', 
                                   size = 5, angle = 90) + theme_void()
# Make x axis label
Xlab_fledg <- ggplot() + theme_void() +
  geom_text(aes(x = 0, y = 0), label = '| Z-score laying date |', size = 5)

# 4b - Plot panel urban model ----
# Plot
urban_panels <- plot_grid(plotlist = urban_plots, nrow = 1,
                          labels = c('a', 'b', 'c', 'd', 'e', 'f'),
                          label_size = 18)
# With y label
urban_plot <- plot_grid(Ylab_urban, NULL, urban_panels, nrow = 1, 
                           rel_widths = c(0.04, 0.01, 2))

# 4c - Plot panel fledgling model ----
# Plot
fledg_panels <- plot_grid(plotlist = fledg_plots, nrow = 1,
                          labels = c('g', 'h', 'i', 'j', 'k', 'l'),
                          label_size = 18)
# With y label
fledg_plot_y <- plot_grid(Ylab_fledg, NULL, fledg_panels, nrow = 1, 
                        rel_widths = c(0.04, 0.01, 2))
# With x label
fledg_plot <- plot_grid(fledg_plot_y, Xlab_fledg, nrow = 2, 
                      rel_heights = c(1.2, 0.2))

# 4d - Combine plots ----
combo_plot <- plot_grid(urban_plot, fledg_plot, nrow = 2, 
                        rel_heights = c(1, 1.1))

# 5 - Save plot ====
ggsave(filename = 'fig1.tiff',
       plot = last_plot(),
       device = 'tiff', path = 'figures/',
       dpi = 300, height = 13, width = 38, units = 'cm', bg = 'white')


