
library(tidyverse)
library(cowplot)

# Load function to get correlograms
source('functions/GetSpatCorrs.R')

# 1 - Load data ====

nestdat <- read.csv('input/nestwatch_download_aug3_2023.csv')

# 2 - Make list with all spatial correlograms from 2010-2019 ====

corr_plots <- list()
for(yr in 2010:2019) {
  
  # Get spatial correlograms
  corr_pm <- spat_corr(nestdat = nestdat, 
                       yr = yr, 
                       species = 'Purple Martin',
                       lagdist = 500,
                       iterations = 50)
  # Plot
  plot_pm <- ggplot(corr_pm, aes(x = Distance, y = spatAutoCorr)) +
    scale_x_continuous(limits = c(0, 6000)) + 
    scale_y_continuous(limits=c(-1,1)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +   
    geom_smooth(aes(ymin = `2.5%`, ymax = `97.5%`), 
                stat= "identity", fill="#fc958f", colour="black") +
    geom_point(colour = "black", size = 3) +
    labs(tag = yr) +
    theme(panel.background = element_rect(colour = 'black', fill = 'white', 
                                          linewidth = 1.25),
          axis.text = element_text(size = 16, colour = 'black'),
          axis.title = element_blank(),
          legend.position = 'none',
          plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), 'cm'),
          plot.tag = element_text(size = 18, colour = 'black'),
          plot.tag.location = 'panel',
          plot.tag.position = c(.8, .9),
          panel.grid = element_line(linewidth = 0.5, colour = '#e5e5e5'),
          panel.border = element_rect(colour = 'black', fill = NA))
  # Add to list
  corr_plots[[as.character(yr)]] <- plot_pm
  
}

# 3 - Construct panel plot ====

# Plot panels
corr_panels <- plot_grid(plotlist = corr_plots, nrow = 2)

# Make y axis label
Ylab <- ggplot() + geom_text(aes(x = 0, y = 0), 
                             label = 'Autocorrelation index', 
                             size = 6, angle = 90) + theme_void()

# Make x axis label
Xlab <- ggplot() + geom_text(aes(x = 0, y = 0), 
                             label = 'Distance (km)', size = 6) + theme_void()

# Plot panels with y label
corr_panels_y <- plot_grid(Ylab, NULL, corr_panels, nrow = 1, rel_widths = c(0.04, 0.01, 1.2))

# Plot panels with x label
plot_grid(corr_panels_y, NULL, Xlab, nrow = 3, rel_heights = c(1.2, 0.02, 0.08))

# 4 - Save ====
ggsave('correlograms.tiff', plot = last_plot(), 
       device = 'tiff', path = 'figures/', 
       dpi = 300, height = 16, width = 40, units = 'cm', bg = 'white')