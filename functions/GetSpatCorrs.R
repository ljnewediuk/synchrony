
library(tidyverse)
library(fields)

spat_corr <- function(nestdat, yr, species, lagdist, distlim = NULL, iterations) {
  
  # Get lay dates in specific year and species
  nests <- nestdat %>%
    filter(Year %in% yr & 
             Species.Name %in% species) %>%
    mutate(LayDate = as_date(First.Lay.Date)) %>%
    mutate(LayDate_j = yday(LayDate)) %>%
    filter(! is.na(LayDate_j)) 
  
  # Compute Great circle distance matrix from nest data
  nestdists <- as.matrix(rdist.earth(x1 = cbind(nests$Latitude, nests$Longitude), 
                                     miles = F, R = NULL))
  # Zeroes on diagonal
  diag(nestdists) <- 0
  
  # Function to calculate autocorrelation at specified lag distance
  autocorr <- function(xy, z, dist = lagdist) {
    # If setting a distance limit, set max(xy) to that limit,
    # otherwise calculate it from the maximum distance in the data
    if(is.null(distlim)) {
      max_xy <- max(xy)
    } else {
      max_xy <- distlim
    }
    # Get number of bands (max distance/lags) and distances
    n_bands <- ceiling(max_xy/dist)
    dists <- seq(0, n_bands*dist, dist)
    # Get pairwise correlations based on lay date for each band
    cors <- NULL
    for(i in 1: n_bands) {
      w1 <- ifelse(xy > dists[i] & xy <= dists[i + 1], 1, 0) 
      w2 <- w1
      for(j in 1: dim(w1)[1]) {
        nu <- sum(w1[j ,])
        if(nu > 0) {
          w2[j ,] <- w1[j ,]/nu
        }  
      }
      lag <- w2 %*% z
      cors <- c(cors, cor(z, lag))
    }
    # Bind correlations with distances in data.frame
    autocorrs <- data.frame(Distance = dists[-1], spatAutoCorr = cors)
    # Return data.frame
    return(autocorrs)
  }
 
  # Get spatial autocorrelation at specified distances
  ac <- autocorr(xy = nestdists, z = nests$LayDate_j)
  
  # Monte Carlo analysis to get 95% confidence intervals for null hypothesis
  # of random spatial autocorrelation
  mc <- matrix(NA, nrow = iterations, ncol = nrow(ac))
  for(i in 1:iterations) {
    # Randomly sample lay dates x iteration number
    nests$rand <- sample(nests$LayDate_j, length(nests$LayDate_j), replace =  F)
    # Test autocorrelation based on random nest dates
    mc[i,] <- autocorr(xy = nestdists, z = nests$rand) %>%
      pull(spatAutoCorr)
  }
  # Get 95% confidence intervals
  mc_cis <- as.data.frame(t(apply(mc, 2, quantile, probs = c(0.025, 0.975), na.rm = T)))
  
  # Bind autocorrelation with 95% confidence intervals of random autocorrelation
  ac_mc <- bind_cols(ac, mc_cis)
}
