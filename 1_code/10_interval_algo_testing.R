#################################################################################################################################
### But: Tester l'algorithme qui créé des intervalles sur des données simulées et sur des conducteurs                         ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Input: trajets_sample_2018.RDS                                                                                            ###
### Output: rien                                                                                                              ###
#################################################################################################################################

source("1_code/00_source.R")


# Load the 20 driver ------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Simulate data (create vector of real numbers) ---------------------------------------------------------------------------------
set.seed(2019)

nb <- c(10, 23, 17, 16, 20)
x <- c(
  rnorm(nb[1], mean = 20, sd = 0.5),
  rnorm(nb[2], mean = 5,  sd = 0.1),
  rnorm(nb[3], mean = 10, sd = 0.5),
  rnorm(nb[4], mean = 30, sd = 0.8),
  rnorm(nb[5], mean = 18, sd = 10)
)


# Functions ---------------------------------------------------------------------------------------------------------------------

# Extract only the pits (or local minima) of a turnpoint object
find_pits <- function(tp) {
  if (!inherits(tp, "turnpoints")) {
    stop("The argument must be a 'turnpoints' object!")
  }
  
  tp_pos <- tp$tppos
  keep <- 1:(tp$nturns / 2) * 2
  tp_pos <- tp_pos[keep]
  
  return(tp_pos)
}

# Compute difference between max and min value in each interval in a numerical vector
compute_interval_ranges <- function(data, breaks) {
  clusters <- cut(data, breaks = c(-Inf, breaks, Inf))
  splits <- split(data, clusters)
  clusters_ranges <- map_dbl(splits, ~ diff(range(.)))
  
  return(clusters_ranges)
}

# Performs natural intervals computing using Kernel Density Estimation
kde_intervals <- function(data, bw) {
  density <- density(data, kernel = "gaussian", bw = bw, n = 10000)
  
  ts_y <- ts(round(density$y, 15))
  tp <- turnpoints(ts_y)
  breaks <- density$x[find_pits(tp)]
  nb_intervals <- length(breaks) + 1
  ranges <- compute_interval_ranges(data, breaks)
  
  res <- list(
    data = data, 
    density = density, 
    breaks = breaks, 
    nb_intervals = nb_intervals, 
    ranges = ranges,
    largest_range = max(ranges),
    bw = bw
  )
  
  class(res) <- "kde_intervals"
  return(res)
}

# Performs kde intervals until all ranges are smaller than max_range
make_intervals <- function(data, max_range) {
  bw <- diff(range(data)) / 100
  kde_intervals <- kde_intervals(data, bw)
  
  while(kde_intervals$largest_range > max_range) {
    bw <- bw * 0.9
    kde_intervals <- kde_intervals(data, bw)
  }
  
  return(kde_intervals)
}

# Plot method for objects of class "kde_intervals"
plot.kde_intervals <- function(kde_intervals) {
  plot(
    kde_intervals$density, 
    main = glue("Binwidth = {round(kde_intervals$bw, 4)}"), 
    xlab = glue("Largest range: {round(kde_intervals$largest_range, 2)}\n Nb intervals: {kde_intervals$nb_intervals}")
  )
  rug(kde_intervals$data, ticksize = 0.03)
  abline(v = kde_intervals$breaks, col = rep("blue", length(kde_intervals$breaks)))
}


# Test algorithm on the drivers -------------------------------------------------------------------------------------------------
test_drivers <- trajets %>% 
  group_by(ENROLLED_VIN) %>% 
  nest() %>% 
  mutate(
    distances = map(data, 1),
    intervals = map(distances, ~ make_intervals(., max_range = 1))
  )

walk(test_drivers$intervals, plot)


# Test algorithm on simulated data ----------------------------------------------------------------------------------------------
test <- make_intervals(x, max_range = 1)
plot(test)
