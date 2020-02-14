#################################################################################################################################
### But: Calculer différents scores de routine pour chacun des conducteurs (basés sur kernel density estimator)               ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Input: trajets.RDS                                                                                                        ###
### Outputs: scores_distance.RDS, scores_time.RDS                                                                             ###
### Running time: environ 7 heures                                                                                            ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Computes total sum of squares on a numeric vector
TSS <- function(x) {
  sum((x - mean(x)) ^ 2)
}

# Computes different stats per interval (given interval breaks) in a numeric vector
compute_stats_per_interval <- function(data, breaks) {
  intervals <- cut(data, breaks = c(-Inf, breaks, Inf))
  splits <- split(data, intervals)

  ranges <- map_dbl(splits, ~ diff(range(.)))
  nb_obs <- map_dbl(splits, length)
  TSS <- map_dbl(splits, TSS)
  
  res <- list(
    ranges = ranges,
    nb_obs = nb_obs,
    TSS = TSS
  )

  return(res)
}

# Find minima of a 'density' object i.e. the values of x where the derivative equals zero
find_minima <- function(density) {
  x <- density$x
  y <- round(density$y, digits = 15)

  ts_y <- ts(y)
  tp <- turnpoints(ts_y)

  minima_ind <- tp$tppos[1:(tp$nturns / 2) * 2]
  minima <- x[minima_ind]

  return(minima)
}

# Performs natural intervals computing using Kernel Density Estimation
kde_intervals <- function(data, bw) {
  density <- density(data, kernel = "gaussian", bw = bw, n = 10000)

  breaks <- find_minima(density)
  nb_intervals <- length(breaks) + 1
  TSS <- TSS(data)
  total_var <- var(data)
  stats <- compute_stats_per_interval(data, breaks)

  res <- structure(
    list(
      breaks = breaks,
      nb_intervals = nb_intervals,
      ranges = stats$ranges,
      nb_obs = stats$nb_obs,
      TSS = stats$TSS,
      total_TSS = TSS,
      largest_range = max(stats$ranges),
      nb_obs_total = length(data),
      bw = bw
    ),
    class = "kde_intervals"
  )

  return(res)
}

# Performs kde intervals until all ranges are smaller than max_range
make_intervals <- function(data, max_range) {
  bw <- diff(range(data)) / 100
  kde_intervals <- kde_intervals(data, bw)

  while (kde_intervals$largest_range > max_range) {
    bw <- bw * 0.9
    kde_intervals <- kde_intervals(data, bw)
  }

  return(kde_intervals)
}

# Computes different routine scores given a 'kde_intervals' object 
routine_scores <- function(kde_intervals) {
  nb_intervals <- kde_intervals$nb_intervals
  perc_var_explained <- 100 - sum(kde_intervals$TSS) / kde_intervals$total_TSS * 100
  top_1_10 <- map(1:10, ~ sum(head(sort(kde_intervals$nb_obs, decreasing = T), .)) / sum(kde_intervals$nb_obs_total) * 100)
  
  res <- c(nb_intervals, perc_var_explained, top_1_10)
  names(res) <- c("nb_intervals", "perc_var_explained", glue("top_{1:10}"))

  return(res)
}

# Creates a database with one row per driver containing all defined routine scores. 
tidy_scores <- function(trip_data, variable, max_range) {
  variable <- quo_name(enquo(variable))
  
  trip_data %>%
    group_by(ENROLLED_VIN) %>%
    nest() %>%
    mutate(
      var = map(data, ~ as.numeric(`[[`(., !!variable))),
      intervals = map(var, ~ make_intervals(., max_range = max_range)),
      scores = map(intervals, routine_scores),
      names = map(scores, names)
    ) %>% 
    select(
      ENROLLED_VIN,
      scores,
      names
    ) %>% 
    unnest() %>% 
    spread(
      names, 
      scores
    ) %>% 
    select(
      -top_10, 
      everything()
    ) %>% 
    rename_at(
      vars(-ENROLLED_VIN), 
      function(x) glue("{x}_{variable}")
    )
}


# Create datasets containing all routine scores for each driver -----------------------------------------------------------------
scores_distance <- tidy_scores(trajets, variable = VSS_DISTANCE, max_range = 1)
scores_time <- tidy_scores(trajets, variable = time, max_range = 900)


# Transform list columns to numeric ---------------------------------------------------------------------------------------------
scores_distance %<>% modify_if(is.list, as.numeric) 
scores_time %<>% modify_if(is.list, as.numeric) 


# Save datasets -----------------------------------------------------------------------------------------------------------------
saveRDS(scores_distance, file = here("2_pipeline", "11_compute_scores_density", "scores_distance.RDS"))
saveRDS(scores_time, file = here("2_pipeline", "11_compute_scores_density", "scores_time.RDS"))
