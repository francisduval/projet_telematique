#################################################################################################################################
### But: Calculer les scores de routine pour chaque VIN en faisant des rectangles sur la distance et le temps de la journée   ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Septembre 2019                                                                                                      ###
### Inputs: trajets.RDS                                                                                                       ###
### Output: scores_distance_time.RDS                                                                                          ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

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

# Performs kde intervals until all ranges are smaller than max_range
make_intervals <- function(x, max_range) {
  bw <- diff(range(x)) / 100
  biggest_range <- Inf
  
  while(biggest_range > max_range) {
    bw <- bw * 0.9
    density <- density(x, kernel = "gaussian", bw = bw, n = 10000)
    breaks <- find_minima(density)
    intervals <- cut(x, breaks = c(-Inf, breaks, Inf))
    splits <- split(x, intervals)
    biggest_range <- max(map_dbl(splits, ~ diff(range(.))))
  }
  
  breaks <- c(-Inf, breaks, Inf)
  return(breaks)
}

# Given a dataset containing only 1 driver with variables VSS_DISTANCE and time, compute routine scores
compute_scores <- function(data) {
  dist <- data %>% pull(VSS_DISTANCE)
  time <- data %>% pull(time) %>% as.numeric
  
  breaks_distance <- make_intervals(dist, max_range = 1)
  breaks_time <- make_intervals(as.numeric(time), max_range = 900)
  
  bin_xy <- tibble(
    dist = findInterval(dist, breaks_distance),
    time = findInterval(time, breaks_time)
  )
  
  results <- c(table(bin_xy))
  nb_trips <- sum(results)
  
  res <- list()
  
  res$nb_rectangles <- length(results)
  res$top_1 <- sum(tail(sort(results), 1)) / nb_trips * 100
  res$top_5 <- sum(tail(sort(results), 5)) / nb_trips * 100
  res$top_10 <- sum(tail(sort(results), 10)) / nb_trips * 100
  res$top_50 <- sum(tail(sort(results), 50)) / nb_trips * 100
  res$top_100 <- sum(tail(sort(results), 100)) / nb_trips * 100
  res$top_500 <- sum(tail(sort(results), 500)) / nb_trips * 100
  res$top_1000 <- sum(tail(sort(results), 1000)) / nb_trips * 100
  
  return(res)
}


# Compute scores ----------------------------------------------------------------------------------------------------------------
results <- trajets %>%
  select(ENROLLED_VIN, VSS_DISTANCE, time) %>%
  group_by(ENROLLED_VIN) %>%
  nest() %>%
  mutate(
    scores = map(data, compute_scores),
    names = map(scores, names)
  ) %>% 
  unnest(scores, names) %>% 
  spread(names, scores) %>%
  mutate(
    nb_rectangles = unlist(nb_rectangles),
    top_1 = unlist(top_1),
    top_5 = unlist(top_5),
    top_10 = unlist(top_10),
    top_50 = unlist(top_50),
    top_100 = unlist(top_100),
    top_500 = unlist(top_500),
    top_1000 = unlist(top_1000),
  ) %>% 
  select(
    ENROLLED_VIN,
    nb_rectangles,
    top_1,
    top_5,
    top_10,
    top_50,
    top_100,
    top_500,
    top_1000
  )


# Save dataset ------------------------------------------------------------------------------------------------------------------
saveRDS(results, file = here("2_pipeline", "12_compute_scores_rectangles", "scores_distance_time.RDS"))
