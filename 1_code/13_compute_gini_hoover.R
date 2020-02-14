#################################################################################################################################
### But: Calculer les indices de Gini et de Hoover pour les 8000 conducteurs                                                  ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Inputs: trajets.RDS                                                                                                       ###
### Output: gini_hoover_indices.RDS                                                                                           ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets_df <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Function to compute Gini index over a vector given a number of intervals n ----------------------------------------------------
compute_gini <- function(x, n) {
  ineq(table(cut_interval(x, n)), type = "Gini")
}


# Function to compute 2D Gini index over 2 vectors given a number of intervals n ------------------------------------------------
compute_gini_2d <- function(x1, x2, n) {
  x1_cut <- cut_interval(x1, n)
  x2_cut <- cut_interval(x2, n)
  
  d <- tibble(x1_cut, x2_cut)
  freq <- count(d, x1_cut, x2_cut)$n
  hoover <- ineq(freq, type = "Gini")
  
  return(hoover)
}


# Function to compute Hoover index over a vector given a number of intervals n --------------------------------------------------
compute_hoover <- function(x, n) {
  hoover(table(cut_interval(x, n)))
}


# Function to compute 2D Hoover index over 2 vectors given a number of intervals n ----------------------------------------------
compute_hoover_2d <- function(x1, x2, n) {
  x1_cut <- cut_interval(x1, n)
  x2_cut <- cut_interval(x2, n)
  
  d <- tibble(x1_cut, x2_cut)
  freq <- count(d, x1_cut, x2_cut)$n
  hoover <- hoover(freq)
  
  return(hoover)
}


# Compute Gini and Hoover indices for each ENROLLED_VIN (time, distance and both (2D)) ------------------------------------------
scores_df <- trajets_df %>% 
  group_by(ENROLLED_VIN) %>% 
  summarise(
    time_vec = list(hours_since_midnight),
    distance_vec = list(VSS_DISTANCE)
  ) %>% 
  mutate(
    gini_time = map_dbl(time_vec, compute_gini, n = 100),
    gini_distance = map_dbl(distance_vec, compute_gini, n = 100),
    gini_2d = map2_dbl(time_vec, distance_vec, compute_gini_2d, n = 100),
    hoover_time = map_dbl(time_vec, compute_hoover, n = 100),
    hoover_distance = map_dbl(distance_vec, compute_hoover, n = 100),
    hoover_2d = map2_dbl(time_vec, distance_vec, compute_hoover_2d, n = 100)
  ) %>% 
  select(-time_vec, -distance_vec)


# Save dataset ------------------------------------------------------------------------------------------------------------------
saveRDS(scores_df, file = here("2_pipeline", "13_compute_gini_hoover", "gini_hoover_indices.RDS"))
