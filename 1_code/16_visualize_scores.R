#################################################################################################################################
### But: Visualiser les scores de routine calculés                                                                            ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Inputs: claims.RDS, fichiers de scores                                                                                    ###
### Output: lien_scores_frequence_expo_distance.pdf, lien_scores_frequence_expo_temps.pdf , histograms_scores.pdf             ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
claims <- loadRDS(here("2_pipeline", "14_output_claims", "claims.RDS"))
scores_distance <- loadRDS(here("2_pipeline", "11_compute_scores_density", "scores_distance.RDS"))
scores_time <- loadRDS(here("2_pipeline", "11_compute_scores_density", "scores_time.RDS"))
scores_distance_time <- loadRDS(here("2_pipeline", "12_compute_scores_rectangles", "scores_distance_time.RDS"))
scores_gini_hoover <- loadRDS(here("2_pipeline", "13_compute_gini_hoover", "gini_hoover_indices.RDS"))


# Add routine scores to the claims dataset --------------------------------------------------------------------------------------
claims_scores <- claims %>% 
  left_join(scores_distance, by = "ENROLLED_VIN") %>% 
  left_join(scores_time, by = "ENROLLED_VIN") %>% 
  left_join(scores_distance_time, by = "ENROLLED_VIN") %>% 
  left_join(scores_gini_hoover, by = "ENROLLED_VIN")


# Takes a vector of real values, makes n intervals and assign each real value to the middle of its interval ---------------------
discretize <- function(x, n = 100) {
  dist <- diff(range(x)) / n
  midpoints <- seq(min(x) + dist / 2, max(x) - dist / 2, by = dist)
  x %<>% cut_interval(n = n)
  names(midpoints) <- levels(x)
  unname(midpoints[x])
}


# Makes a grouped scatter plot of number of claims per 100 000 km vs a routine score --------------------------------------------
make_plot_distance <- function(score) {
  score <- sym(score)
  
  data_plot <- scores_discretize %>% 
    group_by(!!score) %>% 
    summarise(
      expo_distance = sum(expo_distance),
      nb_claims = sum(nb_claims)
    ) %>% 
    mutate(freq_dist = nb_claims / expo_distance * 100000)
  
  plot <- ggplot(data_plot, aes(x = !!score, y = freq_dist, size = expo_distance)) + 
    geom_point(pch = 20) +
    labs(
      y = "Number of claims per 100 000 kilometers",
      size = "Exposition (in number of km)"
    ) +
    theme_bw()
  
  print(plot)
}


# Makes a grouped scatter plot of number of claims per year vs a routine score -------------------------------------------------- 
make_plot_time <- function(score) {
  score <- sym(score)
  
  data_plot <- scores_discretize %>% 
    group_by(!!score) %>% 
    summarise(
      expo_time = sum(expo_time),
      nb_claims = sum(nb_claims)
    ) %>% 
    mutate(freq_time = nb_claims / expo_time)
  
  plot <- ggplot(data_plot, aes(x = !!score, y = freq_time, size = expo_time)) + 
    geom_point(pch = 20) +
    labs(
      y = "Number of claims per year",
      size = "Exposition (in years)"
    ) +
    theme_bw()
  
  print(plot)
}


# Makes an histogram given a dataset and a variable name given as a string ------------------------------------------------------
make_histogram <- function(data, x) {
  x <- sym(x)
  
  ggplot(data, aes(x = !!x)) + 
    geom_histogram(fill = "darkgreen", col = "white", bins = 50) +
    labs(title = glue("Histogram of {x}"), subtitle = glue("For {nrow(data)} drivers")) +
    theme_bw()
}


# Discretize all routine scores -------------------------------------------------------------------------------------------------
scores_discretize <- claims_scores %>% 
  select(matches("(VSS_DISTANCE)|(top)|(gini)|(rectangles)|(nb_intervals)|(perc_var)|(hoover)")) %>% 
  map_df(discretize, n = 100) %>% 
  bind_cols(
    tibble(
      expo_time = claims_scores$expo_time, 
      expo_distance = claims_scores$expo_distance,
      nb_claims = claims_scores$nb_claims
    )
  )


# Plot link between scores and claim frequency ----------------------------------------------------------------------------------
score_names <- names(scores_discretize[1:(length(scores_discretize) - 3)])

pdf(file = here("2_pipeline", "16_visualize_scores", "lien_scores_frequence_expo_distance.pdf")) 
walk(score_names, make_plot_distance)
dev.off()

pdf(file = here("2_pipeline", "16_visualize_scores", "lien_scores_frequence_expo_temps.pdf")) 
walk(score_names, make_plot_time)
dev.off()


# Plot histograms of the routine scores -----------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "16_visualize_scores", "histograms_scores.pdf")) 
map(names(claims_scores[-1]), ~ make_histogram(claims_scores, .x))
dev.off()
