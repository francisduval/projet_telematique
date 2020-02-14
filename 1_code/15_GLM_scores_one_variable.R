#################################################################################################################################
### But: Faire des GLM Poisson à 1 variable sur chacun des scores de routine calculés                                         ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Inputs: claims.RDS, scores_distance.RDS, scores_time.RDS, scores_distance_time.RDS, gini_hoover_indices.RDS               ###
### Output: Table HTML montrant les résultats des GLM                                                                         ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
claims <- loadRDS(here("2_pipeline", "14_output_claims", "claims.RDS"))
scores_files <- c(
  "2_pipeline/11_compute_scores_density/scores_distance.RDS", 
  "2_pipeline/11_compute_scores_density/scores_time.RDS", 
  "2_pipeline/12_compute_scores_rectangles/scores_distance_time.RDS", 
  "2_pipeline/13_compute_gini_hoover/gini_hoover_indices.RDS"
)

scores_df <- map(scores_files, ~ loadRDS(here(.x))) %>% 
  reduce(left_join, by = "ENROLLED_VIN") %>% 
  filter(ENROLLED_VIN %in% claims$ENROLLED_VIN) %>% 
  select(-ENROLLED_VIN)


# Entrainer les GLM -------------------------------------------------------------------------------------------------------------
glm_results <- scores_df %>%
  map(~ glm(claims$nb_claims ~ .x + offset(log(claims$expo_time)), family = poisson, data = scores_df)) %>% 
  map(tidy) %>% 
  map_df(slice, 2) %>% 
  mutate(term = names(scores_df)) %>% 
  setNames(c("Variable", "Estimé", "Écart-type", "Statistique z", "Valeur-p"))


# Function to print HTML table --------------------------------------------------------------------------------------------------
print_results <- function(data) {
  data %>% 
    mutate(`Valeur-p` = formatC(round(`Valeur-p`, 4), format = "f", digits = 4)) %>% 
    mutate(`Valeur-p` = cell_spec(`Valeur-p`, "html", background = ifelse(`Valeur-p` < 0.05, "green", "red"), color = "white")) %>% 
    kable(format = "html", linesep = "", escape = F) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      position = "left"
    )
}


# Print HTML table --------------------------------------------------------------------------------------------------------------
print_results(glm_results)
