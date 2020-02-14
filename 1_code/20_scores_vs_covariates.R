#################################################################################################################################
### But: Observer la distribution                                                                                                                      ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Décembre 2019                                                                                                       ###
### Inputs: claims_covariates.RDS, fichiers de scores                                                                         ###
### Output: plots_*.pdf                                                                                                       ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
claims <- loadRDS(here("2_pipeline", "14_output_claims", "claims_covariates.RDS"))
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


# Function to plot a violin plot ------------------------------------------------------------------------------------------------
plot_violin <- function(data, x, y) {
  ggplot(data, aes(x = !!ensym(x), y = !!ensym(y), fill = !!ensym(x))) +
    geom_violin(alpha = 0.7) +
    coord_flip() +
    theme_bw() +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme(legend.position = "none")
}
  

# Function to save pdfs ---------------------------------------------------------------------------------------------------------
save_pdf <- function(object, filename) {
  pdf(file = here("2_pipeline", "20_scores_vs_covariates", filename)) 
  print(object)
  dev.off()
}
  

# Make plots --------------------------------------------------------------------------------------------------------------------
scores <- names(c(scores_distance[-1], scores_time[-1], scores_distance_time[-1], scores_gini_hoover[-1]))

plots_DRIVER_GENDER <- map(scores, plot_violin, data = claims_scores, x = "DRIVER_GENDER")
plots_DRIVER_MARITALSTATUS <- map(scores, plot_violin, data = claims_scores, x = "DRIVER_MARITALSTATUS")
plots_region <- map(scores, plot_violin, data = claims_scores, x = "region")
plots_driver_age_cat <- map(scores, plot_violin, data = claims_scores, x = "driver_age_cat")
plots_driver_yearslicensed_cat <- map(scores, plot_violin, data = claims_scores, x = "driver_yearslicensed_cat")


# Save plots --------------------------------------------------------------------------------------------------------------------
save_pdf(plots_DRIVER_GENDER, filename = "plots_DRIVER_GENDER.pdf")
save_pdf(plots_DRIVER_MARITALSTATUS, filename = "plots_DRIVER_MARITALSTATUS.pdf")
save_pdf(plots_region, filename = "plots_region.pdf")
save_pdf(plots_driver_age_cat, filename = "plots_driver_age_cat.pdf")
save_pdf(plots_driver_yearslicensed_cat, filename = "plots_driver_yearslicensed_cat.pdf")
