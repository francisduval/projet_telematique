#################################################################################################################################
### But: Tracer les nuages de points des centroids et regarder pour chacun s'il y a une réclamation                           ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Input: drivers_centroids.RDS                                                                                              ###
### Outputs:  centroids_drivers_claims.pdf                                                                                    ###
#################################################################################################################################

source("1_code/00_source.R")


# Load data ---------------------------------------------------------------------------------------------------------------------
drivers <- readRDS(here("2_pipeline", "17_output_drivers_centroids", "drivers_centroids.RDS"))

drivers %<>% 
  mutate(claim_ind = factor(claim_ind, levels = c("0", "1"))) %>% 
  select(
    -FUEL_CONSUMPTION,
    -MIL_STATUS,
    -TRIP_POSITIONAL_QUALITY
  )


# Function to make the plots ----------------------------------------------------------------------------------------------------
make_plot <- function(x, y) {
  x <- sym(x)
  y <- sym(y)
  
  ggplot() +
    geom_point(data = filter(drivers, claim_ind == "0"), aes(x = !!x, y = !!y), pch = 20, alpha = 0.5) +
    geom_point(data = filter(drivers, claim_ind == "1"), aes(x = !!x, y = !!y), pch = 20, col = "red") +
    labs(
      title = glue("Centroids for the {nrow(drivers)} drivers"), 
      subtitle = "A red dot indicates that the driver had a claim"
    ) +
    theme_bw() +
    theme(legend.position = "none")
}


# Make a list with all combination of 2 variables -------------------------------------------------------------------------------
combine_var <- combn(names(drivers[2:7]), m = 2, simplify = F)


# Save plots --------------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "18_centroids_drivers_claims", "centroids_drivers_claims.pdf")) 
map(combine_var, ~ make_plot(.x[[1]], .x[[2]]))
dev.off()
