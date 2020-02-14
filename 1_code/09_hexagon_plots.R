#################################################################################################################################
### But: Visualisation individuelle de conducteurs en 2018 avec des graphiques en hexagone (distance vs heure)                ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juillet 2019                                                                                                        ###
### Input: trajets_sample_2018.RDS                                                                                            ###
### Output: 1 hexagonal plot pour chaque conducteur: hexplot_distance_vs_time_per_driver.pdf                                  ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a hexagon plot. Useful for using with purrr:map function
plot_hex <- function(data, x, y, sub = NULL) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_hex(bins = 48) +
    scale_x_time(breaks = seq(0, 86400, by = 3600), labels = 0:24, limits = c(0, 86400)) +
    scale_fill_viridis() +
    labs(title = "Year 2018", subtitle = sub) +
    theme_bw() +
    theme(legend.title = element_blank())
}

# Exports in PDF format. I put this in a function so I can use it with purrr::walk function
export_pdf <- function(object, filename, ...) {
  pdf(file = here("2_pipeline", "09_hexagon_plots", filename), ...)
  print(object)
  dev.off()
}


# Compute different hexagonal plots for each driver -----------------------------------------------------------------------------
drivers_hexplots <- trajets %>%
  group_by(ENROLLED_VIN) %>%
  nest() %>% 
  mutate(
    sub = map2(data, ENROLLED_VIN, ~ glue("Driver {.y} ({nrow(.x)} trips)")),
    hexplot = map2(data, sub, ~ plot_hex(.x, x = time, y = VSS_DISTANCE, sub = .y)),
  )


# Export hexagonal plots --------------------------------------------------------------------------------------------------------
export_pdf(drivers_hexplots$hexplot, filename = "hexplot_distance_vs_time_per_driver.pdf")
