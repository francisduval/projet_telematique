# Packages ----------------------------------------------------------------------------------------------------------------------
source("source.R")

# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a scatter plot. Useful for using with purrr:map function
plot_scatter <- function(data, x, y, color = NULL, title = NULL) {
  enquo_x <- enquo(x)
  enquo_y <- enquo(y)
  enquo_color <- enquo(color)
  
  ggplot(data, aes(x = !!enquo_x, y = !!enquo_y, color = !!enquo_color)) +
    geom_point(size = 0.8) +
    labs(
      title = title, 
      color = "Clusters"
    ) +
    theme_bw()
}
