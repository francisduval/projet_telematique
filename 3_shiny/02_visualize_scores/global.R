# Packages ----------------------------------------------------------------------------------------------------------------------
source("source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets_sample <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample.RDS"))
scores_distance <- loadRDS(here("2_pipeline", "11_compute_scores_density", "scores_distance.RDS"))
scores_time <- loadRDS(here("2_pipeline", "11_compute_scores_density", "scores_time.RDS"))
claims <- loadRDS(here("2_pipeline", "14_output_claims", "claims_covariates.RDS"))

scores <- scores_distance %>% left_join(scores_time, by = "ENROLLED_VIN")

VINS <- unique(trajets_sample$ENROLLED_VIN)

info_drivers <- scores %>% 
  filter(ENROLLED_VIN %in% VINS) %>% 
  left_join(claims, by = "ENROLLED_VIN")

drivers <- trajets_sample %>% 
  group_by(ENROLLED_VIN) %>% 
  nest() %>% 
  left_join(scores, by = "ENROLLED_VIN") %>% 
  na.omit()


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a hexagonal plot. Useful for using with purrr:map function
plot_hex <- function(data, x, y, title = NULL, subtitle = NULL) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_hex(bins = 48) +
    scale_x_time(breaks = seq(0, 86400, by = 3600), labels = 0:24, limits = c(0, 86400)) +
    scale_fill_viridis(name = "Nombre de trajets") +
    labs(title = title, subtitle = subtitle) +
    theme_bw()
}

# Creates a scatter plot
plot_scatter <- function(data, x, y, title = NULL, subtitle = NULL) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = 0.4) +
    scale_x_time(breaks = seq(0, 86400, by = 3600), labels = 0:24, limits = c(0, 86400)) +
    labs(title = title, subtitle = subtitle) +
    theme_bw()
}

# Plot an histogram with a vertical line
plot_hist <- function(data, x, vline, bins = 50) {
  ggplot(data, aes(x = {{x}})) +
    geom_histogram(fill = "black", col = "white", bins = bins) +
    geom_vline(xintercept = vline, col = "red") +
    theme_bw() +
    labs(y = NULL)
}

# Plot an histogram
plot_hist_2 <- function(data, x, binwidth, xlim) {
  enquo_x <- enquo(x)
  string_x <- quo_name(enquo_x)
  
  ggplot(data, aes(x = !!enquo_x)) +
    geom_histogram(binwidth = binwidth, color = "white", fill = "darkblue") +
    scale_x_continuous(limits = xlim) +
    ylab("Nombre de trajets") +
    xlab(NULL) +
    theme_bw()
}

# Function to make the bottom part of the Shiny app
make_histograms <- function(vin) {
  info_driver <- info_drivers %>% 
    filter(ENROLLED_VIN == vin)
  
  p1 <- plot_hist(scores, x = top_1_VSS_DISTANCE, vline = info_driver$top_1_VSS_DISTANCE) 
  p2 <- plot_hist(scores, x = top_2_VSS_DISTANCE, vline = info_driver$top_2_VSS_DISTANCE)
  p3 <- plot_hist(scores, x = top_3_VSS_DISTANCE, vline = info_driver$top_3_VSS_DISTANCE)
  p4 <- plot_hist(scores, x = top_4_VSS_DISTANCE, vline = info_driver$top_4_VSS_DISTANCE)
  p5 <- plot_hist(scores, x = nb_intervals_VSS_DISTANCE, vline = info_driver$nb_intervals_VSS_DISTANCE)
  p6 <- plot_hist(scores, x = perc_var_explained_VSS_DISTANCE, vline = info_driver$perc_var_explained_VSS_DISTANCE) + 
    xlim(99.975, 100)
  
  grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
}

# Function to make the bottom part of the Shiny app
make_histograms_time <- function(vin) {
  info_driver <- info_drivers %>% 
    filter(ENROLLED_VIN == vin)
  
  p1 <- plot_hist(scores, x = top_1_time, vline = info_driver$top_1_time) 
  p2 <- plot_hist(scores, x = top_2_time, vline = info_driver$top_2_time)
  p3 <- plot_hist(scores, x = top_3_time, vline = info_driver$top_3_time)
  p4 <- plot_hist(scores, x = top_4_time, vline = info_driver$top_4_time)
  p5 <- plot_hist(scores, x = nb_intervals_time, vline = info_driver$nb_intervals_time)
  p6 <- plot_hist(scores, x = perc_var_explained_time, vline = info_driver$perc_var_explained_time) +
    xlim(99.975, 100)
  
  grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
}