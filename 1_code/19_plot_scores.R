#################################################################################################################################
### But: Visualiser tous les scores de routine calculés                                                                       ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Octobre 2019                                                                                                        ###
### Inputs: trajets.RDS, scores_distance.RDS, scores_time.RDS, scores_distance_time.RDS, gini_index.RDS                       ###
### Output: Plusieurs fichiers PDF dans le répertoire Output -> Scores                                                        ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
trajets_df <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))

files <- c(
  "2_pipeline/11_compute_scores_density/scores_distance.RDS", 
  "2_pipeline/11_compute_scores_density/scores_time.RDS", 
  "2_pipeline/12_compute_scores_rectangles/scores_distance_time.RDS", 
  "2_pipeline/13_compute_gini_hoover/gini_hoover_indices.RDS"
)

scores <- map_dfc(files, loadRDS)

vins <- scores %>% select(ENROLLED_VIN)
scores %<>% select(-contains("ENROLLED_VIN"))
scores_vin <- bind_cols(vins, scores) %>% nest(-ENROLLED_VIN, .key = "scores")


# Create constructor for S3 object of type "driver" -----------------------------------------------------------------------------
new_driver <- function(vin, distance_vec, time_vec, scores) {
  structure(
    list(
      vin = vin,
      nb_trips = length(distance_vec),
      distance_vec = distance_vec,
      time_vec = time_vec,
      scores = scores
    ),
    class = "driver"
  )
}


# Plot method for objects of type "driver" --------------------------------------------------------------------------------------
plot.driver <- function(driver, type = 1) {
  if(type == 1) {
    data <- enframe(driver$distance)
    bw <- 2
    xlim <- c(0, 100)
    xlab <- "Distance (km)"
    title <- "Histogram of distance per trip"
    color <- "blue"
  } else if(type == 2) {
    data <- enframe(driver$time)
    bw <- 900
    xlim <- c(0, 86400)
    xlab <- "Time of the day"
    title <- "Histogram of time of the day per trip"
    color <- "darkgreen"
  } else if(type == 3) {
    data <- tibble(distance = driver$distance, time = driver$time)
    
    p <- ggplot(data, aes(x = time, y = distance)) +
      geom_hex(bins = 48) +
      scale_x_time("Time of the day", limits = c(0, 86400)) +
      scale_y_continuous("Distance") +
      scale_fill_viridis() +
      labs(
        title = "Hexagonal plot of time of the day and distance per trip",
        subtitle = glue("Driver {driver$vin} ({driver$nb_trips} trips)")
      ) +
      theme_bw() +
      theme(legend.title = element_blank())
  }
  
  if(type %in% 1:2) {
    hist <- ggplot(data, aes(x = value)) +
      geom_histogram(binwidth = bw, col = "white", fill = color) +
      labs(
        title = title,
        subtitle = glue("Driver {driver$vin} ({driver$nb_trips} trips)")
      ) +
      xlab(xlab) +
      ylab("Number of trips") +
      theme_bw()
  }
  
  if(type == 1) {
    p <- hist + scale_x_continuous(limits = xlim)
  } else if(type == 2) {
    p <- hist + scale_x_time(limits = xlim)
  }
  return(p)
}


# Given a numeric vector, returns the indices of n values equally spaced --------------------------------------------------------
find_ind_quantiles <- function(x, n = 50) {
  i <- round(seq(1, length(x), length.out = n))
  x_sort <- sort(x)
  val <- x_sort[i]
  ind <- map_int(map(val, ~ which(x == .x)), 1)
  return(ind)
}


# Function to plot an histogram -------------------------------------------------------------------------------------------------
plot_hist <- function(x, subtitle = NULL) {
  data <- enframe(x)
  xlim <- range(x)
  
  ggplot(data, aes(x = value)) + 
    geom_histogram(binwidth = diff(xlim) / 50, col = "black", fill = "white") +
    labs(subtitle = subtitle) +
    xlim(xlim) +
    xlab("Score") +
    ylab("Number of drivers") +
    theme_bw()
}


# Function to save a list of plots in a pdf file --------------------------------------------------------------------------------
save_pdf <- function(plot_list, pdf_name) {
  pdf(file = here("2_pipeline", "19_plot_scores", glue("{pdf_name}.pdf"))) 
  walk(plot_list, plot)
  dev.off()
}


# Create dataframe of drivers ---------------------------------------------------------------------------------------------------
drivers <- trajets_df %>% 
  mutate(num_time = as.numeric(time)) %>% 
  group_by(ENROLLED_VIN) %>% 
  summarise(distance = list(VSS_DISTANCE), time = list(num_time)) %>% 
  left_join(scores_vin, by = "ENROLLED_VIN") %>% 
  mutate(driver = pmap(list(ENROLLED_VIN, distance, time, scores), new_driver)) %>% 
  select(driver)


# Compute a histogram for each routine score ------------------------------------------------------------------------------------
hist_scores <- scores %>% imap(~ plot_hist(.x, subtitle = glue("Histogram of {.y} score")))


# Find indices of drivers to use for each score ---------------------------------------------------------------------------------
ind_drivers <- map_df(scores, find_ind_quantiles)
selected_drivers <- map(ind_drivers, ~ drivers[.x, ])


# Add histogram for time, histogram for distance and scatterplot for each driver ------------------------------------------------
selected_drivers %<>% 
  map(
    mutate, 
    hist_distance = map(driver, plot, type = 1),
    hist_time = map(driver, plot, type = 2),
    scatter = map(driver, plot, type = 3)
  )


# Add scores --------------------------------------------------------------------------------------------------------------------
selected_drivers %<>% 
  imap(mutate, scores = map(driver, "scores")) %>% 
  imap(~ mutate(.x, score = map_dbl(scores, .y)))


# Add global histograms of scores -----------------------------------------------------------------------------------------------
selected_drivers %<>% 
  imap(~ mutate(.x, hist_score = list(hist_scores[[.y]]))) %>% 
  map(~ mutate(.x, hist_score = map2(hist_score, score, ~ .x + geom_vline(xintercept = .y, col = "red", size = 0.9))))


# Create 3 lists (one for each type of score) -----------------------------------------------------------------------------------
ind_distance <- imap_lgl(selected_drivers, ~ str_detect(.y, pattern = "(DISTANCE)|(distance)"))
ind_time <- imap_lgl(selected_drivers, ~ str_detect(.y, pattern = "time"))
ind_both <- imap_lgl(selected_drivers, ~ str_detect(.y, pattern = "(time)|(DISTANCE)|(distance)", negate = T))

scores_distance <- selected_drivers[ind_distance]
scores_time <- selected_drivers[ind_time]
scores_both <- selected_drivers[ind_both]


# Combine plots together --------------------------------------------------------------------------------------------------------
scores_distance %<>% 
  map(~ mutate(.x, plot = map2(hist_distance, hist_score, ~ grid.arrange(.x, .y, nrow = 2, ncol = 1, heights = c(2, 1)))))

scores_time %<>% 
  map(~ mutate(.x, plot = map2(hist_time, hist_score, ~ grid.arrange(.x, .y, nrow = 2, ncol = 1, heights = c(2, 1)))))

scores_both %<>% 
  map(~ mutate(.x, plot = map2(scatter, hist_score, ~ grid.arrange(.x, .y, nrow = 2, ncol = 1, heights = c(2, 1)))))


# Save plots --------------------------------------------------------------------------------------------------------------------
walk(names(scores_distance), ~ save_pdf(scores_distance[[.x]][["plot"]], pdf_name = .x))
walk(names(scores_time), ~ save_pdf(scores_time[[.x]][["plot"]], pdf_name = .x))
walk(names(scores_both), ~ save_pdf(scores_both[[.x]][["plot"]], pdf_name = .x))
