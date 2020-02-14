#################################################################################################################################
### But: Visualisation individuelle de conducteurs en 2018 avec des histogrammes                                              ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juillet 2019                                                                                                        ###
### Input: trajets_sample_2018.RDS                                                                                            ###
### Output: 1 pdf par histogramme: histograms_per_driver*.pdf                                                                 ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a histogram. Useful for using with purrr:map function
plot_hist <- function(data, x, binwidth, xlim, sub = NULL) {
  enquo_x <- enquo(x)
  string_x <- quo_name(enquo_x)
  
  p <- ggplot(data, aes(x = !!enquo_x)) +
         geom_histogram(binwidth = binwidth, color = "white", fill = "darkblue") +
         ylab("Number of trips") +
         xlab(NULL) +
         labs(title = glue("Histogram of {string_x} - Year 2018"), subtitle = sub) +
         theme_bw()

  var <- data[[string_x]]

  if (is.numeric(var)) {
    res <- p + scale_x_continuous(limits = xlim)
  } else if (is.hms(var)) {
    res <- p + scale_x_time(limits = xlim)
  } else {
    res <- p
  }
  
  return(res)
}


# Exports in PDF format. I put this in a function so I can use it with purrr::walk function
export_pdf <- function(object, filename, ...) {
  pdf(file = here("2_pipeline", "07_histograms_per_driver", filename), ...)
  print(object)
  dev.off()
}


# Compute different histograms for each driver ----------------------------------------------------------------------------------
drivers_histograms <- trajets %>%
  group_by(ENROLLED_VIN) %>%
  nest() %>% 
  mutate(
    subtitle   = map2(data, ENROLLED_VIN, ~ glue("Driver {.y} ({nrow(.x)} trips)")),
    
    date       = map2(data, subtitle, ~ plot_hist(.x, x = datetime, bin = 86400, sub = .y)),
    distance   = map2(data, subtitle, ~ plot_hist(.x, x = VSS_DISTANCE, bin = 1, xlim = c(0, 100), sub = .y)),
    avg_speed  = map2(data, subtitle, ~ plot_hist(.x, x = VSS_AVG_SPEED, bin = 5, xlim = c(0, 120), sub = .y)),
    max_speed  = map2(data, subtitle, ~ plot_hist(.x, x = VSS_MAX_SPEED, bin = 5, xlim = c(0, 150), sub = .y)),
    time       = map2(data, subtitle, ~ plot_hist(.x, x = time, bin = 900, xlim = c(0, 86400), sub = .y)),
    duration   = map2(data, subtitle, ~ plot_hist(.x, x = duration, bin = 5, xlim = c(0, 300), sub = .y)),
    days_month = map2(data, subtitle, ~ plot_hist(.x, x = days_since_beg_month, bin = 1, xlim = c(0, 32), sub = .y)),
    days_week  = map2(data, subtitle, ~ plot_hist(.x, x = days_since_monday , bin = 1 / 24, xlim = c(0, 7), sub = .y))
  )


# Export histograms -------------------------------------------------------------------------------------------------------------
histograms <- select(drivers_histograms, -ENROLLED_VIN, -data, -subtitle)
iwalk(histograms, ~ export_pdf(.x, filename = glue("histograms_per_driver_{.y}.pdf"), width = 12))
