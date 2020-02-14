#################################################################################################################################
### But: Visualisation individuelle de conducteurs en 2018 avec des nuages de points                                          ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juillet 2019                                                                                                        ###
### Input: trajets_sample_2018.RDS                                                                                            ###
### Output: 1 scatterplot pour chaque paire de variables pertinente: scatterplots_per_driver_*.pdf                            ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a scatter plot. Useful for using with purrr:map function
plot_scatter <- function(data, x, y, sub = NULL) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(size = 0.5) +
    labs(title = "Year 2018", subtitle = sub) +
    theme_bw()
}

# Exports in PDF format. I put this in a function so I can use it with purrr::walk function
export_pdf <- function(object, filename, ...) {
  pdf(file = here("2_pipeline", "08_scatterplots_per_driver", filename), ...)
  print(object)
  dev.off()
}


# Compute different scatter plots for each driver -------------------------------------------------------------------------------
drivers_scatterplots <- trajets %>%
  group_by(ENROLLED_VIN) %>%
  nest() %>% 
  mutate(
    sub = map2(data, ENROLLED_VIN, ~ glue("Driver {.y} ({nrow(.x)} trips)")),
    
    avg_speed_vs_distance = map2(data, sub, ~ plot_scatter(.x, x = VSS_AVG_SPEED, y = VSS_DISTANCE, sub = .y)),
    max_speed_vs_distance = map2(data, sub, ~ plot_scatter(.x, x = VSS_MAX_SPEED, y = VSS_DISTANCE, sub = .y)),
    idle_time_vs_distance = map2(data, sub, ~ plot_scatter(.x, x = IDLE_TIME, y = VSS_DISTANCE, sub = .y)),
    quality_vs_distance   = map2(data, sub, ~ plot_scatter(.x, x = TRIP_POSITIONAL_QUALITY, y = VSS_DISTANCE, sub = .y)),
    duration_vs_distance  = map2(data, sub, ~ plot_scatter(.x, x = duration, y = VSS_DISTANCE, sub = .y)),
    time_vs_distance      = map2(data, sub, ~ plot_scatter(.x, x = time, y = VSS_DISTANCE, sub = .y)),
    datetime_vs_distance  = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = VSS_DISTANCE, sub = .y)),
    dotm_vs_distance      = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = VSS_DISTANCE, sub = .y)),
    dotw_vs_distance      = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = VSS_DISTANCE, sub = .y)),
    
    max_speed_vs_avg_speed = map2(data, sub, ~ plot_scatter(.x, x = VSS_MAX_SPEED, y = VSS_AVG_SPEED, sub = .y)),
    idle_time_vs_avg_speed = map2(data, sub, ~ plot_scatter(.x, x = IDLE_TIME, y = VSS_AVG_SPEED, sub = .y)),
    quality_vs_avg_speed   = map2(data, sub, ~ plot_scatter(.x, x = TRIP_POSITIONAL_QUALITY, y = VSS_AVG_SPEED, sub = .y)),
    duration_vs_avg_speed  = map2(data, sub, ~ plot_scatter(.x, x = duration, y = VSS_AVG_SPEED, sub = .y)),
    time_vs_avg_speed      = map2(data, sub, ~ plot_scatter(.x, x = time, y = VSS_AVG_SPEED, sub = .y)),
    datetime_vs_avg_speed  = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = VSS_AVG_SPEED, sub = .y)),
    dotm_vs_avg_speed      = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = VSS_AVG_SPEED, sub = .y)),
    dotw_vs_avg_speed      = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = VSS_AVG_SPEED, sub = .y)),
    
    idle_time_vs_max_speed = map2(data, sub, ~ plot_scatter(.x, x = IDLE_TIME, y = VSS_MAX_SPEED, sub = .y)),
    quality_vs_max_speed   = map2(data, sub, ~ plot_scatter(.x, x = TRIP_POSITIONAL_QUALITY, y = VSS_MAX_SPEED, sub = .y)),
    duration_vs_max_speed  = map2(data, sub, ~ plot_scatter(.x, x = duration, y = VSS_MAX_SPEED, sub = .y)),
    time_vs_max_speed      = map2(data, sub, ~ plot_scatter(.x, x = time, y = VSS_MAX_SPEED, sub = .y)),
    datetime_vs_max_speed  = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = VSS_MAX_SPEED, sub = .y)),
    dotm_vs_max_speed      = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = VSS_MAX_SPEED, sub = .y)),
    dotw_vs_max_speed      = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = VSS_MAX_SPEED, sub = .y)),
    
    quality_vs_idle_time   = map2(data, sub, ~ plot_scatter(.x, x = TRIP_POSITIONAL_QUALITY, y = IDLE_TIME, sub = .y)),
    duration_vs_idle_time  = map2(data, sub, ~ plot_scatter(.x, x = duration, y = IDLE_TIME, sub = .y)),
    time_vs_idle_time      = map2(data, sub, ~ plot_scatter(.x, x = time, y = IDLE_TIME, sub = .y)),
    datetime_vs_idle_time  = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = IDLE_TIME, sub = .y)),
    dotm_vs_idle_time      = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = IDLE_TIME, sub = .y)),
    dotw_vs_idle_time      = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = IDLE_TIME, sub = .y)),
    
    duration_vs_quality = map2(data, sub, ~ plot_scatter(.x, x = duration, y = TRIP_POSITIONAL_QUALITY, sub = .y)),
    time_vs_quality     = map2(data, sub, ~ plot_scatter(.x, x = time, y = TRIP_POSITIONAL_QUALITY, sub = .y)),
    datetime_vs_quality = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = TRIP_POSITIONAL_QUALITY, sub = .y)),
    dotm_vs_quality     = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = TRIP_POSITIONAL_QUALITY, sub = .y)),
    dotw_vs_quality     = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = TRIP_POSITIONAL_QUALITY, sub = .y)),
    
    time_vs_duration     = map2(data, sub, ~ plot_scatter(.x, x = time, y = duration, sub = .y)),
    datetime_vs_duration = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = duration, sub = .y)),
    dotm_vs_duration     = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = duration, sub = .y)),
    dotw_vs_duration     = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = duration, sub = .y)),
    
    datetime_vs_time = map2(data, sub, ~ plot_scatter(.x, x = datetime, y = time, sub = .y)),
    dotm_vs_time     = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = time, sub = .y)),
    dotw_vs_time     = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = time, sub = .y)),
    
    dotm_vs_datetime = map2(data, sub, ~ plot_scatter(.x, x = days_since_beg_month, y = datetime, sub = .y)),
    dotw_vs_datetime = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = datetime, sub = .y)),
    
    dotw_vs_dotm = map2(data, sub, ~ plot_scatter(.x, x = days_since_monday, y = days_since_beg_month, sub = .y)),
  )


# Export scatterplots -----------------------------------------------------------------------------------------------------------
scatterplots <- select(drivers_scatterplots, -ENROLLED_VIN, -data, -sub)
iwalk(scatterplots, ~ export_pdf(.x, filename = glue("scatterplots_per_driver_{.y}.pdf")))
