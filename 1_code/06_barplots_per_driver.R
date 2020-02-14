#################################################################################################################################
### But: Visualisation individuelle de conducteurs en 2018 avec des graphiques à bandes                                       ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juillet 2019                                                                                                        ###
### Input: trajets_sample_2018.RDS                                                                                            ###
### Output: 1 pdf par graphique à barres: barplots_per_drivers_*.pdf                                                          ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Creates a barplot. Useful for using with purrr:map function
plot_col <- function(data, x, y, FUN = sum, subtitle = NULL) {
  x <- enquo(x)
  y <- enquo(y)
  col_name <- glue("{quo_name(enquo(FUN))}_{quo_name(y)}")
  
  sum_data <- data %>%
    group_by(!!x) %>%
    summarise(!!col_name := FUN(!!y))

  x_var <- rlang::sym(names(sum_data[1]))
  y_var <- rlang::sym(names(sum_data[2]))
  
  ggplot(sum_data, aes(x = !!x_var, y = !!y_var)) +
    geom_col(color = "white", fill = "darkgreen") +
    scale_y_continuous(labels = function(xx) format(xx, big.mark = ",", scientific = FALSE)) +
    labs(title = "Year 2018", subtitle = subtitle) +
    theme_bw()
}

# Exports in PDF format. I put this in a function so I can use it with purrr::walk function
export_pdf <- function(object, filename, ...) {
  pdf(file = here("2_pipeline", "06_barplots_per_driver", filename), ...)
    print(object)
  dev.off()
}


# Compute different barplots for each driver ------------------------------------------------------------------------------------
drivers_barplots <- trajets %>%
  group_by(ENROLLED_VIN) %>%
  nest() %>% 
  mutate(
    subtitle           = map2(data, ENROLLED_VIN, ~ glue("Driver {.y} ({nrow(.x)} trips)")),
    
    distance_vs_month  = map2(data, subtitle, ~ plot_col(.x, month, VSS_DISTANCE, subtitle = .y)),
    distance_vs_dotm   = map2(data, subtitle, ~ plot_col(.x, day, VSS_DISTANCE, subtitle = .y)),
    distance_vs_dotw   = map2(data, subtitle, ~ plot_col(.x, weekday, VSS_DISTANCE, subtitle = .y)),
    distance_vs_hour   = map2(data, subtitle, ~ plot_col(.x, hour, VSS_DISTANCE, subtitle = .y)),
    distance_vs_doty   = map2(data, subtitle, ~ plot_col(.x, date, VSS_DISTANCE, subtitle = .y)),
    
    idle_vs_month      = map2(data, subtitle, ~ plot_col(.x, month, IDLE_TIME, subtitle = .y, FUN = mean)),
    idle_vs_dotm       = map2(data, subtitle, ~ plot_col(.x, day, IDLE_TIME, subtitle = .y, FUN = mean)),
    idle_vs_dotw       = map2(data, subtitle, ~ plot_col(.x, weekday, IDLE_TIME, subtitle = .y, FUN = mean)),
    idle_vs_hour       = map2(data, subtitle, ~ plot_col(.x, hour, IDLE_TIME, subtitle = .y, FUN = mean)),
    idle_vs_doty       = map2(data, subtitle, ~ plot_col(.x, date, IDLE_TIME, subtitle = .y, FUN = mean)),
    
    avg_speed_vs_month = map2(data, subtitle, ~ plot_col(.x, month, VSS_AVG_SPEED, subtitle = .y, FUN = mean)),
    avg_speed_vs_dotm  = map2(data, subtitle, ~ plot_col(.x, day, VSS_AVG_SPEED, subtitle = .y, FUN = mean)),
    avg_speed_vs_dotw  = map2(data, subtitle, ~ plot_col(.x, weekday, VSS_AVG_SPEED, subtitle = .y, FUN = mean)),
    avg_speed_vs_hour  = map2(data, subtitle, ~ plot_col(.x, hour, VSS_AVG_SPEED, subtitle = .y, FUN = mean)),
    avg_speed_vs_doty  = map2(data, subtitle, ~ plot_col(.x, date, VSS_AVG_SPEED, subtitle = .y, FUN = mean)),
  )


# Export barplots ---------------------------------------------------------------------------------------------------------------
barplots <- select(drivers_barplots, -ENROLLED_VIN, -data, -subtitle)
iwalk(barplots, ~ export_pdf(.x, filename = glue("barplots_per_drivers_{.y}.pdf"), width = 12))
