#################################################################################################################################
### But: Faire des graphique à barres pour toutes les combinaisons de variables qui ont du sens                               ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juin 2019                                                                                                           ###
### Input: trajets.RDS                                                                                                        ###
### Output: barplots_global.pdf                                                                                               ###
#################################################################################################################################

source("1_code/00_source.R")


# Functions ---------------------------------------------------------------------------------------------------------------------

### Fonction pour faire un graphique à bandes
colplot_fun <- function(data, x, y, xlabel = x, ylabel = y, stat = mean) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  
  data %<>% group_by(!!x) %>% summarise(mean_y = stat(!!y)) %>% na.omit()
  
  ggplot(data, aes(!!x, mean_y)) +
    geom_col(fill = "tomato2", width = 0.6) +
    xlab(xlabel) +
    ylab(ylabel) +
    labs(title = paste0("Mean of ", ylabel, " per ", xlabel)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.1))
}


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Make barplot for every combination of categorical of integer variable with numeric variable -----------------------------------
xvars <- c("hour", "weekday", "day", "month", "year")
yvars <- c("VSS_DISTANCE", "VSS_AVG_SPEED", "VSS_MAX_SPEED", "FUEL_CONSUMPTION", "IDLE_TIME")
vars_barplot <- expand.grid(xvars, yvars, stringsAsFactors = F)


# Save plots --------------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "04_barplots_global", "barplots_global.pdf")) 
  walk2(.x = vars_barplot$Var1, .y = vars_barplot$Var2, ~ print(colplot_fun(trajets, x = .x, y = .y)))
dev.off()
