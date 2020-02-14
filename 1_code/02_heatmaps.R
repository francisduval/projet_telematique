#################################################################################################################################
### But: Visualiser le nombre de trajets par jour à l'aide d'un calendrier "heatmap"                                          ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juin 2019                                                                                                           ###
### Input: trajets.RDS                                                                                                        ###
### Output: heatmaps_nb_trajets.pdf, heatmaps_moy_trajets.pdf                                                                 ###
#################################################################################################################################

source("1_code/00_source.R")


# Fonctions ---------------------------------------------------------------------------------------------------------------------

# Input: datetime object, Output: week number of the month (either 1, 2, 3, 4 or 5)
monthweeks <- function(date) {
  ceiling(as.numeric(format(date, "%d")) / 7)
}


# Importation de la base trajets ------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Arranger les données ----------------------------------------------------------------------------------------------------------

# Count number of trips per day
dat_nombre <- trajets %>% 
  group_by(date) %>% 
  summarise(n_trajets = n()) %>% 
  na.omit()

# Count average number of trips over all drivers per day
dat_moyenne <- trajets %>% 
  group_by(date, ENROLLED_VIN) %>% 
  summarise(n_trajets = n()) %>%
  group_by(date) %>% 
  summarise(moy_trajets = mean(n_trajets)) %>% 
  na.omit()

# ----------  

dat_nombre_2013_2018 <- dat_nombre %>% filter(date < "2019-01-01")
dat_nombre_2015_2019 <- dat_nombre %>% filter(date >= "2015-01-01")

dat_moyenne_2013_2018 <- dat_moyenne %>% filter(date < "2019-01-01")
dat_moyenne_2015_2019 <- dat_moyenne %>% filter(date >= "2015-01-01")


# Show heatmaps -----------------------------------------------------------------------------------------------------------------
col <- "r2g"

title1 <- "Number of trips"
calendarHeat(dat_nombre_2013_2018$date, dat_nombre_2013_2018$n_trajets, color = col, main = title1)
calendarHeat(dat_nombre_2015_2019$date, dat_nombre_2015_2019$n_trajets, color = col, main = title1)

title2 <- "Average number of trips per drivers\n(among drivers who did at least 1 trip that day)"
calendarHeat(dat_moyenne_2013_2018$date, dat_moyenne_2013_2018$moy_trajets, color = col, main = title2)
calendarHeat(dat_moyenne_2015_2019$date, dat_moyenne_2015_2019$moy_trajets, color = col, main = title2)


# Save heatmaps -----------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "02_heatmaps", "heatmaps_nb_trajets.pdf")) 
  calendarHeat(dat_nombre_2013_2018$date, dat_nombre_2013_2018$n_trajets, color = col, main = title1)
  calendarHeat(dat_nombre_2015_2019$date, dat_nombre_2015_2019$n_trajets, color = col, main = title1)
dev.off()

pdf(file = here("2_pipeline", "02_heatmaps", "heatmaps_moy_trajets.pdf")) 
  calendarHeat(dat_moyenne_2013_2018$date, dat_moyenne_2013_2018$moy_trajets, color = col, main = title2)
  calendarHeat(dat_moyenne_2015_2019$date, dat_moyenne_2015_2019$moy_trajets, color = col, main = title2)
dev.off()
