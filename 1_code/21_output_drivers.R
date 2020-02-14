#################################################################################################################################
### But: Créer un jeu de données qui résume l'information télématique pour chaque ENROLLED_VIN                                ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Décembre 2019                                                                                                       ###
### Input: trajets.RDS                                                                                                        ###
### Output: drivers.RDS                                                                                                       ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Summarise trips per ENROLLED_VIN and create new variables ---------------------------------------------------------------------
drivers <- trajets %>% 
  group_by(ENROLLED_VIN) %>% 
  mutate(
    ind_night_trip = hours_since_midnight < 6,
    ind_trip_weekend = weekday %in% c("Sat", "Sun")
  ) %>% 
  summarise(
    nb_trips = n(),
    nb_days_observed = as.numeric(max(date) - min(date)),
    total_distance_km = sum(VSS_DISTANCE),
    total_duration_hour = sum(duration) / 60,
    total_idle_time_hour = sum(IDLE_TIME) / 3600,
    annual_distance_km = total_distance_km / nb_days_observed * 365.25,
    annual_duration_hour = total_duration_hour / nb_days_observed * 365.25,
    annual_idle_time_hour = total_idle_time_hour / nb_days_observed * 365.25,
    perc_trip_night = sum(ind_night_trip) / nb_trips,
    perc_trip_weekend = sum(ind_trip_weekend) / nb_trips,
    avg_speed = total_distance_km / total_duration_hour,
  )


# Sauvegarder la base de données ------------------------------------------------------------------------------------------------
saveRDS(drivers, file = here("2_pipeline", "21_output_drivers", "drivers.RDS"))

