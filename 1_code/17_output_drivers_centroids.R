#################################################################################################################################
### But: Créer une base où chaque ligne est un conducteur. On calcule les moyennes pour chaque variable pertinente            ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Input: trajets.RDS, scores_claim_info.RDS                                                                                 ###
### Outputs: drivers_centroids.RDS                                                                                            ###
#################################################################################################################################

source("1_code/00_source.R")


# Load data ---------------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))
claims <- loadRDS(here("2_pipeline", "14_output_claims", "claims.RDS"))


# Compute indicator of a claim for each driver ----------------------------------------------------------------------------------
claims_ind <- claims %>% 
  mutate(claim_ind = as.numeric(nb_claims > 0)) %>% 
  select(
    ENROLLED_VIN,
    claim_ind
  )


# Compute centroids for each driver ---------------------------------------------------------------------------------------------
var_to_summarise <- c(
  "VSS_DISTANCE", 
  "VSS_AVG_SPEED", 
  "VSS_MAX_SPEED", 
  "FUEL_CONSUMPTION",
  "MIL_STATUS", 
  "IDLE_TIME", 
  "TRIP_POSITIONAL_QUALITY",
  "time",
  "duration"
)

drivers <- trajets %>% 
  group_by(ENROLLED_VIN) %>% 
  summarise_at(var_to_summarise, mean) %>% 
  mutate(time = as.hms(time))


# Merge -------------------------------------------------------------------------------------------------------------------------
drivers %<>%
  right_join(claims_ind, by = "ENROLLED_VIN")


# Save dataset ------------------------------------------------------------------------------------------------------------------
saveRDS(drivers, file = here("2_pipeline", "17_output_drivers_centroids", "drivers_centroids.RDS"))
