#################################################################################################################################
### But: Créer 2 échantillons de conducteurs: 1 avec conducteurs pris au hasard et l'autre avec conducteurs observés en 2018  ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juillet 2019                                                                                                        ###
### Input: trajets.RDS                                                                                                        ###
### Output: trajets_sample.RDS,  trajets_sample_2018.RDS                                                                      ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Keep only drivers observed all year long in 2018 ------------------------------------------------------------------------------
trajets_2018 <- trajets %>%
  filter(year == 2018) %>% 
  group_by(ENROLLED_VIN) %>% 
  filter(min(month) == "Jan" & max(month) == "Dec") %>% 
  ungroup()


# Take 50 drivers ar random -----------------------------------------------------------------------------------------------------
set.seed(2019)
VINS_2018 <- sample(trajets_2018$ENROLLED_VIN, size = 50)
VINS <- sample(trajets$ENROLLED_VIN, size = 50)

trajets_sample <- trajets %>% 
  filter(ENROLLED_VIN %in% VINS)

trajets_2018_sample <- trajets_2018 %>% 
  filter(ENROLLED_VIN %in% VINS_2018)


# Export databases --------------------------------------------------------------------------------------------------------------
saveRDS(trajets_sample, file = here("2_pipeline", "05_output_trajets_samples", "trajets_sample.RDS"))
saveRDS(trajets_2018_sample, file = here("2_pipeline", "05_output_trajets_samples", "trajets_sample_2018.RDS"))
