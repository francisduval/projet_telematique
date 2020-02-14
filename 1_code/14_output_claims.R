#################################################################################################################################
### But: Créer une base de données de sinistres                                                                               ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Août 2019                                                                                                           ###
### Inputs: ContractAuto.csv, trajets.RDS                                                                                     ###
### Output: claims.RDS, claims_covariates.RDS                                                                                 ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))
contrats <- read_csv(here("0_data", "ContractAuto.csv"))


# Make datset containing ENROLLED_VIN and different covariates ------------------------------------------------------------------
covariates <- contrats %>%
  mutate(temp = str_sub(GARAGEDPOSTALCODE, 1, 1)) %>% 
  mutate(
    region = case_when(
      temp == "M" ~ "Metropolitan Toronto",
      temp == "N" ~ "Southwestern Ontario",
      temp == "P" ~ "Northern Ontario",
      temp == "L" ~ "Central Ontario",
      temp == "K" ~ "Eastern Ontario",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    DRIVER_AGE = as.integer(DRIVER_AGE),
    DRIVER_GENDER = factor(DRIVER_GENDER),
    DRIVER_MARITALSTATUS = factor(DRIVER_MARITALSTATUS),
    DRIVER_YEARSLICENSED = as.integer(DRIVER_YEARSLICENSED),
    region = factor(region),
    driver_age_cat = cut(DRIVER_AGE, breaks = c(-Inf, 25, 60, Inf), labels = c("0-25","26-60","60+")),
    driver_yearslicensed_cat = cut(DRIVER_YEARSLICENSED, breaks = c(-Inf, 3, 10, Inf), labels = c("0-3", "4-10", "10+"))
  ) %>% 
  select(
    ENROLLED_VIN,
    DRIVER_AGE,
    DRIVER_GENDER,
    DRIVER_MARITALSTATUS,
    DRIVER_YEARSLICENSED,
    region,
    driver_age_cat,
    driver_yearslicensed_cat
  )

covariates$DRIVER_GENDER[covariates$DRIVER_GENDER == "Unknown"] <- "Male"


# Keep VINs that are in the trajets dataset only, select useful variables, parse dates ------------------------------------------
contrats %<>% 
  filter(ENROLLED_VIN %in% trajets$ENROLLED_VIN) %>% 
  select(
    ENROLLED_VIN,
    exposit10,
    exposit12,
    exposit26,
    FirstClaim_cov1,
    FirstClaim_cov2,
    FirstClaim_cov3,
    FirstClaim_cov4,
    SecClaim_cov1,
    SecClaim_cov2,
    SecClaim_cov3,
    SecClaim_cov4,
    ThirdClaim_cov1,
    ThirdClaim_cov2,
    ThirdClaim_cov3,
    ThirdClaim_cov4,
    FirstClaim_date,
    SecClaim_date,
    ThirdClaim_date
  ) %>%
  mutate(
    FirstClaim_date = parse_date_time2(FirstClaim_date, "%d%b%y:%H:%M:%S"),
    SecClaim_date = parse_date_time2(SecClaim_date, "%d%b%y:%H:%M:%S"),
    ThirdClaim_date = parse_date_time2(as.character(ThirdClaim_date), "%d%b%y:%H:%M:%S")
  )


# Keep only contracts having the same exposure for coverages 10, 12, and 26. Unite coverages columns ----------------------------
contrats %<>% 
  filter(exposit10 == exposit12 & exposit10 == exposit26) %>% 
  select(
    -exposit10,
    -exposit12,
    -exposit26
  ) %>% 
  unite(
    FirstClaim_cov,
    FirstClaim_cov1,
    FirstClaim_cov2,
    FirstClaim_cov3,
    FirstClaim_cov4
  ) %>% 
  unite(
    SecClaim_cov,
    SecClaim_cov1,
    SecClaim_cov2,
    SecClaim_cov3,
    SecClaim_cov4
  ) %>% 
  unite(
    ThirdClaim_cov,
    ThirdClaim_cov1,
    ThirdClaim_cov2,
    ThirdClaim_cov3,
    ThirdClaim_cov4
  )


# Create tibble with all ENROLLED_VIN having same exposure for coverages 10, 12, and 26 -----------------------------------------
export <- tibble(ENROLLED_VIN = unique(contrats$ENROLLED_VIN))


# Create dataset having 3 lines per contract (1 for each potential claim) and columns coverages and date of the claim -----------
coverages <- contrats %>% 
  select(
    ENROLLED_VIN,
    contains("cov")
  ) %>% 
  gather(
    key, 
    coverages,
    -ENROLLED_VIN,
  ) %>% 
  select(-key)

# ----------

dates <- contrats %>% 
  select(
    ENROLLED_VIN,
    contains("date")
  ) %>% 
  gather(
    key, 
    date,
    -ENROLLED_VIN,
  ) %>% 
  select(-key)

# ----------

claims <- tibble(
  ENROLLED_VIN = coverages$ENROLLED_VIN, 
  coverages = coverages$coverages, 
  claim_date = dates$date
)


# Keep only rows having a claim affecting coverages 10, 12 or 26 ----------------------------------------------------------------
claims %<>% 
  filter(str_detect(coverages, "(10|12|26)")) %>% 
  select(-coverages)


# Get date of first and last trips and exposition in years for each ENROLLED_VIN ------------------------------------------------
dates_first_last_trips <- trajets %>%
  select(ENROLLED_VIN, date) %>% 
  group_by(ENROLLED_VIN) %>%
  mutate(
    date_first_trip = min(date),
    date_last_trip = max(date)
  ) %>% 
  select(-date) %>% 
  distinct()


# Get rid of claims happening before first trip or after last trip --------------------------------------------------------------
claims %<>% 
  left_join(dates_first_last_trips, by = "ENROLLED_VIN") %>% 
  filter(claim_date <= date_last_trip & claim_date >= date_first_trip) %>% 
  select(
    -date_first_trip, 
    -date_last_trip
  ) %>% 
  group_by(ENROLLED_VIN) %>% 
  summarise(nb_claims = n())


# Compute exposition for each driver --------------------------------------------------------------------------------------------
expo <- trajets %>% 
  group_by(ENROLLED_VIN) %>% 
  summarise(
    expo_time = as.numeric(max(date) - min(date)) / 365.25,
    expo_distance = sum(VSS_DISTANCE)
  )


# Merge all info and create frequency variables ---------------------------------------------------------------------------------
export %<>% 
  left_join(expo, by = "ENROLLED_VIN") %>% 
  left_join(claims, by = "ENROLLED_VIN") %>% 
  replace_na(list(nb_claims = 0)) %>% 
  mutate(
    freq_time = nb_claims / expo_time,
    freq_distance = nb_claims / expo_distance * 10000
  )


# Add some covariates (keep first value only, for instance age changes year to year) --------------------------------------------
export_covariates <- export %>% 
  left_join(covariates, by = "ENROLLED_VIN") %>% 
  distinct(ENROLLED_VIN, .keep_all = TRUE)


# Save dataset ------------------------------------------------------------------------------------------------------------------
saveRDS(export, file = here("2_pipeline", "14_output_claims", "claims.RDS"))
saveRDS(export_covariates, file = here("2_pipeline", "14_output_claims", "claims_covariates.RDS"))
