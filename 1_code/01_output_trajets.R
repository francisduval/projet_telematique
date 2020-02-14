#################################################################################################################################
### But: Prend la base trajets originale, fait plusieurs modifications et output une base de données                          ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juin 2019                                                                                                           ###
### Input: VIN1.csv, VIN2.csv, VIN3.csv, VIN4.csv                                                                             ###
### Output: trajets.RDS                                                                                                       ###
#################################################################################################################################

source("1_code/00_source.R")


# Fonctions ---------------------------------------------------------------------------------------------------------------------

# Get number of days since monday midnight for datetime object
get_nb_days_since_monday <- function(datetime) {
  wday(datetime, week_start = 1) + hour(datetime) / 24 + minute(datetime) / 1440 + second(datetime) / 86400 - 1
}

# Get number of days since beginning of month for datetime object
get_nb_days_since_beg_month <- function(datetime) {
  day(datetime) + hour(datetime) / 24 + minute(datetime) / 1440 + second(datetime) / 86400
}

# Create new variables and drop missing observations, drop trips of less than 1km or less than 2 minutes 
save_data <- function(import, export) {
  data <- read_csv(here("0_data", import))
  
  res <- data %>% 
    mutate(
      ENROLLED_VIN         = as.character(ENROLLED_VIN),
      datetime             = parse_date_time2(LOCAL_TRIP_START_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      datetime_end         = parse_date_time2(LOCAL_TRIP_END_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      date                 = date(datetime),
      year                 = year(datetime),
      month                = month(datetime, label = T),
      day                  = day(datetime),
      yday                 = yday(date),
      weekday              = factor(wday(datetime, week_start = 1), ordered = T, lev = 1:7, lab = day.abb),
      time                 = as.hms(datetime, tz = "UTC"),
      hour                 = hour(datetime),
      duration             = as.numeric(difftime(datetime_end, datetime, units = "mins")),
      avg_speed            = VSS_DISTANCE / duration * 60,
      hours_since_midnight = as.numeric(time) / 3600,
      days_since_beg_month = get_nb_days_since_beg_month(datetime),
      days_since_monday    = get_nb_days_since_monday(datetime)
    ) %>%
    filter(VSS_DISTANCE >= 1, duration >= 2) %>% 
    select(-TRIP_NUMBER, -LOCAL_TRIP_START_TIMESTAMP, - LOCAL_TRIP_END_TIMESTAMP) %>% 
    na.omit()
  
  saveRDS(res, file = here("2_pipeline", "01_output_trajets", export))
}


# Exporter les bases de données -------------------------------------------------------------------------------------------------
noms <- glue("VIN{1:4}")
walk(noms, ~ save_data(glue("{.x}.csv"), export = glue("trajets_{.x}.RDS")))


# Importer les bases de données, les combiner et réexporter ---------------------------------------------------------------------
files <- dir_ls(here("2_pipeline", "01_output_trajets"), regexp = "(trajets_VIN)[1-4].RDS")
trajets <- map_dfr(files, readRDS)
file_delete(files)

saveRDS(trajets, file = here("2_pipeline", "01_output_trajets", "trajets.RDS"))
