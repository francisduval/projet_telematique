#################################################################################################################################
### Goal:                                                                                                                     ###
### Author: Francis Duval                                                                                                     ###
### Date: Février 2020                                                                                                        ###
### Input: Fichiers avec les scores calculés, drivers.RDS, claims_covariates.RDS                                              ###
### Output: scores_validation.pdf, lr_test.pdf, scores_une_variable.pdf                                                       ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
score_files <- c(
  here("2_pipeline", "11_compute_scores_density", "scores_distance.RDS"),
  here("2_pipeline", "11_compute_scores_density", "scores_time.RDS"),
  here("2_pipeline", "12_compute_scores_rectangles", "scores_distance_time.RDS"),
  here("2_pipeline", "13_compute_gini_hoover", "gini_hoover_indices.RDS")
)

scores_df <- score_files %>% 
  map(loadRDS) %>% 
  reduce(left_join, by = "ENROLLED_VIN") %>% 
  rename_at(vars(-ENROLLED_VIN), ~ glue("score_{.x}"))

tele_var_df <- loadRDS(here("2_pipeline", "21_output_drivers", "drivers.RDS")) %>% 
  select(
    ENROLLED_VIN,
    perc_trip_night,
    perc_trip_weekend,
    avg_speed
  )

claims_df <- loadRDS(here("2_pipeline", "14_output_claims", "claims_covariates.RDS"))


# Function that returns a Poisson GLM object given the data, the predictors x, the response y and the offset term ---------------
fit_glm_poisson <- function(data, x, y, offset) {
  f <- as.formula(paste(y, paste0(paste(x, collapse = " + "), " + offset(log(", offset, "))"), sep = " ~ "))
  fit <- glm(f, family = poisson, data = data)
  return(fit)
}


# Choose best GLM with classical covariates with stepAIC ------------------------------------------------------------------------
summary(
  stepAIC(
    glm(
      nb_claims ~ DRIVER_AGE + DRIVER_GENDER + DRIVER_MARITALSTATUS + DRIVER_YEARSLICENSED + region + offset(log(expo_time)), 
      family = poisson,
      data = claims_df
    ),
    direction = "both",
    trace = F
  )
)


# Regroup some regions because some have a big p-value --------------------------------------------------------------------------
claims_df %<>%
  mutate(
    region = fct_collapse(
      region,
      `Metropolitan Toronto` = "Metropolitan Toronto",
      `Other` = c("Central Ontario", "Eastern Ontario", "Northern Ontario", "Southwestern Ontario")
    )
  )


# Create a dataset containing all information -----------------------------------------------------------------------------------
scores_claims_df <- claims_df %>% 
  left_join(scores_df, by = "ENROLLED_VIN") %>% 
  left_join(tele_var_df, by = "ENROLLED_VIN") %>% 
  mutate(annual_distance = expo_distance / expo_time)
  

# Function that create nb_folds folds with integer from 1 to n ------------------------------------------------------------------
create_folds <- function(n, nb_folds, theseed = 2020) {
  set.seed(theseed)
  seq(1, n) %>% 
    cut(breaks = nb_folds, labels = F) %>% 
    sample() %>% 
    factor()
}


# Performs 10-folds cross-validation for a Poisson GLM --------------------------------------------------------------------------
cv_10_folds_poisson_glm <- function(data, x, y, offset, score) {
  data %<>% mutate(fold = create_folds(nrow(.), 10))
  responses_ls <- data %>% group_split(fold) %>% map(~ pull(., {{y}}))
  
  models_ls <- map(1:10, ~ fit_glm_poisson(data = filter(data, fold != .), x = x, y = y, offset = offset))
  predictions_ls <- map(1:10, ~ predict(models_ls[[.]], newdata = filter(data, fold == .), type = "response"))
  
  res <- map2(responses_ls, predictions_ls, ~ scores(.x, .y, which = score)) %>% flatten_dbl() %>% mean()
  return(res)
}


# Computes AIC and BIC for a Poisson GLM model ----------------------------------------------------------------------------------
aic_poisson_glm <- function(data, x, y, offset) {
  AIC(fit_glm_poisson(data, x = x, y = y, offset = offset))
}

bic_poisson_glm <- function(data, x, y, offset) {
  BIC(fit_glm_poisson(data, x = x, y = y, offset = offset))
}


# Create a vector of classic predictors and a vector of routine scores ----------------------------------------------------------
classic_pred <- c("DRIVER_GENDER", "DRIVER_YEARSLICENSED", "region", "annual_distance")
scores_pred <- c(names(scores_df[-1]), names(tele_var_df[-1]))


# Create list of predictors -----------------------------------------------------------------------------------------------------
pred_ls <- map(scores_pred, ~ c(classic_pred, .x))


# Compute scores for classic covariates model and for same model but with each of the routine scores added ----------------------
logs_classic <- cv_10_folds_poisson_glm(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time", score = "logs")
logs <- map_dbl(pred_ls, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "logs"))

rps_classic <- cv_10_folds_poisson_glm(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time", score = "rps")
rps <- map_dbl(pred_ls, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "rps"))

ses_classic <- cv_10_folds_poisson_glm(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time", score = "ses")
ses <- map_dbl(pred_ls, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "ses"))

aic_classic <- aic_poisson_glm(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time")
aic <- map_dbl(pred_ls, ~ aic_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time"))

bic_classic <- bic_poisson_glm(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time")
bic <- map_dbl(pred_ls, ~ bic_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time"))

# ----------

logs_scoring <- tibble(
  modele = factor(scores_pred),
  score = logs
)

rps_scoring <- tibble(
  modele = factor(scores_pred),
  score = rps
)

ses_scoring <- tibble(
  modele = factor(scores_pred),
  score = ses
)

aic_scoring <- tibble(
  modele = factor(scores_pred),
  score = aic
)

bic_scoring <- tibble(
  modele = factor(scores_pred),
  score = bic
)


# ----------

logs_scoring %<>% 
  mutate(modele = fct_reorder(modele, score)) %>%
  mutate(type = ifelse(score < logs_classic, "above", "below")) %>%
  mutate(score = -(score - logs_classic) / logs_classic * 100)

rps_scoring %<>% 
  mutate(modele = fct_reorder(modele, score)) %>%
  mutate(type = ifelse(score < rps_classic, "above", "below")) %>%
  mutate(score = -(score - rps_classic) / rps_classic * 100)

ses_scoring %<>% 
  mutate(modele = fct_reorder(modele, score)) %>%
  mutate(type = ifelse(score < ses_classic, "above", "below")) %>%
  mutate(score = -(score - ses_classic) / ses_classic * 100)

aic_scoring %<>% 
  mutate(modele = fct_reorder(modele, score)) %>%
  mutate(type = ifelse(score < aic_classic, "above", "below")) %>%
  mutate(score = -(score - aic_classic) / aic_classic * 100)

bic_scoring %<>% 
  mutate(modele = fct_reorder(modele, score)) %>%
  mutate(type = ifelse(score < bic_classic, "above", "below")) %>%
  mutate(score = -(score - bic_classic) / bic_classic * 100)


# Function to plot the scores ---------------------------------------------------------------------------------------------------
make_plot <- function(data, score_type, subtitle = NULL) {
  ggplot(data, aes(x = modele, y = score)) + 
    geom_bar(stat = "identity", aes(fill = type), width = 0.5) +
    scale_fill_manual(
      name = "", 
      values = c("above" = "darkgreen", "below" = "red"),
      guide = F) + 
    labs(
      title = "Modèle de base: n ~ sexe + années_permis + région + distance_annuelle + ln_expo", 
      subtitle = subtitle
    ) + 
    ylab(glue("Amélioration du score « {score_type} » relativement au modèle de base (%)")) +
    xlab("Variable ajoutée au modèle de base") +
    coord_flip() +
    theme_bw()
}


# Save the plots ----------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "23_scores_validation", "scores_validation.pdf"), width = 10) 
  make_plot(logs_scoring, score_type = "logs", subtitle = "Validation croisée")
  make_plot(rps_scoring, score_type = "rps", subtitle = "Validation croisée")
  make_plot(ses_scoring, score_type = "lses", subtitle = "Validation croisée")
  make_plot(aic_scoring, score_type = "AIC")
  make_plot(bic_scoring, score_type = "BIC")
dev.off()


# Fonction pour calculer le rapport de vraisemblance entre 2 modèles ------------------------------------------------------------
compute_LR <- function(mod_complet, mod_reduit) {
  2 * (logLik(mod_complet) - logLik(mod_reduit))
}


# Ajuster le modèle réduit et tous les modèles complets pour ensuite calculer les rapports de vraisemblance ---------------------
glm_reduit <- fit_glm_poisson(scores_claims_df, x = classic_pred, y = "nb_claims", offset = "expo_time")
glm_complet_ls <- map(pred_ls, ~ fit_glm_poisson(scores_claims_df, x = .x, y = "nb_claims", offset = "expo_time"))

log_like_vec <- map_dbl(glm_complet_ls, compute_LR, mod_reduit = glm_reduit)


# Faire le graphique ------------------------------------------------------------------------------------------------------------
data_chi_deux <- tibble(var = scores_pred, lr = log_like_vec) %>% 
  mutate(var = fct_reorder(var, lr))

pdf(file = here("2_pipeline", "23_scores_validation", "lr_test.pdf"), width = 10) 
ggplot(data_chi_deux, aes(x = var, y = lr)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "white", color = "blue") +
  geom_hline(yintercept = qchisq(0.95, df = 1), linetype = "dashed") +
  labs(
    title = "Test du rapport de vraisemblance",
    subtitle = "Modèle réduit: n ~ sexe + années_permis + région + distance_annuelle + ln_expo",
    x = NULL,
    y = "Rapport de vraisemblance"
  ) +
  coord_flip() +
  theme_bw()
dev.off()


# Calculer la vraisemblance et les scores out-of-sample un prédicteur à la fois -------------------------------------------------
all_pred <- c(classic_pred, scores_pred)

AIC_vec <- map_dbl(all_pred, ~ AIC(fit_glm_poisson(scores_claims_df, x = .x, y = "nb_claims", offset = "expo_time")))
logs_vec <- map_dbl(all_pred, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "logs"))
rps_vec <- map_dbl(all_pred, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "rps"))
ses_vec <- map_dbl(all_pred, ~ cv_10_folds_poisson_glm(scores_claims_df, x = ., y = "nb_claims", offset = "expo_time", score = "ses"))

plot_df <- tibble(
  var = all_pred,
  AIC = AIC_vec,
  logs = logs_vec,
  rps = rps_vec,
  ses = ses_vec,
)

# ----------

pdf(file = here("2_pipeline", "23_scores_validation", "scores_une_variable.pdf"), width = 10) 
ggplot(plot_df %>% mutate(var = fct_reorder(var, AIC)), aes(x = var, y = AIC - 6100)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "blue") +
  labs(
    title = "GLM une variable à la fois et expo_time comme exposition",
    x = NULL,
    y = "AIC"
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = 6100 + seq(0, 100, by = 25)) +
  coord_flip() +
  theme_bw()

# ----------

ggplot(plot_df %>% mutate(var = fct_reorder(var, logs)), aes(x = var, y = logs - 0.412)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "blue") +
  labs(
    title = "GLM une variable à la fois et expo_time comme exposition",
    x = NULL,
    y = "Score « logs » (10-validation croisée)"
  ) +
  scale_y_continuous(breaks = seq(0, 0.005, by = 0.001), labels = 0.412 + seq(0, 0.005, by = 0.001)) +
  coord_flip() +
  theme_bw()

# ----------

ggplot(plot_df %>% mutate(var = fct_reorder(var, rps)), aes(x = var, y = rps - 0.1185)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "blue") +
  labs(
    title = "GLM une variable à la fois et expo_time comme exposition",
    x = NULL,
    y = "Score « rps » (10-validation croisée)"
  ) +
  scale_y_continuous(breaks = seq(0, 0.00075, by = 0.00025), labels = 0.1185 + seq(0, 0.00075, by = 0.00025)) +
  coord_flip() +
  theme_bw()

# ----------

ggplot(plot_df %>% mutate(var = fct_reorder(var, ses)), aes(x = var, y = ses - 0.1415)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "blue") +
  labs(
    title = "GLM une variable à la fois et expo_time comme exposition",
    x = NULL,
    y = "Score « ses » (10-validation croisée)"
  ) +
  scale_y_continuous(breaks = seq(0, 0.0009, by = 0.0003), labels = 0.1415 + seq(0, 0.0009, by = 0.0003)) +
  coord_flip() +
  theme_bw()
dev.off()
