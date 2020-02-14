#################################################################################################################################
### But: Analyser les variables télématiques en lien avec les variables classiques                                            ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Décembre 2019                                                                                                       ###
### Input: drivers.RDS                                                                                                        ###
### Output:                                                                                                                   ###
#################################################################################################################################

source("1_code/00_source.R")


# Load datasets -----------------------------------------------------------------------------------------------------------------
drivers <- readRDS(here("2_pipeline", "21_output_drivers", "drivers.RDS"))
claims <- readRDS(here("2_pipeline", "14_output_claims", "claims_covariates.RDS"))

 
# Plot telematics statistics for all drivers ------------------------------------------------------------------------------------
plot_hist <- function(df, var) {
  xmax <- quantile(df[[var]], probs = 0.999)
  title = glue("Histogram of {var} for the {nrow(df)} drivers")
  hist(df[[var]], breaks = 100, xlim = c(0, xmax), xlab = var, main = title, border = "white", col = "blue")
}


# Plot each numeric telematic variable versus each categorical classic variable -------------------------------------------------
claims_drivers <- claims %>% left_join(drivers, by = "ENROLLED_VIN")

plot_box <- function(df, x, y) {
  ylim <- as.numeric(c(0, quantile(df[[y]], prob = 0.995)))
  ggplot(df, aes(x = !!ensym(x), y = !!ensym(y), fill = !!ensym(x))) +
    geom_boxplot(color = "black", alpha = 0.5) +
    scale_fill_viridis_d(guide = F) +
    ylim(ylim) +
    labs(title = glue("Based on {nrow(df)} drivers")) +
    theme_bw()
}

x_vars <- claims_drivers %>% select_if(is.factor) %>% names()
y_vars <- drivers %>% select(annual_distance_km:avg_speed) %>% names()
xy <- expand.grid(x = x_vars, y = y_vars, stringsAsFactors = F)

tele_vs_classic <- map2(xy$x, xy$y, plot_box, df = claims_drivers)


# Save all plots ----------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "22_telematics_vs_classic", "histograms_telematics.pdf"))
map(names(drivers[-1]), plot_hist, df = drivers)
dev.off()

pdf(file = here("2_pipeline", "22_telematics_vs_classic", "telematics_vs_classic.pdf"))
tele_vs_classic
dev.off()

