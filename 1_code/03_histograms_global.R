#################################################################################################################################
### But: Visualiser les variables trajet une à une                                                                            ###
### Auteur: Francis Duval                                                                                                     ###
### Date: Juin 2019                                                                                                           ###
### Input: trajets.RDS                                                                                                        ###
### Output: histograms_global.pdf                                                                                             ###
#################################################################################################################################

source("1_code/00_source.R")


# Load trip dataset -------------------------------------------------------------------------------------------------------------
trajets <- loadRDS(here("2_pipeline", "01_output_trajets", "trajets.RDS"))


# Functions ---------------------------------------------------------------------------------------------------------------------

# Function to plot an histogram 
plot_hist <- function(x, xlabel = x, breaks = waiver(), limits = NULL, binwidth = 1) {
  ggplot(trajets, aes_string(x = x) ) +
    geom_histogram(color = "white", fill = "black", binwidth = binwidth) +
    scale_x_continuous(breaks = breaks, limits = limits) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(title = paste0("Histogram of ", x, " variable")) +
    xlab(xlabel) +
    ylab("Number of trips") +
    theme_bw()
}


# Histogram for continuous variables --------------------------------------------------------------------------------------------
all_hist <- list()

all_hist[[1]]  <- plot_hist("VSS_DISTANCE", xlabel = "Distance per trip", limits = c(0, 50))
all_hist[[2]]  <- plot_hist("VSS_AVG_SPEED", xlabel = "Average speed", limits = c(0, 110))
all_hist[[3]]  <- plot_hist("VSS_MAX_SPEED", xlabel = "Maximum speed", limits = c(0, 150))
all_hist[[4]]  <- plot_hist("IDLE_TIME", xlabel = "Idle time", limits = c(0, 500), binwidth = 5)
all_hist[[5]]  <- plot_hist("TRIP_POSITIONAL_QUALITY", xlabel = "Trip positional quality", limits = c(0, 2.5), binwidth = 0.1)
all_hist[[6]]  <- plot_hist("hour", xlabel = "Hour of the day", breaks = 0:23)
all_hist[[7]]  <- plot_hist("day", xlabel = "Day of the month")
all_hist[[8]]  <- plot_hist("year", xlabel = "Year", breaks = 2013:2019)
all_hist[[9]]  <- plot_hist("duration", xlabel = "Trip duration (in minutes)", limits = c(0, 100))
all_hist[[10]] <- plot_hist("hours_since_midnight", xlabel = "Hours since midnight", binwidth = 0.25)
all_hist[[11]] <- plot_hist("days_since_monday", xlabel = "Days since monday midnight", limits = c(0, 7), binwidth = 1 / 24)
all_hist[[12]] <- plot_hist("yday", xlabel = "Day of the year", binwidth = 2)


# Save plots---------------------------------------------------------------------------------------------------------------------
pdf(file = here("2_pipeline", "03_histograms_global", "histograms_global.pdf")) 
  walk(all_hist, print)
dev.off()
