# Loader les packages -----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(here)
library(R.filesets)
library(DescTools)
library(magrittr)
library(forcats)
library(hms)
library(glue)
library(plotly)
library(FactoMineR)
library(factoextra)
library(viridis)
library(fs)
library(cluster)
library(dendextend)
library(pastecs)
library(haven)
library(broom)
library(kableExtra)
library(corrplot)
library(ggpmisc)
library(gridExtra)
library(ineq)
library(caret)
library(gbm)
library(catboost)

options(scipen = 999)

# Conflicts ---------------------------------------------------------------------------------------------------------------------
here <- here::here
filter <- dplyr::filter
train <- caret::train
