#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script processes data up to the species level.
# In this paper, they compare both MaxEnt and BRT: https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.13149
#
# 1. Exploratory Data Anlysis
# 2. Split training and testing dataset (70-30)
# 3. Fit MaxEnt
# 4. Fit BRT. Optimize - Variable section, Selection of parameters, Fit Full model
# 5. Cross-validation with testing dataset
# 6. Predictions, with bootstraps


source("setup.R")
source("scr/fun_habitat_plot.R")
#source("conf/config.R")

stack_repo <- paste0(output_data, "/stack_daily")
vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE")


cores <- 50

sp_code <- "GAZ"  # species code



# EDA
source("analysis/habitat_model/scr/eda.R")
# We remove SIT (keep SIC) and SAL (keep MLD)
vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "MLD", "EDGE")



# define model
mod_code <- "brt"
  
# Fit model
# Try using the computer server for BRT
# export results by model (eg me, brt)
source("analysis/habitat_model/scr/fit_brt_v2.R")

mod_code <- "me"
source("analysis/habitat_model/scr/fit_me.R")


#### Predict
# Set period
date_start <- as.Date("2019-02-20")  # change to 2012 in final version
date_end <- as.Date("2019-09-15")

  # define species
  sp_code <- "GAZ" 
  sp_name <- "A. gazella"
  
  # define model
  mod_code <- "brt"
  
  # define subfolder
  predict_folder <- "species"

  # Perdict model
  # export results by model (eg me, brt)
  source("analysis/habitat_model/scr/predict_sdm.R")

# Ensemble models


# Boosted Regression Trees


# Cross-validation
# If independent data is available, use it
# If not available, keep a testing dataset






