#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script processes data up to the species level.
# In this paper, they compare both MaxEnt and BRT: https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.13149
#
# 1. Exploratory Data Anlysis
# 2. Split training and testing dataset
# 3. Fit MaxEnt
# 4. Fit BRT. Optimize - Variable section, Selection of parameters, Fit Full model
# 5. Cross-validation with testing dataset
# 6. Predictions, with bootstraps



## Load dependencies
source("setup.R")
source("scr/fun_habitat_plot.R")


## Set initial parameters
sp_code <- "GAZ"  # species code
sp_name <- "A. gazella"
stack_repo <- paste0(output_data, "/stack_daily")  # location of environmental data
vars <- c("BAT", "SLP", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "MLD", "EDGE")  # list of all predictors
# after preliminary exploration, we remove SDIST (distance to coast - high correlated with bat, and can give problems with EDGE) and SIT(sea icea thickness - high correlated with SIC)

## computer cores
cores <- 50  # numbers of cores



#-----------------------------------------------------------
# 2. Split training and testing dataset
#-----------------------------------------------------------
# Training and testing
# we cross‐validated the models by running a training model by randomly choosing 75% of the entire dataset,
# then comparing model predictions against the remaining 25% of the data, while maintaining the same ratio of presences
# to pseudo‐absences. (Maxwell 2019)
#
# Current ration of presence absence is 1:30

train_prop <- 0.75
source("analysis/habitat_model/scr/training_testing.R")



#-----------------------------------------------------------
# 1. Exploratory Data Analysis
#-----------------------------------------------------------
# Reads all presence/absence data and generates several plots to explore all variables
# - Checks for missing data
# - Pearson and Spearman rank correlations
# - Density plots to compare presence/absence
source("analysis/habitat_model/scr/eda.R")

# After visual inspection, keep and exclude variables
# We remove SIT (keep SIC) and SAL (keep MLD)
# vars2remove <- c("SIT")#,  # we keep SIC
#                  #"SAL",  # we keep MLD
#                  #"SDIST") # keep BAT
# 
# vars <- vars[!vars %in% vars2remove]


#-----------------------------------------------------------
# 3. MaxEnt (cannot be run in the server as MaxEnt not installed as external software)
#-----------------------------------------------------------

# 3.1. Fit MaxEnt

mod_code <- "me"
source("analysis/habitat_model/scr/fit_me.R")


# 3.2. Cross-validate


# 3.3. Predict



#-----------------------------------------------------------
# 4. BRT
#-----------------------------------------------------------


# Fit model
source("analysis/habitat_model/scr/fit_brt_v3.R")


# Predict
# Set period
date_start <- as.Date("2019-02-01")  # change to 2012 in final version
date_end <- as.Date("2019-02-19")

date_start <- as.Date("2019-09-16")  # change to 2012 in final version
date_end <- as.Date("2019-09-30")

date_start <- as.Date("2019-02-20")  # change to 2012 in final version
date_end <- as.Date("2019-09-15")
bootstrap <- F

  
  # define subfolder
  predict_folder <- "species"

  # Perdict model
  # export results by model (eg me, brt)
  source("analysis/habitat_model/scr/predict_brt.R")
  source("analysis/habitat_model/scr/predict_brt_boot.R") # for bootstrap




