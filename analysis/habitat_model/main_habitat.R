#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script controls the workflow used for habitat modeling


#-----------------------------------------------------------
# 1. Set parameters
#-----------------------------------------------------------

## Load dependencies
source("setup.R")
source("scr/fun_habitat_plot.R")

## Set initial parameters
sp_code <- "GAZ"  # species code
sp_name <- "A. gazella"
stack_repo <- paste0(output_data, "/stack_daily")  # location of environmental data
vars <- c("BAT", "SLP", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "MLD", "EDGE", "SIT")  # list of all predictors

## computer cores
cores <- 50  # numbers of cores


#-----------------------------------------------------------
# 2. Split training and testing dataset
#-----------------------------------------------------------
# Training and testing
# This step was originaly designed to split datasets for cross-validation.
# Final version of the analysis uses a leave-one-out approach and this part is
# is no longer required. I keep the code as it does not affect and can be useful
# for other works.
train_prop <- 0.75
source("analysis/habitat_model/scr/training_testing.R")


#-----------------------------------------------------------
# 3. Exploratory Data Analysis
#-----------------------------------------------------------
# Reads all presence/absence data and generates several plots to explore all variables
# - Checks for missing data
# - Pearson and Spearman rank correlations
# - Density plots to compare presence/absence
source("analysis/habitat_model/scr/eda.R")

# After visual inspection, keep and exclude variables
vars2remove <- c("SIT")  # We remove SIT (keep SIC) 
vars <- vars[!vars %in% vars2remove]


#-----------------------------------------------------------
# 4. Fit BRT
#-----------------------------------------------------------
# Fit model
source("analysis/habitat_model/scr/fit_brt.R")



#-----------------------------------------------------------
# 5. Fit Accessibility model
#-----------------------------------------------------------
# Fit model
source("analysis/habitat_model/scr/accessibility_ice.R")


#-----------------------------------------------------------
# 6. Predict combining BRT and accessbility model
#-----------------------------------------------------------
# Set prediction period
date_start <- as.Date("2019-02-01")
date_end <- as.Date("2019-09-30")

# Select if use bootrap models or full model
bootstrap <- TRUE

# Predict
if(bootstrap == FALSE) source("analysis/habitat_model/scr/predict_brt.R")
if(bootstrap == TRUE) source("analysis/habitat_model/scr/predict_brt_boot.R")




