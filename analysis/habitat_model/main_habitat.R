#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script processes data up to the species level.
# Community level is analysis in another script.

source("setup.R")
source("scr/fun_habitat_plot.R")
#source("conf/config.R")

stack_repo <- paste0(output_data, "/stack_daily")
vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE")


cores <- 10

sp_code <- "GAZ"  # species code



# EDA
source("analysis/habitat_model/scr/eda.R")
vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "MLD", "EDGE")



# define model
mod_code <- "brt"
  
# Fit model
# Try using the computer server for BRT
# export results by model (eg me, brt)
source("analysis/habitat_model/scr/fit_brt.R")

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


# Accessibility model ----------
# only for central-place foragers (ie shearwaters)
# weigthed by colony size
# create model per life stage
# create distance to colony for each colony. Use individual maps to fit model.
# apply model to all colonies regardless they have telemetry data.
sp_codes <- spp$sp_code[spp$is_life_stage == "DIO"]
for (j in 1:length(sp_codes)){
  print(j)
  
  sp_code <- sp_codes[j]
  source("analysis/habitat_model/scr/accessibility.R")
}


# Recalculate habitat selection by accessibility ---------
# Multiply prediction per accessibility
spp_access <- filter(spp, accessibility == "Y")
for (i in 1:nrow(spp_access)){
  
  print(i)
  
  # define species
  sp_code <- spp_access$sp_code[i]
  sp_name <- spp_access$sp_name[i]
  
  # define model
  mod_code <- "brt"
  
  # Perdict model by accessibility
  mod_acc <- "unweighted"
  
  # define subfolder
  predict_folder <- "lifestage"
  
  source("analysis/habitat_model/scr/predict_accessibility.R")
  
}






# Habitat importance
# To define percentiles, we will consider all the period of study.
# Here it is important to limit the months for each species or stange in case it won't be present
# this part is new from Hindell.
# create a new folder, since Hi could be the ensemble of different models.
# check that for shearwaters we use the model with accessibility.
for (i in 1:nrow(spp)){
  
  print(i)
  
  # do not process species with life stages
  if(spp$has_life_stage[i] == "Y") next
  
  # define species
  sp_code <- spp$sp_code[i]
  sp_name <- spp$sp_name[i]
  
  # define model
  mod_code <- spp$model[i]
  if(spp$accessibility[i] == "Y") mod_code <- paste0(mod_code, "_acc")
  
  # define subfolder
  predict_folder <- ifelse(spp$is_life_stage[i] == "N", "species", "lifestage")
  
  # time frequency
  tfreq <- ifelse(spp$tfreq[i]=="daily", "d", "m")
  
  # Habitat importance
  source("analysis/habitat_model/scr/habitat_importance.R")
  
}







# Model uncertainty
# See boostrap like Hindell et al. 2020
