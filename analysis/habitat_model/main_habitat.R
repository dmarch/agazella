#---------------------------------------------------------------
# main_habitat_species
#---------------------------------------------------------------
# This script processes data up to the species level.
# Community level is analysis in another script.


library(dplyr)
library(ggplot2)
library(stringr)

source("scr/fun_habitat_plot.R")
source("conf/config.R")

vars <- c("BAT", "SLP", "SDIST", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE")


cores <- 10

sp_code <- "GAZ"  # species code



# EDA
source("analysis/habitat_model/scr/eda.R")



# define model
mod_code <- "me"
  
# Fit model
# Try using the computer server for BRT
# export results by model (eg me, brt)
source("analysis/habitat_model/scr/fit_brt.R")

source("analysis/habitat_model/scr/fit_me.R")


#### Predict
# Set period
date_start <- as.Date("2019-02-20")  # change to 2012 in final version
date_end <- as.Date("2019-09-15")

  # define species
  sp_code <- "GAZ" 
  sp_name <- "A. gazella"
  
  # define model
  mod_code <- "me"
  
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



# JRC Models
spp_jrc <- filter(spp, model == "jrc", has_life_stage == "N")

for (i in 1:nrow(spp_jrc)){
  
  print(i)
  
  # define species
  sp_code <- spp_jrc$sp_code[i]
  sp_name <- spp_jrc$sp_name[i]
  
  # define subfolder
  predict_folder <- ifelse(spp_jrc$is_life_stage[i] == "N", "species", "lifestage")

  # define model
  mod_code <- "jrc"
  
  # define period
  years <- 2014

  source("analysis/habitat_model/scr/predict_jrc.R")

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



# Habitat importance per species --------
# For those with different stages (shearwaters)
# Calculate average habitat importance by selecting the stages that overlap on a given month/day.
# output will be in "DIO" folder
# When stages overlap, weight distribution maps according to the proportion of the population represented by each life‐history stage
date_start <- as.Date("2014-01-01")  # change to 2012 in final version
date_end <- as.Date("2014-12-31")
sp_code <- "DIO"
sp_name <- "Scopoli's shearwater"
predict_folder <- "lifestage"
tfreq <- "d"
source("analysis/habitat_model/scr/average_life_stages_DIO.R")

# Bluefin tuna
date_start <- as.Date("2014-01-01")  # change to 2012 in final version
date_end <- as.Date("2014-12-31")
sp_code <- "BFT"
sp_name <- "Bluefin tuna"
predict_folder <- "lifestage"
tfreq <- "m"
source("analysis/habitat_model/scr/average_life_stages_BFT.R")



# Average habitat importance on a monthly basis --------
# Allow us to compare with JRC models, AIS maps from Emodnet.
spp_daily <- filter(spp, tfreq=="daily", is_life_stage == "N")

# check that for shearwaters we use the model with accessibility.
for (i in 1:nrow(spp_daily)){
  
  print(i)

  # define species
  sp_code <- spp_daily $sp_code[i]
  sp_name <- spp_daily $sp_name[i]
  
  # define subfolder
  predict_folder <- "species"
  
  # time frequency
  tfreq <- ifelse(spp_daily $tfreq[i]=="daily", "d", "m")
  
  # Habitat importance
  source("analysis/habitat_model/scr/habitat_importance_monthly.R")
  
}





# Habitat importance of all the species
# select all files per date and calculate daily average
date_start <- as.Date("2018-01-01")  # change to 2012 in final version
date_end <- as.Date("2018-12-31")
source("analysis/habitat_model/scr/average_species.R")



# Areas Ecological Significance ()
# Assess change through the time
# Overlap with jurisdictional waters
# Overlap with current network of MPAs. See Claudet et al. 2020 for effective regulations.
# Overlap with human activities
# Introduce new concept of "Spatio-temporal coherent structures".
# Check that areas are not only connected in space but also in time. Or make monthly averages.
# build a network:
# - after 90 percentile.
# - a node is a combination of cell(i) and time(t).
# - Determine if has neighbours at same t, before and after.
# - create undirected unweighted network.
# - run community detection to find clusters in time and space.
# Example: https://rmets.onlinelibrary.wiley.com/doi/10.1002/joc.2280

source("analysis/habitat_model/scr/aes.R")


# Cumulative AES



# Model uncertainty
# See boostrap like Hindell et al. 2020


# Cumulative human impacts



# Network theory
# For each day, quantify the overlap between species (percentile 90 and index of overlap)
# Build network at different time scales (from daily to overall)
# Community algorithm
# https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.01942
# https://www.nature.com/articles/s41559-017-0457-3

